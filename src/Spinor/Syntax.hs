{-# LANGUAGE OverloadedStrings #-}

module Spinor.Syntax
  ( Expr(..)
  , Pattern(..)
  , TypeExpr(..)
  , ConstructorDef(..)
  , ImportOption(..)
  , readExpr
  , parseFile
  ) where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | 型式 (コンストラクタ引数の型記述用)
--   TEVar "a"          — 型変数
--   TEApp "MyList" [TEVar "a"] — 型適用 (MyList a)
data TypeExpr
  = TEVar Text
  | TEApp Text [TypeExpr]
  deriving (Show, Eq)

-- | コンストラクタ定義: コンストラクタ名と引数の型式リスト
data ConstructorDef = ConstructorDef Text [TypeExpr]
  deriving (Show, Eq)

-- | インポートオプション
data ImportOption
  = Only [Text]     -- ^ (only sym1 sym2 ...): 指定したシンボルのみをインポート
  | Except [Text]   -- ^ (except sym1 sym2 ...): 指定したシンボルを除いてインポート
  | Prefix Text     -- ^ (prefix pfx-): 全シンボルにプレフィックスを付加
  | Alias Text      -- ^ (alias M): モジュールに別名を与える (M:sym でアクセス)
  deriving (Show, Eq)

-- | パターン (match 式用)
data Pattern
  = PVar  Text             -- 変数パターン: x (任意の値にマッチし束縛)
  | PCon  Text [Pattern]   -- コンストラクタパターン: (Just x), (Cons x xs)
  | PLit  Expr             -- リテラルパターン: 1, #t, "hello"
  | PWild                  -- ワイルドカード: _
  deriving (Show, Eq)

-- | Spinor の抽象構文木
--   Lisp の S式 を Haskell の代数的データ型にマッピングする。
--     シンボル → Text, リスト → [Expr]
data Expr
  = EInt  Integer
  | EBool Bool
  | ESym  Text
  | EStr  Text
  | EList [Expr]
  | ELet  Text Expr Expr            -- ^ (let name val body)
  | EData Text [ConstructorDef]     -- ^ (data Name (Con1 a b) (Con2))
  | EMatch Expr [(Pattern, Expr)]   -- ^ (match target (pat1 body1) (pat2 body2) ...)
  | EModule Text [Text]             -- ^ (module name (export sym1 sym2 ...))
  | EImport Text [ImportOption]     -- ^ (import module-name options...)
  deriving (Show, Eq)

type Parser = Parsec Void Text

-- Space Consumer: 空白 + セミコロンコメントをスキップ
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- 整数リテラル (負数対応)
pInt :: Parser Expr
pInt = EInt <$> lexeme (L.signed (pure ()) L.decimal)

-- 真偽値リテラル: #t / #f
pBool :: Parser Expr
pBool = lexeme $ do
  _ <- char '#'
  EBool True  <$ char 't'
    <|> EBool False <$ char 'f'

-- 文字列リテラル: "..."
pStr :: Parser Expr
pStr = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  pure $ EStr (T.pack s)

-- シンボル: 英字・記号で始まり、英数字・記号が続く
pSym :: Parser Expr
pSym = lexeme $ do
  s <- some (satisfy isSymChar)
  pure $ ESym (T.pack s)
  where
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- Quote 略記: 'expr → (quote expr)
pQuote :: Parser Expr
pQuote = do
  _ <- char '\''
  expr <- parseExpr
  pure $ EList [ESym "quote", expr]

-- TypeExpr パーサー: コンストラクタ引数の型記述
--   シンボル単体 → TEVar "a"
--   (Name args...) → TEApp "Name" [TypeExpr...]
pTypeExpr :: Parser TypeExpr
pTypeExpr = pTypeApp <|> pTypeVar
  where
    pTypeVar = lexeme $ do
      s <- some (satisfy isSymChar)
      pure $ TEVar (T.pack s)
    pTypeApp = between (lexeme (char '(')) (lexeme (char ')')) $ do
      TEVar name <- pTypeVar
      args <- many pTypeExpr
      pure $ TEApp name args
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- コンストラクタ定義パーサー: (ConName arg1 arg2 ...) または (ConName)
pConstructorDef :: Parser ConstructorDef
pConstructorDef = between (lexeme (char '(')) (lexeme (char ')')) $ do
  name <- lexeme $ do
    s <- some (satisfy isSymChar)
    pure (T.pack s)
  args <- many pTypeExpr
  pure $ ConstructorDef name args
  where
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- リスト: ( expr* )
--   (let name val body) は ELet に変換する
--   (data Name (Con1 a b) (Con2)) は EData に変換する
--   (module ...) と (import ...) は特殊形式として処理する
pList :: Parser Expr
pList = do
  xs <- between (lexeme (char '(')) (lexeme (char ')')) (many parseExpr)
  case xs of
    [ESym "let", ESym name, val, body] -> pure $ ELet name val body
    (ESym "module" : rest) -> parseModuleForm rest
    (ESym "import" : rest) -> parseImportForm rest
    _ -> pure $ EList xs

-- module 式パーサー: (module name (export sym1 sym2 ...))
parseModuleForm :: [Expr] -> Parser Expr
parseModuleForm [ESym name, EList (ESym "export" : exports)] = do
  exportNames <- mapM extractSymName exports
  pure $ EModule name exportNames
parseModuleForm [ESym name] =
  -- export を省略した場合は何も公開しない
  pure $ EModule name []
parseModuleForm _ = fail "module: 不正な構文です。(module name (export sym ...)) の形式が必要です"

-- import 式パーサー: (import module-name options...)
parseImportForm :: [Expr] -> Parser Expr
parseImportForm (modSpec : optExprs) = do
  modName <- extractModuleName modSpec
  opts <- mapM parseImportOption optExprs
  pure $ EImport modName opts
parseImportForm [] = fail "import: モジュール名が必要です"

-- モジュール名を抽出 (シンボルまたは文字列)
extractModuleName :: Expr -> Parser Text
extractModuleName (ESym s)  = pure s
extractModuleName (EStr s)  = pure s
extractModuleName (EList [ESym "quote", ESym s]) = pure s
extractModuleName _ = fail "import: モジュール名はシンボルまたは文字列で指定してください"

-- シンボル名を抽出
extractSymName :: Expr -> Parser Text
extractSymName (ESym s) = pure s
extractSymName _ = fail "export: シンボル名が必要です"

-- インポートオプションをパース
parseImportOption :: Expr -> Parser ImportOption
parseImportOption (EList (ESym "only" : syms)) = do
  names <- mapM extractSymName syms
  pure $ Only names
parseImportOption (EList (ESym "except" : syms)) = do
  names <- mapM extractSymName syms
  pure $ Except names
parseImportOption (EList [ESym "prefix", ESym pfx]) =
  pure $ Prefix pfx
parseImportOption (EList [ESym "alias", ESym alias]) =
  pure $ Alias alias
parseImportOption _ = fail "import: 不正なインポートオプションです"

-- data 式パーサー: (data TypeName (Con1 args...) (Con2 args...) ...)
pData :: Parser Expr
pData = between (lexeme (char '(')) (lexeme (char ')')) $ do
  _ <- lexeme (chunk "data")
  typeName <- lexeme $ do
    s <- some (satisfy isSymChar)
    pure (T.pack s)
  constrs <- many pConstructorDef
  pure $ EData typeName constrs
  where
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- match 式パーサー: (match target (pat1 body1) (pat2 body2) ...)
pMatch :: Parser Expr
pMatch = between (lexeme (char '(')) (lexeme (char ')')) $ do
  _ <- lexeme (chunk "match")
  target <- parseExpr
  branches <- many pBranch
  pure $ EMatch target branches
  where
    pBranch = between (lexeme (char '(')) (lexeme (char ')')) $ do
      pat  <- pPattern
      body <- parseExpr
      pure (pat, body)

-- パターンパーサー
pPattern :: Parser Pattern
pPattern = sc *> (pPatWild <|> pPatCon <|> pPatBool <|> pPatStr <|> try pPatInt <|> pPatSym)
  where
    -- ワイルドカード: _
    pPatWild = PWild <$ lexeme (chunk "_" <* notFollowedBy (satisfy isSymChar))
    -- コンストラクタパターン: (ConName pat...)
    pPatCon = between (lexeme (char '(')) (lexeme (char ')')) $ do
      name <- lexeme $ do
        s <- some (satisfy isSymChar)
        pure (T.pack s)
      pats <- many pPattern
      pure $ PCon name pats
    -- 真偽値リテラルパターン
    pPatBool = lexeme $ do
      _ <- char '#'
      PLit . EBool <$> (True <$ char 't' <|> False <$ char 'f')
    -- 文字列リテラルパターン
    pPatStr = lexeme $ do
      _ <- char '"'
      s <- manyTill L.charLiteral (char '"')
      pure $ PLit (EStr (T.pack s))
    -- 整数リテラルパターン
    pPatInt = PLit . EInt <$> lexeme (L.signed (pure ()) L.decimal)
    -- シンボル: 大文字始まりなら0引数コンストラクタ、それ以外は変数パターン
    pPatSym = lexeme $ do
      s <- some (satisfy isSymChar)
      let name = T.pack s
      pure $ if not (T.null name) && isUpper (T.head name)
             then PCon name []
             else PVar name
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- メインパーサー
parseExpr :: Parser Expr
parseExpr = sc *> (try pData <|> try pMatch <|> pList <|> pQuote <|> pBool <|> pStr <|> try pInt <|> pSym)

-- パース実行ヘルパー (単一式)
readExpr :: Text -> Either String Expr
readExpr input = case parse (sc *> parseExpr <* eof) "<stdin>" input of
  Left  err -> Left (errorBundlePretty err)
  Right ast -> Right ast

-- ファイル全体のパース (複数式)
parseFile :: Text -> Either String [Expr]
parseFile input = case parse (sc *> many parseExpr <* eof) "<file>" input of
  Left  err  -> Left (errorBundlePretty err)
  Right asts -> Right asts
