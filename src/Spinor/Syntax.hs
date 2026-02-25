{-# LANGUAGE OverloadedStrings #-}

module Spinor.Syntax
  ( Expr(..)
  , Pattern(..)
  , TypeExpr(..)
  , ConstructorDef(..)
  , ImportOption(..)
  , SourcePos(..)
  , SourceSpan(..)
  , dummySpan
  , exprSpan
  , readExpr
  , parseFile
  , SpinorParseError(..)
  , parseFileWithErrors
  ) where

import Data.Char (isUpper)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec hiding (SourcePos)
import qualified Text.Megaparsec as MP
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

-- | ソースコード上の位置 (ファイル名、行、列)
data SourcePos = SourcePos
  { posFile   :: FilePath
  , posLine   :: Int
  , posColumn :: Int
  } deriving (Show, Eq)

-- | ソースコード上の範囲 (開始位置〜終了位置)
data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Show, Eq)

-- | ダミーの SourceSpan (テスト用・内部生成コード用)
dummySpan :: SourceSpan
dummySpan = SourceSpan pos pos
  where pos = SourcePos "<internal>" 0 0

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
--   各コンストラクタの第1引数は SourceSpan (ソースコード上の位置情報)
data Expr
  = EInt    SourceSpan Integer
  | EBool   SourceSpan Bool
  | ESym    SourceSpan Text
  | EStr    SourceSpan Text
  | EList   SourceSpan [Expr]
  | ELet    SourceSpan [(Text, Expr)] Expr       -- ^ (let ((var1 val1) ...) body)
  | EData   SourceSpan Text [ConstructorDef]     -- ^ (data Name (Con1 a b) (Con2))
  | EMatch  SourceSpan Expr [(Pattern, Expr)]    -- ^ (match target (pat1 body1) (pat2 body2) ...)
  | EModule SourceSpan Text [Text]               -- ^ (module name (export sym1 sym2 ...))
  | EImport SourceSpan Text [ImportOption]       -- ^ (import module-name options...)
  deriving (Show, Eq)

-- | 式から SourceSpan を取得
exprSpan :: Expr -> SourceSpan
exprSpan (EInt    sp _)     = sp
exprSpan (EBool   sp _)     = sp
exprSpan (ESym    sp _)     = sp
exprSpan (EStr    sp _)     = sp
exprSpan (EList   sp _)     = sp
exprSpan (ELet    sp _ _)   = sp
exprSpan (EData   sp _ _)   = sp
exprSpan (EMatch  sp _ _)   = sp
exprSpan (EModule sp _ _)   = sp
exprSpan (EImport sp _ _)   = sp

type Parser = Parsec Void Text

-- Space Consumer: 空白 + セミコロンコメントをスキップ
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Megaparsec の SourcePos から Spinor の SourcePos に変換
toSourcePos :: MP.SourcePos -> SourcePos
toSourcePos pos = SourcePos
  { posFile   = sourceName pos
  , posLine   = unPos (sourceLine pos)
  , posColumn = unPos (sourceColumn pos)
  }

-- | パーサーをラップして SourceSpan を取得
withSpan :: Parser (SourceSpan -> a) -> Parser a
withSpan p = do
  startPos <- toSourcePos <$> getSourcePos
  f <- p
  endPos <- toSourcePos <$> getSourcePos
  pure $ f (SourceSpan startPos endPos)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- 整数リテラル (負数対応)
pInt :: Parser Expr
pInt = withSpan $ do
  n <- lexeme (L.signed (pure ()) L.decimal)
  pure $ \sp -> EInt sp n

-- 真偽値リテラル: #t / #f
pBool :: Parser Expr
pBool = withSpan $ lexeme $ do
  _ <- char '#'
  b <- True <$ char 't' <|> False <$ char 'f'
  pure $ \sp -> EBool sp b

-- 文字列リテラル: "..."
pStr :: Parser Expr
pStr = withSpan $ lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  pure $ \sp -> EStr sp (T.pack s)

-- シンボル: 英字・記号で始まり、英数字・記号が続く
pSym :: Parser Expr
pSym = withSpan $ lexeme $ do
  s <- some (satisfy isSymChar)
  pure $ \sp -> ESym sp (T.pack s)
  where
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- Quote 略記: 'expr → (quote expr)
pQuote :: Parser Expr
pQuote = withSpan $ do
  _ <- char '\''
  expr <- parseExpr
  pure $ \sp -> EList sp [ESym sp "quote", expr]

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
pList = withSpan $ do
  xs <- between (lexeme (char '(')) (lexeme (char ')')) (many parseExpr)
  case xs of
    -- 新形式: (let ((var1 val1) (var2 val2) ...) body)
    [ESym _ "let", EList _ bindingsExprs, body] ->
      case parseLetBindings bindingsExprs of
        Just bindings -> pure $ \sp -> ELet sp bindings body
        Nothing       -> fail "let: 不正な束縛形式です。((var1 val1) (var2 val2) ...) の形式が必要です"
    -- 旧形式: (let var val body) をサポート (後方互換)
    [ESym _ "let", ESym _ name, val, body] -> pure $ \sp -> ELet sp [(name, val)] body
    (ESym _ "module" : rest) -> parseModuleForm rest
    (ESym _ "import" : rest) -> parseImportForm rest
    _ -> pure $ \sp -> EList sp xs

-- | let 式の束縛リストをパースする
--   ((var1 val1) (var2 val2) ...) → [(var1, val1), (var2, val2), ...]
parseLetBindings :: [Expr] -> Maybe [(Text, Expr)]
parseLetBindings = mapM parseBinding
  where
    parseBinding (EList _ [ESym _ var, valExpr]) = Just (var, valExpr)
    parseBinding _                                = Nothing

-- module 式パーサー: (module name (export sym1 sym2 ...))
parseModuleForm :: [Expr] -> Parser (SourceSpan -> Expr)
parseModuleForm [ESym _ name, EList _ (ESym _ "export" : exports)] = do
  exportNames <- mapM extractSymName exports
  pure $ \sp -> EModule sp name exportNames
parseModuleForm [ESym _ name] =
  -- export を省略した場合は何も公開しない
  pure $ \sp -> EModule sp name []
parseModuleForm _ = fail "module: 不正な構文です。(module name (export sym ...)) の形式が必要です"

-- import 式パーサー: (import module-name options...)
parseImportForm :: [Expr] -> Parser (SourceSpan -> Expr)
parseImportForm (modSpec : optExprs) = do
  modName <- extractModuleName modSpec
  opts <- mapM parseImportOption optExprs
  pure $ \sp -> EImport sp modName opts
parseImportForm [] = fail "import: モジュール名が必要です"

-- モジュール名を抽出 (シンボルまたは文字列)
extractModuleName :: Expr -> Parser Text
extractModuleName (ESym _ s)  = pure s
extractModuleName (EStr _ s)  = pure s
extractModuleName (EList _ [ESym _ "quote", ESym _ s]) = pure s
extractModuleName _ = fail "import: モジュール名はシンボルまたは文字列で指定してください"

-- シンボル名を抽出
extractSymName :: Expr -> Parser Text
extractSymName (ESym _ s) = pure s
extractSymName _ = fail "export: シンボル名が必要です"

-- インポートオプションをパース
parseImportOption :: Expr -> Parser ImportOption
parseImportOption (EList _ (ESym _ "only" : syms)) = do
  names <- mapM extractSymName syms
  pure $ Only names
parseImportOption (EList _ (ESym _ "except" : syms)) = do
  names <- mapM extractSymName syms
  pure $ Except names
parseImportOption (EList _ [ESym _ "prefix", ESym _ pfx]) =
  pure $ Prefix pfx
parseImportOption (EList _ [ESym _ "alias", ESym _ alias]) =
  pure $ Alias alias
parseImportOption _ = fail "import: 不正なインポートオプションです"

-- data 式パーサー: (data TypeName (Con1 args...) (Con2 args...) ...)
pData :: Parser Expr
pData = withSpan $ between (lexeme (char '(')) (lexeme (char ')')) $ do
  _ <- lexeme (chunk "data")
  typeName <- lexeme $ do
    s <- some (satisfy isSymChar)
    pure (T.pack s)
  constrs <- many pConstructorDef
  pure $ \sp -> EData sp typeName constrs
  where
    isSymChar c = c `notElem` (" \t\n\r();#'\"" :: String)

-- match 式パーサー: (match target (pat1 body1) (pat2 body2) ...)
pMatch :: Parser Expr
pMatch = withSpan $ between (lexeme (char '(')) (lexeme (char ')')) $ do
  _ <- lexeme (chunk "match")
  target <- parseExpr
  branches <- many pBranch
  pure $ \sp -> EMatch sp target branches
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
      b <- True <$ char 't' <|> False <$ char 'f'
      pure $ PLit (EBool dummySpan b)
    -- 文字列リテラルパターン
    pPatStr = lexeme $ do
      _ <- char '"'
      s <- manyTill L.charLiteral (char '"')
      pure $ PLit (EStr dummySpan (T.pack s))
    -- 整数リテラルパターン
    pPatInt = do
      n <- lexeme (L.signed (pure ()) L.decimal)
      pure $ PLit (EInt dummySpan n)
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

-- | パースエラー (位置情報付き)
data SpinorParseError = SpinorParseError
  { errorLine    :: Int    -- ^ 1-indexed 行番号
  , errorColumn  :: Int    -- ^ 1-indexed 列番号
  , errorMessage :: Text   -- ^ エラーメッセージ
  } deriving (Show, Eq)

-- | ファイル全体のパース (位置情報付きエラー)
--   LSP サーバーなどで使用するため、エラー発生位置を正確に返す
parseFileWithErrors :: Text -> Either [SpinorParseError] [Expr]
parseFileWithErrors input = case parse (sc *> many parseExpr <* eof) "<file>" input of
  Left err -> Left (extractErrors err)
  Right asts -> Right asts
  where
    extractErrors :: ParseErrorBundle Text Void -> [SpinorParseError]
    extractErrors bundle =
      let errs = bundleErrors bundle
          posState = bundlePosState bundle
      in map (toSpinorParseError posState) (toList errs)

    toSpinorParseError :: PosState Text -> ParseError Text Void -> SpinorParseError
    toSpinorParseError posState err =
      let offset = errorOffset err
          (_, newPosState) = reachOffset offset posState
          MP.SourcePos _ line col = pstateSourcePos newPosState
      in SpinorParseError
           { errorLine    = unPos line
           , errorColumn  = unPos col
           , errorMessage = T.pack (parseErrorTextPretty err)
           }

    toList (e :| es) = e : es
