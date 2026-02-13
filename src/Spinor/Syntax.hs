{-# LANGUAGE OverloadedStrings #-}

module Spinor.Syntax
  ( Expr(..)
  , readExpr
  , parseFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Spinor の抽象構文木
--   Lisp の S式 を Haskell の代数的データ型にマッピングする。
--     シンボル → Text, リスト → [Expr]
data Expr
  = EInt  Integer
  | EBool Bool
  | ESym  Text
  | EStr  Text
  | EList [Expr]
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
    isSymChar c = c `notElem` (" \t\n\r();#" :: String)

-- リスト: ( expr* )
pList :: Parser Expr
pList = EList <$> between (lexeme (char '(')) (lexeme (char ')')) (many parseExpr)

-- メインパーサー
parseExpr :: Parser Expr
parseExpr = sc *> (pList <|> pBool <|> pStr <|> try pInt <|> pSym)

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
