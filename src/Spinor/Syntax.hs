{-# LANGUAGE OverloadedStrings #-}

module Spinor.Syntax
  ( Expr(..)
  , readExpr
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

-- シンボル: 英字・記号で始まり、英数字・記号が続く
pSym :: Parser Expr
pSym = lexeme $ do
  s <- some (satisfy isSymChar)
  pure $ ESym (T.pack s)
  where
    isSymChar c = c `notElem` (" \t\n\r();#" :: String)

-- リスト: ( expr* )
pList :: Parser Expr
pList = EList <$> between (lexeme (char '(')) (char ')') (many parseExpr)

-- メインパーサー
parseExpr :: Parser Expr
parseExpr = sc *> (pList <|> pBool <|> try pInt <|> pSym)

-- パース実行ヘルパー
readExpr :: Text -> Either String Expr
readExpr input = case parse (sc *> parseExpr <* eof) "<stdin>" input of
  Left  err -> Left (errorBundlePretty err)
  Right ast -> Right ast
