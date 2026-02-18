{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Spinor.Compiler.Codegen
Description : Spinor AST から C99 コードを生成するトランスパイラ

Spinor のサブセットを C 言語に変換する。生成されたコードは
runtime/spinor.h および runtime/spinor.c と組み合わせて使用する。
-}
module Spinor.Compiler.Codegen
  ( compileProgram
  , compileExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Spinor.Syntax (Expr(..))

-- | C コードの型エイリアス
type CCode = Text

-- | プログラム全体を C 言語のソースコードに変換する
--
-- 生成されるコードは main 関数を含み、各トップレベル式の結果を
-- sp_print で標準出力に表示する。
compileProgram :: [Expr] -> CCode
compileProgram exprs = T.unlines
    [ "#include <stdio.h>"
    , "#include <stdbool.h>"
    , "#include \"spinor.h\""
    , ""
    , "int main(void) {"
    , T.unlines (map compileStmt exprs)
    , "    return 0;"
    , "}"
    ]

-- | トップレベル式を C のステートメントに変換
--
-- 式を評価し、その結果を sp_print で表示する。
compileStmt :: Expr -> CCode
compileStmt expr =
    let valCode = compileExpr expr
    in "    sp_print(" <> valCode <> ");"

-- | 式を C の式 (SpObject* を返すコード) に変換する
--
-- 各 Spinor 式は SpObject* を返す C の式に変換される。
compileExpr :: Expr -> CCode
-- リテラル
compileExpr (EInt n)  = "sp_make_int(" <> T.pack (show n) <> ")"
compileExpr (EBool b) = "sp_make_bool(" <> (if b then "true" else "false") <> ")"
compileExpr (EStr _)  = "sp_make_nil() /* TODO: string */"

-- if 式: (if cond then else)
compileExpr (EList [ESym "if", cond, thenE, elseE]) =
    "(" <> compileExpr cond <> "->value.boolean ? " <>
    compileExpr thenE <> " : " <> compileExpr elseE <> ")"

-- 算術演算
compileExpr (EList [ESym "+", a, b]) =
    "sp_add(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym "-", a, b]) =
    "sp_sub(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym "*", a, b]) =
    "sp_mul(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym "/", a, b]) =
    "sp_div(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- 比較演算
compileExpr (EList [ESym "=", a, b]) =
    "sp_eq(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym "<", a, b]) =
    "sp_lt(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym ">", a, b]) =
    "sp_gt(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- 未実装のパターン
compileExpr other = "sp_make_nil() /* TODO: " <> T.pack (show other) <> " */"
