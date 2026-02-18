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
import Data.List (partition)
import Data.Char (isAlphaNum)
import Spinor.Syntax (Expr(..))

-- | C コードの型エイリアス
type CCode = Text

-- | プログラム全体を C 言語のソースコードに変換する
--
-- defun 式は C のトップレベル関数に変換され、
-- その他のトップレベル式は main 関数内で評価・表示される。
compileProgram :: [Expr] -> CCode
compileProgram exprs =
    let (defuns, others) = partition isDefun exprs
        funDefs = T.unlines (map compileFunDef defuns)
        mainStmts = T.unlines (map compileStmt others)
    in T.unlines
        [ "#include <stdio.h>"
        , "#include <stdbool.h>"
        , "#include \"spinor.h\""
        , ""
        , funDefs
        , "int main(void) {"
        , mainStmts
        , "    return 0;"
        , "}"
        ]

-- | defun 式かどうかを判定する
isDefun :: Expr -> Bool
isDefun (EList (ESym "defun" : _)) = True
isDefun _ = False

-- | defun 式を C の関数定義に変換する
compileFunDef :: Expr -> CCode
compileFunDef (EList [ESym "defun", ESym name, EList args, body]) =
    let cName = mangle name
        cArgs = T.intercalate ", " (map toCArg args)
        cBody = compileExpr body
    in T.unlines
        [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
        , "    return " <> cBody <> ";"
        , "}"
        ]
  where
    toCArg (ESym argName) = "SpObject* " <> mangle argName
    toCArg _ = "SpObject* _unknown"
compileFunDef _ = "/* invalid defun */"

-- | Spinor シンボルを安全な C 識別子に変換する (名前マングリング)
--
-- ユーザー定義関数には user_ プレフィックスを付与し、
-- C の識別子として無効な文字は _ に置換する。
mangle :: Text -> CCode
mangle name = "user_" <> T.map sanitize name
  where
    sanitize c | isAlphaNum c = c
               | otherwise    = '_'

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

-- 変数参照 (引数など)
compileExpr (ESym s) = mangle s

-- ユーザー定義関数の呼び出し (プリミティブでない関数)
compileExpr (EList (ESym fname : args))
    | fname `notElem` primitives =
        let cFun = mangle fname
            cArgs = T.intercalate ", " (map compileExpr args)
        in cFun <> "(" <> cArgs <> ")"
  where
    primitives = ["+", "-", "*", "/", "=", "<", ">", "if", "defun"]

-- 未実装のパターン
compileExpr other = "sp_make_nil() /* TODO: " <> T.pack (show other) <> " */"
