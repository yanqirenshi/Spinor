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
--
-- 末尾自己再帰が検出された場合は TCO (while(1) + continue) を適用する。
compileFunDef :: Expr -> CCode
compileFunDef (EList [ESym "defun", ESym name, EList argExprs, body]) =
    let cName = mangle name
        cArgs = T.intercalate ", " (map toCArg argExprs)
        paramNames = [n | ESym n <- argExprs]
    in if hasTailSelfCall name body
       then -- TCO 適用: while(1) ループで末尾再帰を最適化
            T.unlines
              [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
              , "    while(1) {"
              , compileTailBody name paramNames body
              , "    }"
              , "}"
              ]
       else -- TCO 非適用: 従来通り return
            T.unlines
              [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
              , "    return " <> compileExpr body <> ";"
              , "}"
              ]
  where
    toCArg (ESym argName) = "SpObject* " <> mangle argName
    toCArg _ = "SpObject* _unknown"
compileFunDef _ = "/* invalid defun */"

-- | 関数本体の末尾位置に自己再帰呼び出しがあるかを判定する
--
-- if 式の場合は両分岐を検査する。
hasTailSelfCall :: Text -> Expr -> Bool
hasTailSelfCall fname (EList [ESym "if", _, thenE, elseE]) =
    hasTailSelfCall fname thenE || hasTailSelfCall fname elseE
hasTailSelfCall fname (EList (ESym f : _)) = f == fname
hasTailSelfCall _ _ = False

-- | 末尾位置の式を TCO 対応の C コード (文) に変換する
--
-- - if 式: C の if/else 文に変換し、各分岐を再帰的に処理
-- - 自己再帰呼び出し: 一時変数で引数評価 → パラメータ更新 → continue
-- - その他: return 文を生成
compileTailBody :: Text -> [Text] -> Expr -> CCode
-- if 式: 分岐を C の if/else 文に変換
compileTailBody fname params (EList [ESym "if", cond, thenE, elseE]) =
    T.unlines
      [ "        if (" <> compileExpr cond <> "->value.boolean) {"
      , compileTailBody fname params thenE
      , "        } else {"
      , compileTailBody fname params elseE
      , "        }"
      ]
-- 末尾自己再帰呼び出し: 引数を一時変数に退避してから更新 + continue
compileTailBody fname params (EList (ESym f : args))
    | f == fname =
        let compiledArgs = map compileExpr args
            indexedArgs = zip [0::Int ..] compiledArgs
            indexedParams = zip [0::Int ..] params
            tmpDecls = T.unlines
              [ "            SpObject* _tco_tmp_" <> T.pack (show i) <> " = " <> a <> ";"
              | (i, a) <- indexedArgs ]
            assigns = T.unlines
              [ "            " <> mangle p <> " = _tco_tmp_" <> T.pack (show i) <> ";"
              | (i, p) <- indexedParams ]
        in tmpDecls <> assigns <> "            continue;"
-- その他の式: return で値を返す
compileTailBody _ _ expr =
    "            return " <> compileExpr expr <> ";"

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
compileExpr (EStr s)  = "sp_make_str(\"" <> escapeC s <> "\")"

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
compileExpr (EList [ESym "<=", a, b]) =
    "sp_lte(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym ">=", a, b]) =
    "sp_gte(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- 文字列操作
compileExpr (EList [ESym "string-append", a, b]) =
    "sp_str_append(" <> compileExpr a <> ", " <> compileExpr b <> ")"
compileExpr (EList [ESym "string-length", s]) =
    "sp_str_length(" <> compileExpr s <> ")"
compileExpr (EList [ESym "substring", s, start, end]) =
    "sp_substring(" <> compileExpr s <> ", " <> compileExpr start <> ", " <> compileExpr end <> ")"
compileExpr (EList [ESym "string=?", a, b]) =
    "sp_str_eq(" <> compileExpr a <> ", " <> compileExpr b <> ")"

-- ファイル I/O
compileExpr (EList [ESym "read-file", path]) =
    "sp_read_file(" <> compileExpr path <> ")"
compileExpr (EList [ESym "write-file", path, content]) =
    "sp_write_file(" <> compileExpr path <> ", " <> compileExpr content <> ")"
compileExpr (EList [ESym "append-file", path, content]) =
    "sp_append_file(" <> compileExpr path <> ", " <> compileExpr content <> ")"
compileExpr (EList [ESym "file-exists?", path]) =
    "sp_file_exists(" <> compileExpr path <> ")"

-- 変数参照 (引数など)
compileExpr (ESym s) = mangle s

-- ユーザー定義関数の呼び出し (プリミティブでない関数)
compileExpr (EList (ESym fname : args))
    | fname `notElem` primitives =
        let cFun = mangle fname
            cArgs = T.intercalate ", " (map compileExpr args)
        in cFun <> "(" <> cArgs <> ")"
  where
    primitives = ["+", "-", "*", "/", "=", "<", ">", "<=", ">=", "if", "defun",
                  "string-append", "string-length", "substring", "string=?",
                  "read-file", "write-file", "append-file", "file-exists?"]

-- 未実装のパターン
compileExpr other = "sp_make_nil() /* TODO: " <> T.pack (show other) <> " */"

-- | C 文字列リテラルのエスケープ
escapeC :: Text -> Text
escapeC = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar '\n' = "\\n"
    escapeChar '\t' = "\\t"
    escapeChar '\r' = "\\r"
    escapeChar c    = T.singleton c
