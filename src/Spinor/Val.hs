{-# LANGUAGE OverloadedStrings #-}

module Spinor.Val
  ( Val(..)
  , Env
  , showVal
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

import Spinor.Syntax (Expr)

-- | 変数環境: シンボル名 → 値
--   Val と Env は相互に参照するため同一モジュールに定義する。
type Env = Map.Map Text Val

-- | 評価結果の値型
--   Lisp の値を Haskell の代数的データ型にマッピングする。
--   Expr (構文) と Val (値) を分離することで、評価前後の区別を型レベルで保証する。
data Val
  = VInt  Integer                              -- 整数値
  | VBool Bool                                 -- 真偽値
  | VPrim Text ([Val] -> Either Text Val)      -- プリミティブ関数 (名前, 実装)
  | VFunc  [Text] Expr Env                     -- ユーザー定義関数 (引数名, 本体, クロージャ環境)
  | VMacro [Text] Expr Env                     -- マクロ (引数名, 本体, クロージャ環境)
  | VList [Val]                                -- リスト
  | VNil                                       -- 空リスト / nil
  | VSym  Text                                 -- シンボル (quote 用)
  | VStr  Text                                 -- 文字列

-- | テスト用の構造的等値比較
--   VPrim, VFunc, VMacro は関数を含むため常に不等
instance Eq Val where
  VInt  a   == VInt  b   = a == b
  VBool a   == VBool b   = a == b
  VStr  a   == VStr  b   = a == b
  VSym  a   == VSym  b   = a == b
  VList as  == VList bs  = as == bs
  VNil      == VNil      = True
  _         == _         = False

instance Show Val where
  show = showVal

-- | Val の表示用関数
showVal :: Val -> String
showVal (VInt n)       = show n
showVal (VBool True)   = "#t"
showVal (VBool False)  = "#f"
showVal (VPrim name _) = "<primitive:" ++ show name ++ ">"
showVal (VFunc _ _ _)  = "<function>"
showVal (VMacro _ _ _) = "<macro>"
showVal (VList vs)     = "(" ++ unwords (map showVal vs) ++ ")"
showVal VNil           = "nil"
showVal (VSym s)       = show s
showVal (VStr s)       = show s
