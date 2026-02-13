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
  | VFunc [Text] Expr Env                      -- ユーザー定義関数 (引数名, 本体, クロージャ環境)
  | VList [Val]                                -- リスト
  | VNil                                       -- 空リスト / nil
  | VSym  Text                                 -- シンボル (quote 用)

instance Show Val where
  show = showVal

-- | Val の表示用関数
showVal :: Val -> String
showVal (VInt n)       = show n
showVal (VBool True)   = "#t"
showVal (VBool False)  = "#f"
showVal (VPrim name _) = "<primitive:" ++ show name ++ ">"
showVal (VFunc _ _ _)  = "<function>"
showVal (VList vs)     = "(" ++ unwords (map showVal vs) ++ ")"
showVal VNil           = "nil"
showVal (VSym s)       = show s
