{-# LANGUAGE OverloadedStrings #-}

module Spinor.Val
  ( Val(..)
  , showVal
  ) where

import Data.Text (Text)

-- | 評価結果の値型
--   Lisp の値を Haskell の代数的データ型にマッピングする。
--   Expr (構文) と Val (値) を分離することで、評価前後の区別を型レベルで保証する。
data Val
  = VInt  Integer              -- 整数値
  | VBool Bool                 -- 真偽値
  | VPrim Text ([Val] -> Either Text Val)  -- プリミティブ関数 (名前, 実装)
  | VList [Val]                -- リスト
  | VNil                       -- 空リスト / nil

instance Show Val where
  show = showVal

-- | Val の表示用関数
showVal :: Val -> String
showVal (VInt n)    = show n
showVal (VBool True)  = "#t"
showVal (VBool False) = "#f"
showVal (VPrim name _) = "<primitive:" ++ show name ++ ">"
showVal (VList vs)  = "(" ++ unwords (map showVal vs) ++ ")"
showVal VNil        = "nil"
