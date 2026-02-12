{-# LANGUAGE OverloadedStrings #-}

module Spinor.Primitive
  ( primitiveBindings
  ) where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map

import Spinor.Val (Val(..))
import Spinor.Eval (Env)

-- | 初期環境: プリミティブ関数を束縛した Env
primitiveBindings :: Env
primitiveBindings = Map.fromList
  [ ("+",  VPrim "+"  $ numBinOp (+))
  , ("-",  VPrim "-"  $ numBinOp (-))
  , ("*",  VPrim "*"  $ numBinOp (*))
  , ("=",  VPrim "="  numEq)
  , ("<",  VPrim "<"  $ numCmp (<))
  , (">",  VPrim ">"  $ numCmp (>))
  ]

-- | 整数の二項演算をラップするヘルパー
numBinOp :: (Integer -> Integer -> Integer) -> [Val] -> Either Text Val
numBinOp op [VInt a, VInt b] = Right $ VInt (op a b)
numBinOp _  [_, _]           = Left "算術演算には整数が必要です"
numBinOp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                    <> tshow (length args) <> ")"

-- | 整数の等値比較
numEq :: [Val] -> Either Text Val
numEq [VInt a, VInt b] = Right $ VBool (a == b)
numEq [_, _]           = Left "= には整数が必要です"
numEq args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                              <> tshow (length args) <> ")"

-- | 整数の比較演算をラップするヘルパー
numCmp :: (Integer -> Integer -> Bool) -> [Val] -> Either Text Val
numCmp op [VInt a, VInt b] = Right $ VBool (op a b)
numCmp _  [_, _]           = Left "比較演算には整数が必要です"
numCmp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                  <> tshow (length args) <> ")"

tshow :: Show a => a -> Text
tshow = pack . show
