{-# LANGUAGE OverloadedStrings #-}

module Spinor.Primitive
  ( primitiveBindings
  ) where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map

import Spinor.Val (Val(..), Env)

-- | 初期環境: プリミティブ関数を束縛した Env
primitiveBindings :: Env
primitiveBindings = Map.fromList
  [ ("+",  VPrim "+"  $ numBinOp (+))
  , ("-",  VPrim "-"  $ numBinOp (-))
  , ("*",  VPrim "*"  $ numBinOp (*))
  , ("%",  VPrim "%"  $ numBinOp mod)
  , ("=",  VPrim "="  numEq)
  , ("<",  VPrim "<"  $ numCmp (<))
  , (">",  VPrim ">"  $ numCmp (>))
  , ("cons",  VPrim "cons"  primCons)
  , ("car",   VPrim "car"   primCar)
  , ("cdr",   VPrim "cdr"   primCdr)
  , ("list",  VPrim "list"  primList)
  , ("null?",  VPrim "null?"  primNull)
  , ("empty?", VPrim "empty?" primNull)
  ]

-- | 整数の二項演算をラップするヘルパー
numBinOp :: (Integer -> Integer -> Integer) -> [Val] -> Either Text Val
numBinOp op [VInt a, VInt b] = Right $ VInt (op a b)
numBinOp _  [_, _]           = Left "算術演算には整数が必要です"
numBinOp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                    <> tshow (length args) <> ")"

-- | 等値比較 (整数・真偽値対応)
numEq :: [Val] -> Either Text Val
numEq [VInt a, VInt b]   = Right $ VBool (a == b)
numEq [VBool a, VBool b] = Right $ VBool (a == b)
numEq [_, _]             = Left "= には整数または真偽値が必要です"
numEq args               = Left $ "引数の数が不正です (期待: 2, 実際: "
                                <> tshow (length args) <> ")"

-- | 整数の比較演算をラップするヘルパー
numCmp :: (Integer -> Integer -> Bool) -> [Val] -> Either Text Val
numCmp op [VInt a, VInt b] = Right $ VBool (op a b)
numCmp _  [_, _]           = Left "比較演算には整数が必要です"
numCmp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                  <> tshow (length args) <> ")"

-- | cons: 値をリストの先頭に追加
primCons :: [Val] -> Either Text Val
primCons [val, VList xs] = Right $ VList (val : xs)
primCons [val, VNil]     = Right $ VList [val]
primCons [_, _]          = Left "cons の第2引数にはリストが必要です"
primCons args            = Left $ "cons: 引数の数が不正です (期待: 2, 実際: "
                                <> tshow (length args) <> ")"

-- | car: リストの先頭要素を返す
primCar :: [Val] -> Either Text Val
primCar [VList (x:_)] = Right x
primCar [VList []]     = Left "car: 空リストに対しては適用できません"
primCar [VNil]         = Left "car: nil に対しては適用できません"
primCar [_]            = Left "car: リストが必要です"
primCar args           = Left $ "car: 引数の数が不正です (期待: 1, 実際: "
                              <> tshow (length args) <> ")"

-- | cdr: リストの先頭以外を返す
primCdr :: [Val] -> Either Text Val
primCdr [VList (_:xs)] = Right $ VList xs
primCdr [VList []]     = Left "cdr: 空リストに対しては適用できません"
primCdr [VNil]         = Left "cdr: nil に対しては適用できません"
primCdr [_]            = Left "cdr: リストが必要です"
primCdr args           = Left $ "cdr: 引数の数が不正です (期待: 1, 実際: "
                              <> tshow (length args) <> ")"

-- | list: 引数をリストにまとめる (可変長引数)
primList :: [Val] -> Either Text Val
primList args = Right $ VList args

-- | null?: リストが空かどうかを判定
primNull :: [Val] -> Either Text Val
primNull [VList []] = Right $ VBool True
primNull [VNil]     = Right $ VBool True
primNull [_]        = Right $ VBool False
primNull args       = Left $ "null?: 引数の数が不正です (期待: 1, 実際: "
                           <> tshow (length args) <> ")"

tshow :: Show a => a -> Text
tshow = pack . show
