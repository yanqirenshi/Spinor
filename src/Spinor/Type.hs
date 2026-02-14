{-# LANGUAGE OverloadedStrings #-}

module Spinor.Type
  ( Type(..)
  , Scheme(..)
  , TypeEnv
  , showType
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

-- | 型
data Type
  = TVar  Text       -- ^ 型変数: a, b, ...
  | TInt             -- ^ 整数型
  | TBool            -- ^ 真偽値型
  | TStr             -- ^ 文字列型
  | TArr  Type Type  -- ^ 関数型: t1 -> t2
  | TList Type       -- ^ リスト型: [t]
  deriving (Eq, Ord, Show)

-- | 型スキーム (多相型)
--   forall a b. a -> b -> a のような量子化された型を表す。
--   Scheme ["a", "b"] (TArr (TVar "a") (TArr (TVar "b") (TVar "a")))
data Scheme = Scheme [Text] Type
  deriving (Eq, Show)

-- | 型環境: 変数名から型スキームへのマッピング
type TypeEnv = Map.Map Text Scheme

-- | 型の人間向け表示
showType :: Type -> Text
showType (TVar n)     = n
showType TInt         = "Int"
showType TBool        = "Bool"
showType TStr         = "Str"
showType (TArr t1 t2) = "(" <> showType t1 <> " -> " <> showType t2 <> ")"
showType (TList t)    = "[" <> showType t <> "]"
