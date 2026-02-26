{-# LANGUAGE OverloadedStrings #-}

module Spinor.Type
  ( Type(..)
  , Scheme(..)
  , TypeEnv
  , Linearity(..)
  , showType
  , showLinearity
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map

-- | 線形性修飾子 (Experimental: 所有権システム)
--   Linear: 値は必ず一度だけ使用される
--   Unrestricted: 従来通りの制限なし
data Linearity
  = Linear        -- ^ 線形: 一度だけ消費しなければならない
  | Unrestricted  -- ^ 非線形: 自由に使用可能 (デフォルト)
  deriving (Eq, Ord, Show)

-- | 型
data Type
  = TVar  Text       -- ^ 型変数: a, b, ...
  | TInt             -- ^ 整数型
  | TBool            -- ^ 真偽値型
  | TStr             -- ^ 文字列型
  | TArr  Type Type  -- ^ 関数型: t1 -> t2
  | TList Type       -- ^ リスト型: [t]
  | TCon  Text       -- ^ 型コンストラクタ名 (例: "Maybe")
  | TApp  Type Type  -- ^ 型適用 (例: TApp (TCon "Maybe") TInt)
  | TLinear Linearity Type  -- ^ 線形型修飾: 所有権追跡用 (Experimental)
  deriving (Eq, Ord, Show)

-- | 型スキーム (多相型)
--   forall a b. a -> b -> a のような量子化された型を表す。
--   Scheme ["a", "b"] (TArr (TVar "a") (TArr (TVar "b") (TVar "a")))
data Scheme = Scheme [Text] Type
  deriving (Eq, Show)

-- | 型環境: 変数名から型スキームへのマッピング
type TypeEnv = Map.Map Text Scheme

-- | 線形性の表示
showLinearity :: Linearity -> Text
showLinearity Linear       = "linear"
showLinearity Unrestricted = "unrestricted"

-- | 型の人間向け表示
showType :: Type -> Text
showType (TVar n)     = n
showType TInt         = "Int"
showType TBool        = "Bool"
showType TStr         = "Str"
showType (TArr t1 t2) = "(" <> showType t1 <> " -> " <> showType t2 <> ")"
showType (TList t)    = "[" <> showType t <> "]"
showType (TCon n)     = n
showType (TApp t1 t2) = "(" <> showType t1 <> " " <> showType t2 <> ")"
showType (TLinear lin t) = showLinearity lin <> " " <> showType t
