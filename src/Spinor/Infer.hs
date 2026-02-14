{-# LANGUAGE OverloadedStrings #-}

module Spinor.Infer
  ( Subst
  , Types(..)
  , nullSubst
  , composeSubst
  , unify
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Spinor.Type (Type(..), Scheme(..))

-- ============================================================
-- 置換 (Substitution)
-- ============================================================

-- | 型変数名から具体的な型へのマッピング
type Subst = Map.Map Text Type

-- | 空の置換
nullSubst :: Subst
nullSubst = Map.empty

-- | 置換の合成
--   s1 `composeSubst` s2 は「まず s2 を適用し、次に s1 を適用する」置換。
--   s2 の各値に s1 を適用してから、s1 のマッピングを追加する。
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- ============================================================
-- Types クラス (置換の適用と自由型変数の取得)
-- ============================================================

-- | 型に対する操作を統一するクラス
class Types a where
  -- | 置換を適用する
  apply :: Subst -> a -> a
  -- | 自由型変数 (Free Type Variables) を返す
  ftv   :: a -> Set.Set Text

-- Type に対するインスタンス
instance Types Type where
  apply s (TVar n)     = Map.findWithDefault (TVar n) n s
  apply _ TInt         = TInt
  apply _ TBool        = TBool
  apply _ TStr         = TStr
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  apply s (TList t)    = TList (apply s t)

  ftv (TVar n)     = Set.singleton n
  ftv TInt         = Set.empty
  ftv TBool        = Set.empty
  ftv TStr         = Set.empty
  ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TList t)    = ftv t

-- Scheme に対するインスタンス
--   量子化された変数は自由ではないので除外する
instance Types Scheme where
  apply s (Scheme vars t) = Scheme vars (apply s' t)
    where s' = foldr Map.delete s vars
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars

-- リストに対するインスタンス
instance Types a => Types [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- ============================================================
-- 単一化 (Unification)
-- ============================================================

-- | 2つの型を一致させるための置換を求める
--
--   基本ルール:
--     1. 同じ基本型同士 → 空の置換 (一致済み)
--     2. 型変数と任意の型 → 型変数を束縛 (occurs check 付き)
--     3. 関数型同士 → 引数・戻り値を再帰的に単一化
--     4. リスト型同士 → 要素型を再帰的に単一化
--     5. それ以外 → 型エラー
unify :: Type -> Type -> Either Text Subst

-- 同じ基本型: 置換不要
unify TInt  TInt  = Right nullSubst
unify TBool TBool = Right nullSubst
unify TStr  TStr  = Right nullSubst

-- 型変数の束縛
unify (TVar a) t = varBind a t
unify t (TVar a) = varBind a t

-- 関数型: 引数型と戻り値型をそれぞれ単一化
--   1. まず引数型 (t1, t3) を単一化して置換 s1 を得る
--   2. s1 を戻り値型に適用してから、戻り値型 (t2, t4) を単一化して置換 s2 を得る
--   3. s2 と s1 を合成する
unify (TArr t1 t2) (TArr t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  Right (composeSubst s2 s1)

-- リスト型: 要素型を単一化
unify (TList t1) (TList t2) = unify t1 t2

-- 不一致
unify t1 t2 = Left $ "型が一致しません: " <> showType t1 <> " と " <> showType t2

-- | 型変数を型に束縛する (occurs check 付き)
--
--   Occurs check: 型変数 a を a -> b に束縛すると無限型になるため、
--   束縛先の型に同じ型変数が含まれていないかチェックする。
varBind :: Text -> Type -> Either Text Subst
varBind a t
  | t == TVar a       = Right nullSubst           -- 自分自身: 束縛不要
  | a `Set.member` ftv t = Left $ "無限型エラー: " <> a
                                <> " は " <> showType t <> " に出現します"
  | otherwise         = Right (Map.singleton a t)  -- 束縛

-- | 型の簡易表示 (エラーメッセージ用)
showType :: Type -> Text
showType (TVar n)     = n
showType TInt         = "Int"
showType TBool        = "Bool"
showType TStr         = "Str"
showType (TArr t1 t2) = "(" <> showType t1 <> " -> " <> showType t2 <> ")"
showType (TList t)    = "[" <> showType t <> "]"

-- ============================================================
-- テスト用関数
-- ============================================================

-- | 単一化のテスト
--   (a -> Int) と (Bool -> b) を単一化すると a=Bool, b=Int になるはず
_testUnify :: Either Text Subst
_testUnify = unify (TArr (TVar "a") TInt) (TArr TBool (TVar "b"))
-- 期待結果: Right (fromList [("a", TBool), ("b", TInt)])

-- | Occurs check のテスト
--   a と (a -> b) を単一化すると無限型エラーになるはず
_testOccursCheck :: Either Text Subst
_testOccursCheck = unify (TVar "a") (TArr (TVar "a") (TVar "b"))
-- 期待結果: Left "無限型エラー: a は (a -> b) に出現します"

-- | リスト型のテスト
--   [a] と [Int] を単一化すると a=Int になるはず
_testListUnify :: Either Text Subst
_testListUnify = unify (TList (TVar "a")) (TList TInt)
-- 期待結果: Right (fromList [("a", TInt)])

-- | 型エラーのテスト
--   Int と Bool は単一化できない
_testTypeMismatch :: Either Text Subst
_testTypeMismatch = unify TInt TBool
-- 期待結果: Left "型が一致しません: Int と Bool"

-- | 合成テスト
--   (a -> b) と (Int -> (c -> Bool)) を単一化
--   結果: a=Int, b=(c -> Bool)
_testCompose :: Either Text Subst
_testCompose = unify (TArr (TVar "a") (TVar "b"))
                     (TArr TInt (TArr (TVar "c") TBool))
-- 期待結果: Right (fromList [("a", TInt), ("b", TArr (TVar "c") TBool)])
