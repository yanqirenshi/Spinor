{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spinor.Infer
  ( Subst
  , Types(..)
  , nullSubst
  , composeSubst
  , unify
  , Infer
  , runInfer
  , infer
  , inferTop
  , generalize
  , baseTypeEnv
  ) where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (foldM)
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Type   (Type(..), Scheme(..), TypeEnv, showType)
import Spinor.Syntax (Expr(..))

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
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- ============================================================
-- Types クラス (置換の適用と自由型変数の取得)
-- ============================================================

-- | 型に対する操作を統一するクラス
class Types a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set Text

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

instance Types Scheme where
  apply s (Scheme vars t) = Scheme vars (apply s' t)
    where s' = foldr Map.delete s vars
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- TypeEnv に対する Types インスタンス
instance {-# OVERLAPPING #-} Types TypeEnv where
  apply s = Map.map (apply s)
  ftv     = ftv . Map.elems

-- ============================================================
-- 単一化 (Unification)
-- ============================================================

unify :: Type -> Type -> Either Text Subst

unify TInt  TInt  = Right nullSubst
unify TBool TBool = Right nullSubst
unify TStr  TStr  = Right nullSubst

unify (TVar a) t = varBind a t
unify t (TVar a) = varBind a t

unify (TArr t1 t2) (TArr t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  Right (composeSubst s2 s1)

unify (TList t1) (TList t2) = unify t1 t2

unify t1 t2 = Left $ "型が一致しません: " <> showType t1 <> " と " <> showType t2

varBind :: Text -> Type -> Either Text Subst
varBind a t
  | t == TVar a          = Right nullSubst
  | a `Set.member` ftv t = Left $ "無限型エラー: " <> a
                                <> " は " <> showType t <> " に出現します"
  | otherwise            = Right (Map.singleton a t)

-- ============================================================
-- Infer モナド
-- ============================================================

-- | 型推論モナド
--   StateT Int: フレッシュ型変数のカウンタ (t0, t1, t2, ...)
--   ExceptT Text: 型エラーの報告
newtype Infer a = Infer (StateT Int (Either Text) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError Text)

-- | Infer モナドを実行する
runInfer :: Infer a -> Either Text a
runInfer (Infer m) = fmap fst (runStateT m 0)

-- | 新しい型変数を生成する (t0, t1, t2, ...)
fresh :: Infer Type
fresh = do
  n <- get
  put (n + 1)
  pure $ TVar ("t" <> pack (show n))

-- ============================================================
-- instantiate / generalize
-- ============================================================

-- | Scheme の量子化された変数をフレッシュ型変数に置き換える
--   forall a b. a -> b  →  t0 -> t1  (フレッシュな型変数で具体化)
instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (const fresh) vars
  let s = Map.fromList (zip vars freshVars)
  pure (apply s t)

-- | 環境に含まれない自由型変数を量子化して Scheme にする
--   例: 環境の ftv = {}, 型 = a -> a  →  forall a. a -> a
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (Set.toList vars) t
  where vars = ftv t `Set.difference` ftv env

-- ============================================================
-- 型推論 (Algorithm W)
-- ============================================================

-- | AST を走査して型を推論する
--   戻り値: (置換, 推論された型)
infer :: TypeEnv -> Expr -> Infer (Subst, Type)

-- 整数リテラル → TInt
infer _ (EInt _) = pure (nullSubst, TInt)

-- 真偽値リテラル → TBool
infer _ (EBool _) = pure (nullSubst, TBool)

-- 文字列リテラル → TStr
infer _ (EStr _) = pure (nullSubst, TStr)

-- シンボル → 型環境から検索して instantiate
infer env (ESym x) =
  case Map.lookup x env of
    Just scheme -> do
      t <- instantiate scheme
      pure (nullSubst, t)
    Nothing -> throwError $ "未定義のシンボル: " <> x

-- 空リスト → フレッシュな要素型の空リスト
infer _ (EList []) = do
  a <- fresh
  pure (nullSubst, TList a)

-- quote → quote の中身を型推論 (リテラルとリストのみ)
infer _ (EList [ESym "quote", expr]) = pure (nullSubst, inferQuote expr)

-- if: cond は Bool, then と else の型を単一化
infer env (EList [ESym "if", cond, thn, els]) = do
  (s1, tCond) <- infer env cond
  s1' <- liftEither $ unify (apply s1 tCond) TBool
  let s1'' = composeSubst s1' s1
  (s2, tThn) <- infer (apply s1'' env) thn
  let s12 = composeSubst s2 s1''
  (s3, tEls) <- infer (apply s12 env) els
  let s123 = composeSubst s3 s12
  s4 <- liftEither $ unify (apply s123 tThn) (apply s123 tEls)
  let sFinal = composeSubst s4 s123
  pure (sFinal, apply sFinal tThn)

-- let: Let多相 — val を推論 → generalize → body を推論
infer env (ELet name val body) = do
  (s1, t1) <- infer env val
  let env'   = apply s1 env
      scheme = generalize env' t1
      env''  = Map.insert name scheme env'
  (s2, t2) <- infer env'' body
  pure (composeSubst s2 s1, t2)

-- define / def: 本体を推論し、環境に追加
infer env (EList [ESym "define", ESym name, body]) = inferDefine env name body
infer env (EList [ESym "def",    ESym name, body]) = inferDefine env name body

-- fn (固定長引数): 引数にフレッシュ型変数を割り当て、本体を推論
infer env (EList [ESym "fn", EList params, body]) = do
  paramNames <- mapM extractSymName params
  freshTypes <- mapM (const fresh) paramNames
  let paramSchemes = map (\t -> Scheme [] t) freshTypes
      env' = Map.union (Map.fromList (zip paramNames paramSchemes)) env
  (s, tBody) <- infer env' body
  let tFunc = foldr (\t acc -> TArr (apply s t) acc) (apply s tBody) freshTypes
  pure (s, tFunc)

-- fn (全引数キャプチャ): 引数リスト全体を1つのリスト型として扱う
infer env (EList [ESym "fn", ESym param, body]) = do
  a <- fresh
  let paramT = TList a
      env' = Map.insert param (Scheme [] paramT) env
  (s, tBody) <- infer env' body
  pure (s, TArr (apply s paramT) (apply s tBody))

-- 関数適用: (func arg1 arg2 ...)
--   多引数はカリー化として扱う
infer env (EList (func : args)) = inferApp env func args

-- ============================================================
-- トップレベル推論 (inferTop)
-- ============================================================

-- | トップレベル式を推論し、define の場合は型環境を更新する
--   define: 右辺を推論 → generalize して型環境に登録 → 更新された型環境を返す
--   それ以外: 通常の infer → 型環境は変更なし
inferTop :: TypeEnv -> Expr -> Infer (TypeEnv, Subst, Type)
inferTop env (EList [ESym "define", ESym name, body]) = inferTopDefine env name body
inferTop env (EList [ESym "def",    ESym name, body]) = inferTopDefine env name body
inferTop env expr = do
  (s, t) <- infer env expr
  pure (apply s env, s, t)

-- | define / def のトップレベル推論
--   1. 再帰対応: フレッシュ型変数を環境に仮登録
--   2. 右辺を推論
--   3. 仮型変数と推論結果を単一化
--   4. generalize して多相型に昇格
--   5. 更新された型環境を返す
inferTopDefine :: TypeEnv -> Text -> Expr -> Infer (TypeEnv, Subst, Type)
inferTopDefine env name body = do
  tv <- fresh
  let env' = Map.insert name (Scheme [] tv) env
  (s1, tBody) <- infer env' body
  s2 <- liftEither $ unify (apply s1 tv) tBody
  let sFinal = composeSubst s2 s1
      finalType = apply sFinal tBody
      scheme = generalize (apply sFinal env) finalType
      newEnv = Map.insert name scheme (apply sFinal env)
  pure (newEnv, sFinal, finalType)

-- | quote 内の型を静的に推論する (簡易版)
inferQuote :: Expr -> Type
inferQuote (EInt _)    = TInt
inferQuote (EBool _)   = TBool
inferQuote (EStr _)    = TStr
inferQuote (EList [])  = TList (TVar "_q")
inferQuote (EList (x:_)) = TList (inferQuote x)
inferQuote (ESym _)    = TStr  -- quote されたシンボルは文字列的に扱う
inferQuote (ELet _ _ body) = inferQuote body

-- | define / def の型推論共通実装
inferDefine :: TypeEnv -> Text -> Expr -> Infer (Subst, Type)
inferDefine env name body = do
  -- 再帰対応: 本体推論前にフレッシュ型変数を環境に入れる
  tv <- fresh
  let env' = Map.insert name (Scheme [] tv) env
  (s1, tBody) <- infer env' body
  s2 <- liftEither $ unify (apply s1 tv) tBody
  let sFinal = composeSubst s2 s1
  pure (sFinal, apply sFinal tBody)

-- | 関数適用の型推論 (多引数対応)
--   func を推論 → 引数を順に推論 → func の型を arg1 -> arg2 -> ... -> ret と単一化
inferApp :: TypeEnv -> Expr -> [Expr] -> Infer (Subst, Type)
inferApp env func args = do
  (s0, tFunc) <- infer env func
  tRet <- fresh
  -- 引数を左から順に推論し、置換を累積する
  (sFinal, tArgTypes) <- foldM inferArg (s0, []) args
  -- func の型を arg1 -> arg2 -> ... -> ret と単一化
  let expectedFuncType = foldr TArr tRet (reverse tArgTypes)
  sUnify <- liftEither $ unify (apply sFinal tFunc) (apply sFinal expectedFuncType)
  let sResult = composeSubst sUnify sFinal
  pure (sResult, apply sResult tRet)
  where
    inferArg (sAcc, ts) argExpr = do
      (s1, tArg) <- infer (apply sAcc env) argExpr
      let s' = composeSubst s1 sAcc
      pure (s', tArg : ts)

-- | Expr からシンボル名を取り出す (パラメータリスト用)
extractSymName :: Expr -> Infer Text
extractSymName (ESym s) = pure s
extractSymName _        = throwError "引数にはシンボルが必要です"

-- ============================================================
-- プリミティブの型環境
-- ============================================================

-- | プリミティブ関数の初期型環境
baseTypeEnv :: TypeEnv
baseTypeEnv = Map.fromList
  [ -- 算術演算: Int -> Int -> Int
    ("+",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("-",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("*",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("%",  Scheme [] (TArr TInt (TArr TInt TInt)))
    -- 比較演算: Int -> Int -> Bool
  , ("<",  Scheme [] (TArr TInt (TArr TInt TBool)))
  , (">",  Scheme [] (TArr TInt (TArr TInt TBool)))
    -- 等値比較: forall a. a -> a -> Bool
  , ("=",  Scheme ["a"] (TArr (TVar "a") (TArr (TVar "a") TBool)))
    -- リスト操作
  , ("cons",   Scheme ["a"] (TArr (TVar "a") (TArr (TList (TVar "a")) (TList (TVar "a")))))
  , ("car",    Scheme ["a"] (TArr (TList (TVar "a")) (TVar "a")))
  , ("cdr",    Scheme ["a"] (TArr (TList (TVar "a")) (TList (TVar "a"))))
  , ("list",   Scheme [] (TArr (TVar "_") (TList (TVar "_"))))  -- 簡易版: 単引数として扱う
  , ("null?",  Scheme ["a"] (TArr (TList (TVar "a")) TBool))
  , ("empty?", Scheme ["a"] (TArr (TList (TVar "a")) TBool))
    -- 出力
  , ("print",  Scheme ["a"] (TArr (TVar "a") (TVar "a")))
  ]
