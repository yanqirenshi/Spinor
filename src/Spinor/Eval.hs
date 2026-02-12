{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spinor.Eval
  ( Eval
  , Env
  , eval
  , runEval
  ) where

import Data.Text (Text, pack)
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Syntax (Expr(..))
import Spinor.Val    (Val(..), showVal)

-- | 変数環境: シンボル名 → 値
type Env = Map.Map Text Val

-- | 評価モナド
--   StateT  : 変数環境の読み書き
--   ExceptT : エラーハンドリング
--   IO      : 将来の拡張用 (ファイル読み込みなど)
newtype Eval a = Eval (StateT Env (ExceptT Text IO) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadError Text, MonadIO)

-- | Eval モナドを実行する
runEval :: Env -> Eval a -> IO (Either Text (a, Env))
runEval env (Eval m) = runExceptT (runStateT m env)

-- | 式を評価して値を返す
eval :: Expr -> Eval Val

-- アトム: 整数 → VInt
eval (EInt n) = pure $ VInt n

-- アトム: 真偽値 → VBool
eval (EBool b) = pure $ VBool b

-- アトム: シンボル → 環境から検索
eval (ESym name) = do
  env <- get
  case Map.lookup name env of
    Just val -> pure val
    Nothing  -> throwError $ "未定義のシンボル: " <> name

-- 空リスト → VNil
eval (EList []) = pure VNil

-- 特殊形式: (define sym expr)
eval (EList [ESym "define", ESym name, body]) = do
  val <- eval body
  modify (Map.insert name val)
  pure val

-- 特殊形式: (if cond then else)
eval (EList [ESym "if", cond, thenE, elseE]) = do
  c <- eval cond
  case c of
    VBool False -> eval elseE
    VNil        -> eval elseE
    _           -> eval thenE

-- 関数適用: (f arg1 arg2 ...)
eval (EList (x:xs)) = do
  func <- eval x
  args <- mapM eval xs
  case func of
    VPrim _ f -> case f args of
      Right val -> pure val
      Left  err -> throwError err
    other -> throwError $ "関数ではない値を適用しようとしました: " <> pack (showVal other)
