{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Spinor.Eval
  ( Eval
  , eval
  , runEval
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Syntax (Expr(..), parseFile)
import Spinor.Val    (Val(..), Env, showVal)

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

-- アトム: 文字列 → VStr
eval (EStr s) = pure $ VStr s

-- アトム: シンボル → 環境から検索
eval (ESym name) = do
  env <- get
  case Map.lookup name env of
    Just val -> pure val
    Nothing  -> throwError $ "未定義のシンボル: " <> name

-- 空リスト → VNil
eval (EList []) = pure VNil

-- 特殊形式: (quote expr) — 式を評価せず値として返す
eval (EList [ESym "quote", expr]) = pure (exprToVal expr)

-- 特殊形式: (define sym expr) / (def sym expr)
eval (EList [ESym "define", ESym name, body]) = evalDefine name body
eval (EList [ESym "def",    ESym name, body]) = evalDefine name body

-- 特殊形式: (load "filename") — ファイルを読み込み、全式を順次評価
eval (EList [ESym "load", arg]) = do
  val <- eval arg
  case val of
    VStr path -> do
      content <- liftIO $ TIO.readFile (T.unpack path)
      case parseFile content of
        Left err   -> throwError $ "load パースエラー (" <> path <> "): " <> pack err
        Right exprs -> do
          mapM_ eval exprs
          pure $ VBool True
    _ -> throwError "load: ファイルパス (文字列) が必要です"

-- 特殊形式: (print expr) — 値を表示して返す
eval (EList [ESym "print", arg]) = do
  val <- eval arg
  case val of
    VStr s -> liftIO $ TIO.putStrLn s
    _      -> liftIO $ putStrLn (showVal val)
  pure val

-- 特殊形式: (if cond then else)
eval (EList [ESym "if", cond, thenE, elseE]) = do
  c <- eval cond
  case c of
    VBool False -> eval elseE
    VNil        -> eval elseE
    _           -> eval thenE

-- 特殊形式: (fn (params...) body)
--   現在の環境をキャプチャしてクロージャを生成する。
--   Lisp の lambda → Haskell の VFunc (引数名リスト, 本体, 環境スナップショット)
eval (EList [ESym "fn", EList params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VFunc paramNames body closureEnv

-- 特殊形式: (mac (params...) body)
--   fn と同構造だが VMacro を生成する。マクロは適用時に引数を評価しない。
eval (EList [ESym "mac", EList params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VMacro paramNames body closureEnv

-- 関数適用 / マクロ展開: (f arg1 arg2 ...)
eval (EList (x:xs)) = do
  func <- eval x
  case func of
    -- マクロ: 引数を評価せずに適用し、結果を Expr に逆変換して再評価
    VMacro {} -> do
      let argVals = map exprToVal xs
      expanded <- apply func argVals
      eval (valToExpr expanded)
    -- 通常の関数: 引数を評価してから適用
    _ -> do
      args <- mapM eval xs
      apply func args

-- | define / def の共通実装
evalDefine :: Text -> Expr -> Eval Val
evalDefine name body = do
  val <- eval body
  modify (Map.insert name val)
  pure val

-- | 関数適用
apply :: Val -> [Val] -> Eval Val

-- プリミティブ関数
apply (VPrim _ f) args = case f args of
  Right val -> pure val
  Left  err -> throwError err

-- ユーザー定義関数 (クロージャ)
--   1. 引数の数を検証
--   2. 現在の環境を退避
--   3. クロージャ環境 + 引数束縛で新しい環境を構築
--   4. 本体を評価
--   5. 元の環境を復元 (レキシカルスコープ)
apply (VFunc params body closureEnv) args = applyClosureBody params body closureEnv args

-- マクロ適用 (VFunc と同じクロージャ適用ロジック)
apply (VMacro params body closureEnv) args = applyClosureBody params body closureEnv args

apply other _ =
  throwError $ "関数ではない値を適用しようとしました: " <> pack (showVal other)

-- | VFunc / VMacro 共通のクロージャ適用ロジック
applyClosureBody :: [Text] -> Expr -> Env -> [Val] -> Eval Val
applyClosureBody params body closureEnv args
  | length params /= length args =
      throwError $ "引数の数が不正です (期待: " <> pack (show (length params))
                <> ", 実際: " <> pack (show (length args)) <> ")"
  | otherwise = do
      savedEnv <- get
      let bindings = Map.fromList (zip params args)
          localEnv = Map.union bindings (Map.union closureEnv savedEnv)
      put localEnv
      result <- eval body
      put savedEnv
      pure result

-- | Expr を評価せずに Val に変換する (quote / マクロ引数用)
exprToVal :: Expr -> Val
exprToVal (EInt n)    = VInt n
exprToVal (EBool b)   = VBool b
exprToVal (ESym s)    = VSym s
exprToVal (EStr s)    = VStr s
exprToVal (EList xs)  = VList (map exprToVal xs)

-- | Val を Expr に逆変換する (マクロ展開結果の再評価用)
valToExpr :: Val -> Expr
valToExpr (VInt n)    = EInt n
valToExpr (VBool b)   = EBool b
valToExpr (VSym s)    = ESym s
valToExpr (VStr s)    = EStr s
valToExpr (VList vs)  = EList (map valToExpr vs)
valToExpr VNil        = EList []
valToExpr other       = ESym $ "<" <> pack (showVal other) <> ">"

-- | パラメータリストからシンボル名を抽出する (fn / mac 共通)
extractSym :: Expr -> Eval Text
extractSym (ESym s) = pure s
extractSym other    = throwError $ "引数にはシンボルが必要です: "
                                <> pack (show other)
