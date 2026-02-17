{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}

module Spinor.Eval
  ( Eval
  , eval
  , runEval
  , applyClosureBody
  , exprToVal
  , valToExpr
  ) where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Syntax (Expr(..), ConstructorDef(..))
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

-- 特殊形式: (let name val body) — ローカル変数束縛
eval (ELet name valExpr body) = do
  val <- eval valExpr
  savedEnv <- get
  modify (Map.insert name val)
  result <- eval body
  put savedEnv
  pure result

-- data 式: (data TypeName (Con1 a b) (Con2)) — ADT 定義
--   各コンストラクタを環境に登録する
eval (EData _typeName constrs) = do
  mapM_ registerConstructor constrs
  pure VNil
  where
    registerConstructor (ConstructorDef cname fields) =
      case length fields of
        0 -> modify (Map.insert cname (VData cname []))
        n -> modify (Map.insert cname (VPrim cname (mkConstructor cname n)))
    mkConstructor cname arity args
      | length args == arity = Right $ VData cname args
      | otherwise = Left $ cname <> ": 引数の数が不正です (期待: "
                    <> pack (show arity) <> ", 実際: "
                    <> pack (show (length args)) <> ")"

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

-- 特殊形式: (fn (params...) body) — 固定長 / ドット記法可変長
--   現在の環境をキャプチャしてクロージャを生成する。
eval (EList [ESym "fn", EList params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VFunc paramNames body closureEnv

-- 特殊形式: (fn name body) — 全引数を1つのシンボルにキャプチャ
eval (EList [ESym "fn", ESym param, body]) = do
  closureEnv <- get
  pure $ VFunc [".", param] body closureEnv

-- 特殊形式: (mac (params...) body) — 固定長 / ドット記法可変長
eval (EList [ESym "mac", EList params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VMacro paramNames body closureEnv

-- 特殊形式: (mac name body) — 全引数を1つのシンボルにキャプチャ
eval (EList [ESym "mac", ESym param, body]) = do
  closureEnv <- get
  pure $ VMacro [".", param] body closureEnv

-- 関数適用: (f arg1 arg2 ...)
--   マクロ展開は Expander.expand で処理済みの前提。
eval (EList (x:xs)) = do
  func <- eval x
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
applyClosureBody params body closureEnv args = do
  case bindArgs params args of
    Left err -> throwError err
    Right bindings -> do
      savedEnv <- get
      let localEnv = Map.union bindings (Map.union closureEnv savedEnv)
      put localEnv
      result <- eval body
      put savedEnv
      pure result

-- | 仮引数と実引数の束縛 (固定長 / ドット記法可変長対応)
--   ["a", "b"]          — 固定長: 引数の数が一致する必要がある
--   ["a", ".", "rest"]  — 可変長: a に1つ束縛し、残りを rest にリストで束縛
--   [".", "args"]       — 全キャプチャ: 全引数を args にリストで束縛
bindArgs :: [Text] -> [Val] -> Either Text Env
bindArgs params args = case break (== ".") params of
  (fixed, []) ->
    -- 固定長引数
    if length fixed /= length args
    then Left $ "引数の数が不正です (期待: " <> pack (show (length fixed))
             <> ", 実際: " <> pack (show (length args)) <> ")"
    else Right $ Map.fromList (zip fixed args)
  (fixed, [".", rest]) ->
    -- 可変長引数: 固定部分 + 残り
    if length args < length fixed
    then Left $ "引数の数が不正です (最低: " <> pack (show (length fixed))
             <> ", 実際: " <> pack (show (length args)) <> ")"
    else let (fixedArgs, restArgs) = splitAt (length fixed) args
         in Right $ Map.fromList ((rest, VList restArgs) : zip fixed fixedArgs)
  _ -> Left "不正な引数リスト: '.' の後にはパラメータが1つ必要です"

-- | Expr を評価せずに Val に変換する (quote / マクロ引数用)
exprToVal :: Expr -> Val
exprToVal (EInt n)    = VInt n
exprToVal (EBool b)   = VBool b
exprToVal (ESym s)    = VSym s
exprToVal (EStr s)    = VStr s
exprToVal (EList xs)  = VList (map exprToVal xs)
exprToVal (ELet name val body) = VList [VSym "let", VSym name, exprToVal val, exprToVal body]
exprToVal (EData name _) = VSym ("<data:" <> name <> ">")

-- | Val を Expr に逆変換する (マクロ展開結果の再評価用)
valToExpr :: Val -> Expr
valToExpr (VInt n)    = EInt n
valToExpr (VBool b)   = EBool b
valToExpr (VSym s)    = ESym s
valToExpr (VStr s)    = EStr s
valToExpr (VList vs)  = EList (map valToExpr vs)
valToExpr VNil        = EList []
valToExpr (VData name vs) = EList (ESym name : map valToExpr vs)
valToExpr other       = ESym $ "<" <> pack (showVal other) <> ">"

-- | パラメータリストからシンボル名を抽出する (fn / mac 共通)
extractSym :: Expr -> Eval Text
extractSym (ESym s) = pure s
extractSym other    = throwError $ "引数にはシンボルが必要です: "
                                <> pack (show other)
