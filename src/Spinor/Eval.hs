{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE TypeApplications           #-}

module Spinor.Eval
  ( Eval
  , eval
  , runEval
  , applyClosureBody
  , exprToVal
  , valToExpr
  , throwErrorAt
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (void)
import Control.Exception (try, IOException)
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)

import Spinor.Syntax (Expr(..), Pattern(..), ConstructorDef(..), SourceSpan, SpinorError(..), dummySpan, exprSpan, formatError)
import Spinor.Val    (Val(..), Env, showVal)

-- | 評価モナド
--   StateT  : 変数環境の読み書き
--   ExceptT : エラーハンドリング (位置情報付き)
--   IO      : 将来の拡張用 (ファイル読み込みなど)
newtype Eval a = Eval (StateT Env (ExceptT SpinorError IO) a)
  deriving (Functor, Applicative, Monad, MonadState Env, MonadError SpinorError, MonadIO)

-- | Eval モナドを実行する
runEval :: Env -> Eval a -> IO (Either SpinorError (a, Env))
runEval env (Eval m) = runExceptT (runStateT m env)

-- | 位置情報付きエラーを投げるヘルパー
throwErrorAt :: SourceSpan -> Text -> Eval a
throwErrorAt sp msg = throwError (SpinorError sp msg)

-- | 式を評価して値を返す
eval :: Expr -> Eval Val

-- アトム: 整数 → VInt
eval (EInt _ n) = pure $ VInt n

-- アトム: 真偽値 → VBool
eval (EBool _ b) = pure $ VBool b

-- アトム: 文字列 → VStr
eval (EStr _ s) = pure $ VStr s

-- アトム: シンボル → 環境から検索
eval (ESym sp name) = do
  env <- get
  case Map.lookup name env of
    Just val -> pure val
    Nothing  -> throwErrorAt sp $ "未定義のシンボル: " <> name

-- 空リスト → VNil
eval (EList _ []) = pure VNil

-- 特殊形式: (quote expr) — 式を評価せず値として返す
eval (EList _ [ESym _ "quote", expr]) = pure (exprToVal expr)

-- 特殊形式: (define sym expr) / (def sym expr)
eval (EList _ [ESym _ "define", ESym _ name, body]) = evalDefine name body
eval (EList _ [ESym _ "def",    ESym _ name, body]) = evalDefine name body

-- 特殊形式: (let ((var1 val1) ...) body) — 並列ローカル変数束縛
--   すべての init-expr を現在の環境で評価した後、一括して束縛を追加する
eval (ELet _ bindings body) = do
  savedEnv <- get
  -- 1. すべての値を現在の環境で評価 (並列束縛)
  vals <- mapM (eval . snd) bindings
  let names = map fst bindings
      newBindings = Map.fromList (zip names vals)
  -- 2. 新しい束縛を追加した環境で body を評価
  modify (Map.union newBindings)
  result <- eval body
  put savedEnv
  pure result

-- data 式: (data TypeName (Con1 a b) (Con2)) — ADT 定義
--   各コンストラクタを環境に登録する
eval (EData _ _typeName constrs) = do
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

-- match 式: (match target (pat1 body1) (pat2 body2) ...)
eval (EMatch _ targetExpr branches) = do
  targetVal <- eval targetExpr
  matchBranches targetVal branches

-- module 宣言: ローダーで処理されるべき
eval (EModule sp _ _) =
  throwErrorAt sp "module: module宣言はファイルの先頭に配置し、モジュールローダーで処理される必要があります"

-- import 宣言: ローダーで処理されるべき
eval (EImport sp _ _) =
  throwErrorAt sp "import: import宣言はトップレベルに配置し、モジュールローダーで処理される必要があります"

-- 特殊形式: (print expr) — 値を表示して返す
eval (EList _ [ESym _ "print", arg]) = do
  val <- eval arg
  case val of
    VStr s -> liftIO $ TIO.putStrLn s
    _      -> liftIO $ putStrLn (showVal val)
  pure val

-- 特殊形式: (if cond then else)
eval (EList _ [ESym _ "if", cond, thenE, elseE]) = do
  c <- eval cond
  case c of
    VBool False -> eval elseE
    VNil        -> eval elseE
    _           -> eval thenE

-- 特殊形式: (begin expr1 expr2 ...) / (progn expr1 expr2 ...) — 順次評価
eval (EList _ (ESym _ "begin" : exprs)) = evalSequence exprs
eval (EList _ (ESym _ "progn" : exprs)) = evalSequence exprs

-- 特殊形式: (setq var new-value-expr) — 破壊的代入
eval (EList sp [ESym _ "setq", ESym _ name, valExpr]) = do
  env <- get
  case Map.lookup name env of
    Nothing -> throwErrorAt sp $ "setq: 未束縛の変数です: " <> name
    Just _  -> do
      val <- eval valExpr
      modify (Map.insert name val)
      pure val

-- 特殊形式: (spawn expr) — 新しいスレッドで式を評価
eval (EList _ [ESym _ "spawn", expr]) = do
  env <- get  -- 現在の環境をキャプチャ (クロージャと同じ原理)
  liftIO $ void $ forkIO $ do
    -- 新しいスレッドで評価を実行。エラーは表示。
    result <- runEval env (eval expr)
    case result of
      Left err -> TIO.putStrLn $ "[spawn error] " <> formatError err
      Right _  -> pure ()
  pure $ VBool True

-- 特殊形式: (sleep millis) — 指定ミリ秒だけスレッドを停止
eval (EList sp [ESym _ "sleep", arg]) = do
  val <- eval arg
  case val of
    VInt ms -> do
      liftIO $ threadDelay (fromInteger ms * 1000)  -- ミリ秒→マイクロ秒
      pure $ VBool True
    _ -> throwErrorAt sp "sleep: 整数が必要です"

-- 特殊形式: (new-mvar) / (new-mvar val) — MVar を作成
eval (EList _ [ESym _ "new-mvar"]) = do
  mvar <- liftIO newEmptyMVar
  pure $ VMVar mvar

eval (EList _ [ESym _ "new-mvar", arg]) = do
  val <- eval arg
  mvar <- liftIO $ newMVar val
  pure $ VMVar mvar

-- 特殊形式: (take-mvar mvar) — MVar から値を取り出す (ブロッキング)
eval (EList sp [ESym _ "take-mvar", arg]) = do
  val <- eval arg
  case val of
    VMVar mvar -> liftIO $ takeMVar mvar
    _ -> throwErrorAt sp "take-mvar: MVar が必要です"

-- 特殊形式: (put-mvar mvar val) — MVar に値を格納する (ブロッキング)
eval (EList sp [ESym _ "put-mvar", mvarExpr, valExpr]) = do
  mvarVal <- eval mvarExpr
  val <- eval valExpr
  case mvarVal of
    VMVar mvar -> do
      liftIO $ putMVar mvar val
      pure $ VBool True
    _ -> throwErrorAt sp "put-mvar: 第1引数には MVar が必要です"

-- 特殊形式: (fn (params...) body) — 固定長 / ドット記法可変長
--   現在の環境をキャプチャしてクロージャを生成する。
eval (EList _ [ESym _ "fn", EList _ params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VFunc paramNames body closureEnv

-- 特殊形式: (fn name body) — 全引数を1つのシンボルにキャプチャ
eval (EList _ [ESym _ "fn", ESym _ param, body]) = do
  closureEnv <- get
  pure $ VFunc [".", param] body closureEnv

-- 特殊形式: (mac (params...) body) — 固定長 / ドット記法可変長
eval (EList _ [ESym _ "mac", EList _ params, body]) = do
  paramNames <- mapM extractSym params
  closureEnv <- get
  pure $ VMacro paramNames body closureEnv

-- 特殊形式: (mac name body) — 全引数を1つのシンボルにキャプチャ
eval (EList _ [ESym _ "mac", ESym _ param, body]) = do
  closureEnv <- get
  pure $ VMacro [".", param] body closureEnv

-- 特殊形式: (dotimes (var count-expr) body...) — カウンタループ
--   setq の副作用がクロージャ境界を超えないため、特殊形式として実装
eval (EList sp (ESym _ "dotimes" : EList _ [ESym _ var, countExpr] : body)) = do
  countVal <- eval countExpr
  case countVal of
    VInt count -> dotimesLoop var count body 0
    _ -> throwErrorAt sp "dotimes: 整数が必要です"

-- 特殊形式: (dolist (var list-expr) body...) — リスト反復
eval (EList sp (ESym _ "dolist" : EList _ [ESym _ var, listExpr] : body)) = do
  listVal <- eval listExpr
  case listVal of
    VList xs -> dolistLoop var xs body
    VNil     -> pure VNil
    _        -> throwErrorAt sp "dolist: リストが必要です"

-- 特殊形式: (error message) — ランタイムエラーを発生させる
eval (EList sp [ESym _ "error", msgExpr]) = do
  val <- eval msgExpr
  case val of
    VStr msg -> throwErrorAt sp msg
    _        -> throwErrorAt sp "error: 文字列が必要です"

-- 特殊形式: (bound? symbol) — シンボルが環境に定義されているか判定
eval (EList sp [ESym _ "bound?", arg]) = do
  val <- eval arg
  case val of
    VSym name -> do
      env <- get
      pure $ VBool (Map.member name env)
    _ -> throwErrorAt sp "bound?: シンボルが必要です"

-- 特殊形式: (read-file path) — ファイルを読み込んで文字列として返す
eval (EList sp [ESym _ "read-file", pathExpr]) = do
  pathVal <- eval pathExpr
  case pathVal of
    VStr path -> do
      result <- liftIO $ try @IOException $ TIO.readFile (T.unpack path)
      case result of
        Right content -> pure $ VStr content
        Left err      -> throwErrorAt sp $ "read-file: " <> T.pack (show err)
    _ -> throwErrorAt sp "read-file: パスには文字列が必要です"

-- 特殊形式: (write-file path content) — ファイルに書き込む (上書き)
eval (EList sp [ESym _ "write-file", pathExpr, contentExpr]) = do
  pathVal <- eval pathExpr
  contentVal <- eval contentExpr
  case (pathVal, contentVal) of
    (VStr path, VStr content) -> do
      result <- liftIO $ try @IOException $ TIO.writeFile (T.unpack path) content
      case result of
        Right () -> pure $ VBool True
        Left err -> throwErrorAt sp $ "write-file: " <> T.pack (show err)
    _ -> throwErrorAt sp "write-file: (String, String) が必要です"

-- 特殊形式: (append-file path content) — ファイルに追記する
eval (EList sp [ESym _ "append-file", pathExpr, contentExpr]) = do
  pathVal <- eval pathExpr
  contentVal <- eval contentExpr
  case (pathVal, contentVal) of
    (VStr path, VStr content) -> do
      result <- liftIO $ try @IOException $ TIO.appendFile (T.unpack path) content
      case result of
        Right () -> pure $ VBool True
        Left err -> throwErrorAt sp $ "append-file: " <> T.pack (show err)
    _ -> throwErrorAt sp "append-file: (String, String) が必要です"

-- 特殊形式: (file-exists? path) — ファイルが存在するか確認
eval (EList sp [ESym _ "file-exists?", pathExpr]) = do
  pathVal <- eval pathExpr
  case pathVal of
    VStr path -> do
      exists <- liftIO $ doesFileExist (T.unpack path)
      pure $ VBool exists
    _ -> throwErrorAt sp "file-exists?: パスには文字列が必要です"

-- 特殊形式: (command-line-args) — コマンドライン引数を取得
eval (EList _ [ESym _ "command-line-args"]) = do
  args <- liftIO getArgs
  pure $ VList (map (VStr . T.pack) args)

-- 特殊形式: (getenv name) — 環境変数を取得
eval (EList sp [ESym _ "getenv", nameExpr]) = do
  nameVal <- eval nameExpr
  case nameVal of
    VStr name -> do
      result <- liftIO $ lookupEnv (T.unpack name)
      pure $ VStr $ maybe "" T.pack result
    _ -> throwErrorAt sp "getenv: 環境変数名には文字列が必要です"

-- 関数適用: (f arg1 arg2 ...)
--   マクロ展開は Expander.expand で処理済みの前提。
eval (EList _ (x:xs)) = do
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
  Left  err -> throwErrorAt dummySpan err

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
  throwErrorAt dummySpan $ "関数ではない値を適用しようとしました: " <> pack (showVal other)

-- | VFunc / VMacro 共通のクロージャ適用ロジック
applyClosureBody :: [Text] -> Expr -> Env -> [Val] -> Eval Val
applyClosureBody params body closureEnv args = do
  case bindArgs params args of
    Left err -> throwErrorAt (exprSpan body) err
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
exprToVal (EInt _ n)    = VInt n
exprToVal (EBool _ b)   = VBool b
exprToVal (ESym _ s)    = VSym s
exprToVal (EStr _ s)    = VStr s
exprToVal (EList _ xs)  = VList (map exprToVal xs)
exprToVal (ELet _ bindings body) =
  let bindingsVal = VList [VList [VSym name, exprToVal expr] | (name, expr) <- bindings]
  in VList [VSym "let", bindingsVal, exprToVal body]
exprToVal (EData _ name _) = VSym ("<data:" <> name <> ">")
exprToVal (EMatch _ _ _) = VSym "<match>"
exprToVal (EModule _ name _) = VSym ("<module:" <> name <> ">")
exprToVal (EImport _ name _) = VSym ("<import:" <> name <> ">")

-- | Val を Expr に逆変換する (マクロ展開結果の再評価用)
valToExpr :: Val -> Expr
valToExpr (VInt n)    = EInt dummySpan n
valToExpr (VBool b)   = EBool dummySpan b
valToExpr (VSym s)    = ESym dummySpan s
valToExpr (VStr s)    = EStr dummySpan s
valToExpr (VList vs)  = EList dummySpan (map valToExpr vs)
valToExpr VNil        = EList dummySpan []
valToExpr (VData name vs) = EList dummySpan (ESym dummySpan name : map valToExpr vs)
-- VMatch は存在しないが網羅性のため
valToExpr other       = ESym dummySpan $ "<" <> pack (showVal other) <> ">"

-- | match 式の分岐を上から順に試行する
matchBranches :: Val -> [(Pattern, Expr)] -> Eval Val
matchBranches _ [] = throwErrorAt dummySpan "match: どのパターンにもマッチしませんでした"
matchBranches val ((pat, body) : rest) =
  case matchPattern val pat of
    Nothing       -> matchBranches val rest
    Just bindings -> do
      savedEnv <- get
      modify (Map.union bindings)
      result <- eval body
      put savedEnv
      pure result

-- | パターンマッチを試行し、成功時は束縛を返す
matchPattern :: Val -> Pattern -> Maybe Env
matchPattern val (PVar name) = Just (Map.singleton name val)
matchPattern _   PWild       = Just Map.empty
matchPattern val (PLit expr) =
  let litVal = exprToVal expr
  in if val == litVal then Just Map.empty else Nothing
matchPattern (VData conName vals) (PCon pName pats)
  | conName == pName && length vals == length pats =
      foldl (\acc (v, p) -> do
        binds <- acc
        bs <- matchPattern v p
        Just (Map.union bs binds)
      ) (Just Map.empty) (zip vals pats)
  | otherwise = Nothing
matchPattern _ (PCon _ _) = Nothing

-- | begin/progn の式リストを順次評価する
evalSequence :: [Expr] -> Eval Val
evalSequence []     = pure VNil
evalSequence [e]    = eval e
evalSequence (e:es) = eval e >> evalSequence es

-- | dotimes のループ実装
dotimesLoop :: Text -> Integer -> [Expr] -> Integer -> Eval Val
dotimesLoop _   count _    i | i >= count = pure VNil
dotimesLoop var count body i = do
  modify (Map.insert var (VInt i))
  mapM_ eval body
  dotimesLoop var count body (i + 1)

-- | dolist のループ実装
dolistLoop :: Text -> [Val] -> [Expr] -> Eval Val
dolistLoop _   []     _    = pure VNil
dolistLoop var (x:xs) body = do
  modify (Map.insert var x)
  mapM_ eval body
  dolistLoop var xs body

-- | パラメータリストからシンボル名を抽出する (fn / mac 共通)
extractSym :: Expr -> Eval Text
extractSym (ESym _ s) = pure s
extractSym other      = throwErrorAt (exprSpan other) $ "引数にはシンボルが必要です: "
                                  <> pack (show other)
