{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Spinor.Syntax    (Expr, readExpr, parseFile)
import Spinor.Type      (TypeEnv, showType)
import Spinor.Val       (Env)
import Spinor.Eval      (Eval, eval, runEval)
import Spinor.Expander  (expand)
import Spinor.Infer     (Types(..), runInfer, inferTop, baseTypeEnv)
import Spinor.Primitive (primitiveBindings)

-- | Twister ファイル一覧 (ロード順)
twisterFiles :: [FilePath]
twisterFiles =
  [ "twister/core.spin"
  , "twister/list.spin"
  , "twister/math.spin"
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  case args of
    []     -> replMode
    [file] -> batchMode file
    _      -> putStrLn "Usage: spinor [file]"

-- | REPL モード (引数なし)
replMode :: IO ()
replMode = do
  putStrLn "Spinor REPL (step16)"
  (env, tyEnv) <- loadBoot primitiveBindings baseTypeEnv
  loop env tyEnv

-- | バッチ実行モード (引数でファイル指定)
batchMode :: FilePath -> IO ()
batchMode file = do
  (env, _tyEnv) <- loadBoot primitiveBindings baseTypeEnv
  content <- TIO.readFile file
  case parseFile content of
    Left err -> do
      putStrLn $ "パースエラー: " ++ err
      exitFailure
    Right exprs -> do
      result <- foldlM' evalBatchExpr (env, True) exprs
      case result of
        (_, True)  -> putStrLn "\nAll tests passed."
        (_, False) -> do
          putStrLn "\nSome tests FAILED."
          exitFailure

-- | バッチモードで式を1つ評価する
evalBatchExpr :: (Env, Bool) -> Expr -> IO (Env, Bool)
evalBatchExpr (env, ok) expr = do
  expandResult <- runEval env (expand expr)
  case expandResult of
    Left err -> do
      TIO.putStrLn $ "エラー: " <> err
      pure (env, False)
    Right (expanded, envAfterExpand) -> do
      evalResult <- runEval envAfterExpand (eval expanded)
      case evalResult of
        Left err -> do
          TIO.putStrLn $ "エラー: " <> err
          pure (envAfterExpand, False)
        Right (_, env') -> pure (env', ok)

-- | 起動時に Twister ファイルをロードする
--   各式を展開 → 型推論 (ベストエフォート) → 評価し、
--   実行環境と型環境の両方を更新する
loadBoot :: Env -> TypeEnv -> IO (Env, TypeEnv)
loadBoot env tyEnv = do
  allExist <- mapM doesFileExist twisterFiles
  if not (and allExist)
    then do
      putStrLn "(Twister ファイルが見つかりません — スキップ)"
      pure (env, tyEnv)
    else do
      putStrLn "Loading Twister environment..."
      (env', tyEnv') <- foldlM' loadSpinFile (env, tyEnv) twisterFiles
      putStrLn "Twister loaded."
      pure (env', tyEnv')

-- | 単一の .spin ファイルをロードする
loadSpinFile :: (Env, TypeEnv) -> FilePath -> IO (Env, TypeEnv)
loadSpinFile (env, tyEnv) path = do
  content <- TIO.readFile path
  case parseFile content of
    Left err -> do
      putStrLn $ path ++ " パースエラー: " ++ err
      pure (env, tyEnv)
    Right exprs -> foldlM' processBootExpr (env, tyEnv) exprs

-- | boot 中の単一式を処理する
--   1. マクロ展開
--   2. 型推論 (失敗しても続行)
--   3. 評価
processBootExpr :: (Env, TypeEnv) -> Expr -> IO (Env, TypeEnv)
processBootExpr (env, tyEnv) expr = do
  -- 1. マクロ展開
  expandResult <- runEval env (expand expr)
  case expandResult of
    Left _ -> do
      -- 展開失敗: 評価のみ試行
      evalResult <- runEval env (evalExpr expr)
      case evalResult of
        Left _           -> pure (env, tyEnv)
        Right (_, env')  -> pure (env', tyEnv)
    Right (expanded, envAfterExpand) -> do
      -- 2. 型推論 (ベストエフォート)
      let tyEnv' = case runInfer (inferTop tyEnv expanded) of
                     Right (te, _, _) -> te
                     Left _           -> tyEnv
      -- 3. 評価
      evalResult <- runEval envAfterExpand (eval expanded)
      case evalResult of
        Left _           -> pure (envAfterExpand, tyEnv')
        Right (_, env')  -> pure (env', tyEnv')

-- | 式を展開してから評価する (Eval モナド内)
evalExpr :: Expr -> Eval ()
evalExpr expr = expand expr >>= eval >> pure ()

-- | 厳密な左畳み込み (IO 版)
foldlM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM' _ acc []     = pure acc
foldlM' f acc (x:xs) = do
  acc' <- f acc x
  foldlM' f acc' xs

loop :: Env -> TypeEnv -> IO ()
loop env tyEnv = do
  putStr "spinor> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof
    then putStrLn "\nBye."
    else do
      line <- TIO.getLine
      if T.null (T.strip line)
        then loop env tyEnv
        else do
          case readExpr line of
            Left err -> do
              putStrLn err
              loop env tyEnv
            Right ast -> do
              -- 1. マクロ展開
              expandResult <- runEval env (expand ast)
              case expandResult of
                Left err -> do
                  TIO.putStrLn $ "展開エラー: " <> err
                  loop env tyEnv
                Right (expanded, _) -> do
                  -- 2. 型推論 (inferTop で型環境も更新)
                  case runInfer (inferTop tyEnv expanded) of
                    Left tyErr -> do
                      -- 型推論失敗: 型エラーを表示し、実行しない
                      TIO.putStrLn $ "型エラー: " <> tyErr
                      loop env tyEnv
                    Right (tyEnv', subst, ty) -> do
                      -- 型推論成功: 型を表示してから評価
                      let finalType = apply subst ty
                      TIO.putStrLn $ ":: " <> showType finalType
                      result <- runEval env (expand ast >>= eval)
                      case result of
                        Left err -> do
                          TIO.putStrLn $ "エラー: " <> err
                          loop env tyEnv'
                        Right (val, env') -> do
                          putStrLn (show val)
                          loop env' tyEnv'
