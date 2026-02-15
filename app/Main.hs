{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF)
import System.Directory (doesFileExist)
import Spinor.Syntax    (readExpr, parseFile)
import Spinor.Type      (TypeEnv, showType)
import Spinor.Val       (Env)
import Spinor.Eval      (runEval)
import Spinor.Expander  (expand, expandAndEval)
import Spinor.Infer     (Types(..), runInfer, inferTop, baseTypeEnv)
import Spinor.Primitive (primitiveBindings)

-- | ブートファイルのパス
bootFile :: FilePath
bootFile = "twister/boot.spin"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Spinor REPL (step12)"
  env <- loadBoot primitiveBindings
  loop env baseTypeEnv

-- | 起動時に boot.spin を読み込む (存在する場合)
--   boot.spin はマクロ定義等を含むため、型推論はスキップして評価のみ行う
loadBoot :: Env -> IO Env
loadBoot env = do
  exists <- doesFileExist bootFile
  if not exists
    then do
      putStrLn "(twister/boot.spin が見つかりません — スキップ)"
      pure env
    else do
      content <- TIO.readFile bootFile
      case parseFile content of
        Left err -> do
          putStrLn $ "boot.spin パースエラー: " ++ err
          pure env
        Right exprs -> do
          result <- runEval env (mapM_ expandAndEval exprs)
          case result of
            Left err -> do
              TIO.putStrLn $ "boot.spin 実行エラー: " <> err
              pure env
            Right (_, env') -> pure env'

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
                      result <- runEval env (expandAndEval ast)
                      case result of
                        Left err -> do
                          TIO.putStrLn $ "エラー: " <> err
                          loop env tyEnv'
                        Right (val, env') -> do
                          putStrLn (show val)
                          loop env' tyEnv'
