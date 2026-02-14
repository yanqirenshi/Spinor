{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF)
import System.Directory (doesFileExist)
import Spinor.Syntax    (readExpr, parseFile)
import Spinor.Val       (Env)
import Spinor.Eval      (runEval)
import Spinor.Expander  (expandAndEval)
import Spinor.Primitive (primitiveBindings)

-- | ブートファイルのパス
bootFile :: FilePath
bootFile = "twister/boot.spin"

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Spinor REPL (step9)"
  env <- loadBoot primitiveBindings
  loop env

-- | 起動時に boot.spin を読み込む (存在する場合)
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

loop :: Env -> IO ()
loop env = do
  putStr "spinor> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof
    then putStrLn "\nBye."
    else do
      line <- TIO.getLine
      if T.null (T.strip line)
        then loop env
        else do
          case readExpr line of
            Left err -> do
              putStrLn err
              loop env
            Right ast -> do
              result <- runEval env (expandAndEval ast)
              case result of
                Left err -> do
                  TIO.putStrLn $ "エラー: " <> err
                  loop env
                Right (val, env') -> do
                  putStrLn (show val)
                  loop env'
