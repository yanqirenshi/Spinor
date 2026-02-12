{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF)
import Spinor.Syntax    (readExpr)
import Spinor.Eval      (Env, eval, runEval)
import Spinor.Primitive (primitiveBindings)
import Spinor.Val       ()  -- Show インスタンス

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Spinor REPL (step2)"
  loop primitiveBindings

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
              result <- runEval env (eval ast)
              case result of
                Left err -> do
                  TIO.putStrLn $ "エラー: " <> err
                  loop env
                Right (val, env') -> do
                  putStrLn (show val)
                  loop env'
