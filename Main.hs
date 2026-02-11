{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF)
import Syntax (readExpr)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  putStrLn "Spinor REPL (step1)"
  loop

loop :: IO ()
loop = do
  putStr "spinor> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof
    then putStrLn "\nBye."
    else do
      line <- TIO.getLine
      if T.null (T.strip line)
        then loop
        else do
          case readExpr line of
            Left  err -> putStrLn err
            Right ast -> print ast
          loop
