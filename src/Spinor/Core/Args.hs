{-# LANGUAGE OverloadedStrings #-}

module Spinor.Core.Args
  ( setScriptArgs
  , getScriptArgs
  , argsPrimitives
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

import Spinor.Val (Val(..))

-- | グローバルな引数格納用 IORef
--   スクリプト実行前に Main.hs から設定される
{-# NOINLINE scriptArgsRef #-}
scriptArgsRef :: IORef [Text]
scriptArgsRef = unsafePerformIO (newIORef [])

-- | スクリプト引数を設定する (Main.hs から呼び出し)
setScriptArgs :: [String] -> IO ()
setScriptArgs args = writeIORef scriptArgsRef (map T.pack args)

-- | スクリプト引数を取得する
getScriptArgs :: IO [Text]
getScriptArgs = readIORef scriptArgsRef

-- | 引数関連のプリミティブ
argsPrimitives :: [(Text, Val)]
argsPrimitives =
  [ ("command-line-args", VPrim "command-line-args" primCommandLineArgs)
  ]

-- | (command-line-args) -> 文字列のリスト
--   スクリプトに渡された引数のみを返す（spinor コマンドやファイル名は除外済み）
primCommandLineArgs :: [Val] -> Either Text Val
primCommandLineArgs [] = unsafePerformIO $ do
  args <- getScriptArgs
  pure $ Right $ VList (map VStr args)
primCommandLineArgs args =
  Left $ "command-line-args: 引数は不要です (実際: " <> T.pack (show (length args)) <> ")"
