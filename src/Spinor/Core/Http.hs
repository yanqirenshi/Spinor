{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spinor.Core.Http
  ( httpPrimitives
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)

import Spinor.Val (Val(..))

-- | HTTP 関連のプリミティブ関数群
httpPrimitives :: [(Text, Val)]
httpPrimitives =
  [ ("core-http-request", VPrim "core-http-request" coreHttpRequest)
  ]

-- | 低レベル HTTP リクエストプリミティブ
--   引数: (method url headers body)
--   - method: "GET", "POST" 等の文字列
--   - url: リクエスト先 URL
--   - headers: ((:header-name "value") ...) 形式の Alist または nil
--   - body: POST 等で送信するボディ (文字列または nil)
--   戻り値: ((:status-code . 200) (:headers . (...)) (:body . "..."))
coreHttpRequest :: [Val] -> Either Text Val
coreHttpRequest [VStr method, VStr url, headers, body] =
  unsafePerformHttpRequest method url headers body
coreHttpRequest args =
  Left $ "core-http-request: 引数が不正です (method url headers body が必要): " <> T.pack (show (length args))

-- | HTTP リクエストを実行する (unsafePerformIO を使用)
unsafePerformHttpRequest :: Text -> Text -> Val -> Val -> Either Text Val
unsafePerformHttpRequest method url headers body =
  unsafePerformIO (performHttpRequest method url headers body)

-- | curl を使用して HTTP リクエストを実行
performHttpRequest :: Text -> Text -> Val -> Val -> IO (Either Text Val)
performHttpRequest method url headers body = do
  result <- try $ do
    -- curl コマンドを構築
    let headerArgs = buildHeaderArgs headers
        bodyArgs = buildBodyArgs body
        -- -w でステータスコードを末尾に出力、-s でプログレスを抑制
        curlArgs = ["-s", "-w", "\n%{http_code}"]
                   ++ ["-X", T.unpack method]
                   ++ headerArgs
                   ++ bodyArgs
                   ++ [T.unpack url]

    -- curl を実行
    (exitCode, stdout, stderr) <- readProcessWithExitCode "curl" curlArgs ""

    case exitCode of
      ExitSuccess -> do
        -- 最後の行がステータスコード
        let outputLines = lines stdout
        case reverse outputLines of
          (statusLine:bodyLines) -> do
            let statusCode = fromMaybe 0 (readMaybe statusLine :: Maybe Int)
                respBody = unlines (reverse bodyLines)
            pure $ VList
              [ VList [VSym ":status-code", VInt (fromIntegral statusCode)]
              , VList [VSym ":headers", VNil]  -- curl では詳細ヘッダー取得は複雑なため省略
              , VList [VSym ":body", VStr (T.pack respBody)]
              ]
          [] -> pure $ VList
                  [ VList [VSym ":status-code", VInt 0]
                  , VList [VSym ":headers", VNil]
                  , VList [VSym ":body", VStr ""]
                  ]
      ExitFailure code ->
        error $ "curl failed with exit code " ++ show code ++ ": " ++ stderr

  case result of
    Left (e :: SomeException) -> pure $ Left $ "HTTP エラー: " <> T.pack (show e)
    Right val -> pure $ Right val

-- | ヘッダー引数を構築
buildHeaderArgs :: Val -> [String]
buildHeaderArgs VNil = []
buildHeaderArgs (VList pairs) = concatMap buildHeaderArg pairs
buildHeaderArgs _ = []

-- | 単一のヘッダー引数を構築
buildHeaderArg :: Val -> [String]
buildHeaderArg (VList [VSym name, VStr value])
  | T.isPrefixOf ":" name =
      ["-H", T.unpack (T.drop 1 name) ++ ": " ++ T.unpack value]
  | otherwise =
      ["-H", T.unpack name ++ ": " ++ T.unpack value]
buildHeaderArg (VList [VStr name, VStr value]) =
  ["-H", T.unpack name ++ ": " ++ T.unpack value]
buildHeaderArg _ = []

-- | ボディ引数を構築
buildBodyArgs :: Val -> [String]
buildBodyArgs (VStr b)
  | not (T.null b) = ["-d", T.unpack b]
  | otherwise = []
buildBodyArgs _ = []
