{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spinor.Core.Socket
  ( socketPrimitives
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Control.Exception (try, SomeException)
import System.IO.Unsafe (unsafePerformIO)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import Spinor.Val (Val(..))

-- | Socket 関連のプリミティブ関数群
socketPrimitives :: [(Text, Val)]
socketPrimitives =
  [ ("socket-listen",  VPrim "socket-listen"  primSocketListen)
  , ("socket-accept",  VPrim "socket-accept"  primSocketAccept)
  , ("socket-recv",    VPrim "socket-recv"    primSocketRecv)
  , ("socket-send",    VPrim "socket-send"    primSocketSend)
  , ("socket-close",   VPrim "socket-close"   primSocketClose)
  , ("socket-connect", VPrim "socket-connect" primSocketConnect)
  ]

-- | socket-listen: 指定ポートで TCP サーバーソケットを作成
--   (socket-listen port) -> Socket
primSocketListen :: [Val] -> Either Text Val
primSocketListen [VInt port] = unsafePerformIO $ do
  result <- try $ do
    -- TCP ソケットを作成
    sock <- socket AF_INET Stream defaultProtocol
    -- SO_REUSEADDR を設定 (ポート再利用)
    setSocketOption sock ReuseAddr 1
    -- 指定ポートにバインド
    bind sock (SockAddrInet (fromIntegral port) 0)  -- 0 = INADDR_ANY
    -- listen 状態に遷移 (backlog = 5)
    listen sock 5
    pure sock
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-listen: " <> T.pack (show e)
    Right sock ->
      pure $ Right $ VSocket sock
primSocketListen args =
  Left $ "socket-listen: 引数が不正です (port が必要): " <> T.pack (show (length args))

-- | socket-accept: クライアント接続を待ち受け
--   (socket-accept server-socket) -> (client-socket peer-address)
primSocketAccept :: [Val] -> Either Text Val
primSocketAccept [VSocket serverSock] = unsafePerformIO $ do
  result <- try $ do
    -- ブロッキングで接続を待ち受け
    (clientSock, peerAddr) <- accept serverSock
    pure (clientSock, peerAddr)
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-accept: " <> T.pack (show e)
    Right (clientSock, peerAddr) ->
      pure $ Right $ VList [VSocket clientSock, VStr (T.pack (show peerAddr))]
primSocketAccept [_] =
  Left "socket-accept: 引数はソケットである必要があります"
primSocketAccept args =
  Left $ "socket-accept: 引数が不正です (socket が必要): " <> T.pack (show (length args))

-- | socket-recv: ソケットからデータを受信
--   (socket-recv socket len) -> String
--   (socket-recv socket) -> String (デフォルト 4096 バイト)
primSocketRecv :: [Val] -> Either Text Val
primSocketRecv [VSocket sock, VInt len] = unsafePerformIO $ do
  result <- try $ do
    -- 指定バイト数まで受信
    bs <- recv sock (fromIntegral len)
    -- UTF-8 としてデコード (失敗時は Latin-1 としてデコード)
    pure $ case TE.decodeUtf8' bs of
      Right txt -> txt
      Left _ -> TE.decodeLatin1 bs
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-recv: " <> T.pack (show e)
    Right txt ->
      pure $ Right $ VStr txt
primSocketRecv [VSocket sock] =
  -- デフォルト 4096 バイト
  primSocketRecv [VSocket sock, VInt 4096]
primSocketRecv [_, _] =
  Left "socket-recv: 第1引数はソケット、第2引数は整数である必要があります"
primSocketRecv [_] =
  Left "socket-recv: 引数はソケットである必要があります"
primSocketRecv args =
  Left $ "socket-recv: 引数が不正です (socket [len] が必要): " <> T.pack (show (length args))

-- | socket-send: ソケットにデータを送信
--   (socket-send socket data) -> Int (送信バイト数)
primSocketSend :: [Val] -> Either Text Val
primSocketSend [VSocket sock, VStr txt] = unsafePerformIO $ do
  result <- try $ do
    let bs = TE.encodeUtf8 txt
    -- 全データを送信
    sendAll sock bs
    pure $ BS.length bs
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-send: " <> T.pack (show e)
    Right len ->
      pure $ Right $ VInt (fromIntegral len)
primSocketSend [_, _] =
  Left "socket-send: 第1引数はソケット、第2引数は文字列である必要があります"
primSocketSend args =
  Left $ "socket-send: 引数が不正です (socket data が必要): " <> T.pack (show (length args))

-- | socket-close: ソケットを閉じる
--   (socket-close socket) -> Bool
primSocketClose :: [Val] -> Either Text Val
primSocketClose [VSocket sock] = unsafePerformIO $ do
  result <- try $ do
    -- グレースフルにクローズ
    close sock
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-close: " <> T.pack (show e)
    Right () ->
      pure $ Right $ VBool True
primSocketClose [_] =
  Left "socket-close: 引数はソケットである必要があります"
primSocketClose args =
  Left $ "socket-close: 引数が不正です (socket が必要): " <> T.pack (show (length args))

-- | socket-connect: 指定ホスト:ポートに接続
--   (socket-connect host port) -> Socket
primSocketConnect :: [Val] -> Either Text Val
primSocketConnect [VStr host, VInt port] = unsafePerformIO $ do
  result <- try $ do
    -- アドレス解決
    let hints = defaultHints { addrSocketType = Stream }
    addrs <- getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
    case addrs of
      [] -> error "socket-connect: アドレス解決に失敗しました"
      (addr:_) -> do
        -- ソケット作成
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        -- 接続
        connect sock (addrAddress addr)
        pure sock
  case result of
    Left (e :: SomeException) ->
      pure $ Left $ "socket-connect: " <> T.pack (show e)
    Right sock ->
      pure $ Right $ VSocket sock
primSocketConnect [_, _] =
  Left "socket-connect: 第1引数は文字列 (host)、第2引数は整数 (port) である必要があります"
primSocketConnect args =
  Left $ "socket-connect: 引数が不正です (host port が必要): " <> T.pack (show (length args))
