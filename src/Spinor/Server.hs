{-# LANGUAGE OverloadedStrings #-}

module Spinor.Server (runServer) where

import Network.Socket
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Control.Exception (bracket, SomeException, catch)
import System.IO (Handle, IOMode(..), hSetBuffering, hGetLine, hPutStrLn, BufferMode(..), hClose, hSetEncoding, utf8)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Spinor.Syntax (readExpr)
import Spinor.Eval (runEval, eval)
import Spinor.Val (Env, showVal)
import Spinor.Expander (expand)

-- | TCP サーバーを起動する
--   指定されたポートで待ち受け、クライアントからの S式 を評価して結果を返す
runServer :: String -> Env -> IO ()
runServer port initialEnv = do
    addr <- resolve
    bracket (open addr) close acceptLoop
  where
    resolve = do
        let hints = defaultHints
              { addrFlags = [AI_PASSIVE]
              , addrSocketType = Stream
              }
        head <$> getAddrInfo (Just hints) Nothing (Just port)

    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 5
        pure sock

    acceptLoop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected: " ++ show peer
        void $ forkFinally (handleClient conn initialEnv) (\_ -> close conn)

-- | クライアント接続を処理する
--   ソケットを Handle に変換し、REPL ループを実行する
handleClient :: Socket -> Env -> IO ()
handleClient sock env = do
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle LineBuffering
    hSetEncoding handle utf8
    clientLoop handle env `catch` handleDisconnect handle
  where
    handleDisconnect :: Handle -> SomeException -> IO ()
    handleDisconnect h _ = do
        putStrLn "Client disconnected."
        hClose h

-- | クライアントとの REPL ループ
--   一行ずつ読み取り、評価して結果を返す
clientLoop :: Handle -> Env -> IO ()
clientLoop handle env = do
    line <- hGetLine handle
    let input = T.pack line
    if T.null (T.strip input)
      then clientLoop handle env
      else case readExpr input of
        Left err -> do
            hPutStrLn handle $ "パースエラー: " ++ err
            clientLoop handle env
        Right ast -> do
            -- マクロ展開 → 評価
            result <- runEval env (expand ast >>= eval)
            case result of
              Left err -> do
                  TIO.hPutStrLn handle $ "エラー: " <> err
                  clientLoop handle env
              Right (val, env') -> do
                  hPutStrLn handle (showVal val)
                  clientLoop handle env'
