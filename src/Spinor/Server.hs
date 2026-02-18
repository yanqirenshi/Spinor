{-# LANGUAGE OverloadedStrings #-}

module Spinor.Server (runServer) where

import Network.Socket
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Control.Exception (bracket, SomeException, catch)
import System.IO (Handle, IOMode(..), hSetBuffering, BufferMode(..), hClose, hSetBinaryMode)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Printf (printf)
import Numeric (readHex)

import Spinor.Syntax (Expr(..), readExpr)
import Spinor.Eval (runEval, eval, valToExpr)
import Spinor.Val (Env)
import Spinor.Expander (expand)

--------------------------------------------------------------------------------
-- Swank Protocol Layer
--------------------------------------------------------------------------------

-- | ヘッダを読み、ペイロード長を返す
recvHeader :: Handle -> IO (Maybe Int)
recvHeader h = do
    headerBytes <- BS.hGet h 6
    if BS.length headerBytes < 6
      then pure Nothing  -- 接続が閉じられた
      else case readHex (T.unpack $ decodeUtf8 headerBytes) of
            [(len, "")] -> pure (Just len)
            _           -> pure Nothing

-- | ペイロードを受信する
recvPayload :: Handle -> Int -> IO Text
recvPayload h len = decodeUtf8 <$> BS.hGet h len

-- | パケットを送信する (ヘッダ + ペイロード)
sendPacket :: Handle -> Text -> IO ()
sendPacket h payload = do
    let payloadBytes = encodeUtf8 payload
        len = BS.length payloadBytes
        header = encodeUtf8 $ T.pack $ printf "%06x" len
    BS.hPut h (header <> payloadBytes)

--------------------------------------------------------------------------------
-- Expr to Lisp S-expression String
--------------------------------------------------------------------------------

-- | Expr を Lisp 形式の文字列に変換する
exprToText :: Expr -> Text
exprToText (EInt n)   = T.pack (show n)
exprToText (EBool True)  = "t"
exprToText (EBool False) = "nil"
exprToText (ESym s)   = s
exprToText (EStr s)   = "\"" <> escapeString s <> "\""
exprToText (EList []) = "nil"
exprToText (EList xs) = "(" <> T.intercalate " " (map exprToText xs) <> ")"
exprToText (ELet _ _) = "<let>"
exprToText (EData _ _) = "<data>"
exprToText (EMatch _ _) = "<match>"
exprToText (EModule _ _) = "<module>"
exprToText (EImport _ _) = "<import>"

-- | 文字列内の特殊文字をエスケープする
escapeString :: Text -> Text
escapeString = T.concatMap escapeChar
  where
    escapeChar '"'  = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c    = T.singleton c

--------------------------------------------------------------------------------
-- Swank RPC Dispatcher
--------------------------------------------------------------------------------

-- | Swank RPC リクエストを処理する
handleSwankRequest :: Handle -> Env -> Expr -> Integer -> IO Env
handleSwankRequest h env form reqId = case form of
    -- swank:connection-info - ハンドシェイク
    EList [ESym "swank:connection-info"] -> do
        let response = mkConnectionInfoResponse reqId
        sendPacket h (exprToText response)
        pure env

    -- swank:swank-require - 無視して成功を返す
    EList (ESym "swank:swank-require" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:create-repl - REPL 作成
    EList (ESym "swank:create-repl" : _) -> do
        let response = mkOkResponse reqId (EList [EStr "user", EStr "SPINOR>"])
        sendPacket h (exprToText response)
        pure env

    -- swank:listener-eval - S式の評価
    EList [ESym "swank:listener-eval", EStr code] -> do
        evalAndRespond h env code reqId

    -- swank:interactive-eval - S式の評価
    EList [ESym "swank:interactive-eval", EStr code] -> do
        evalAndRespond h env code reqId

    -- swank:compile-string-for-emacs - バッファからのコンパイル
    EList (ESym "swank:compile-string-for-emacs" : EStr code : _) -> do
        compileAndRespond h env code reqId

    -- swank:autodoc - 自動ドキュメント (簡易実装)
    EList (ESym "swank:autodoc" : _) -> do
        let response = mkOkResponse reqId (EList [EStr ":not-available", ESym "t"])
        sendPacket h (exprToText response)
        pure env

    -- swank:operator-arglist - 引数リスト (簡易実装)
    EList (ESym "swank:operator-arglist" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- その他 - 未実装エラー
    _ -> do
        let errMsg = "Unknown swank command: " <> exprToText form
            response = mkAbortResponse reqId errMsg
        sendPacket h (exprToText response)
        pure env

-- | S式を評価してレスポンスを送信する
evalAndRespond :: Handle -> Env -> Text -> Integer -> IO Env
evalAndRespond h env code reqId =
    case readExpr code of
        Left err -> do
            let response = mkAbortResponse reqId (T.pack err)
            sendPacket h (exprToText response)
            pure env
        Right ast -> do
            result <- runEval env (expand ast >>= eval)
            case result of
                Left err -> do
                    let response = mkAbortResponse reqId err
                    sendPacket h (exprToText response)
                    pure env
                Right (val, env') -> do
                    let resultExpr = valToExpr val
                        -- SLY は結果を文字列として表示するため、文字列に変換
                        resultStr = exprToText resultExpr
                        response = mkOkResponse reqId (EStr resultStr)
                    sendPacket h (exprToText response)
                    pure env'

-- | S式をコンパイル（評価）して固定の成功メッセージを返す
compileAndRespond :: Handle -> Env -> Text -> Integer -> IO Env
compileAndRespond h env code reqId =
    case readExpr code of
        Left err -> do
            let response = mkAbortResponse reqId (T.pack err)
            sendPacket h (exprToText response)
            pure env
        Right ast -> do
            result <- runEval env (expand ast >>= eval)
            case result of
                Left err -> do
                    let response = mkAbortResponse reqId err
                    sendPacket h (exprToText response)
                    pure env
                Right (_, env') -> do
                    -- 成功時は固定の成功メッセージを返す
                    let successMsg = EList [EStr "Compilation finished.", ESym "t"]
                        response = mkOkResponse reqId successMsg
                    sendPacket h (exprToText response)
                    pure env'

--------------------------------------------------------------------------------
-- Response Builders
--------------------------------------------------------------------------------

-- | :return (:ok result) request-id
mkOkResponse :: Integer -> Expr -> Expr
mkOkResponse reqId result =
    EList [ESym ":return", EList [ESym ":ok", result], EInt reqId]

-- | :return (:abort message) request-id
mkAbortResponse :: Integer -> Text -> Expr
mkAbortResponse reqId msg =
    EList [ESym ":return", EList [ESym ":abort", EStr msg], EInt reqId]

-- | connection-info レスポンスを生成する
mkConnectionInfoResponse :: Integer -> Expr
mkConnectionInfoResponse reqId =
    EList
        [ ESym ":return"
        , EList
            [ ESym ":ok"
            , EList
                [ ESym ":pid", EInt 0
                , ESym ":style", ESym ":spawn"
                , ESym ":encoding", EList [ESym ":coding-systems", EList [EStr "utf-8-unix"]]
                , ESym ":lisp-implementation"
                , EList
                    [ ESym ":type", EStr "Spinor"
                    , ESym ":version", EStr "0.1.0"
                    , ESym ":program", EStr "spinor"
                    , ESym ":package", EList [ESym ":name", EStr "user", ESym ":prompt", EStr "SPINOR>"]
                    ]
                , ESym ":version", EStr "2.27"
                ]
            ]
        , EInt reqId
        ]

--------------------------------------------------------------------------------
-- TCP Server
--------------------------------------------------------------------------------

-- | TCP サーバーを起動する
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
        putStrLn $ "Swank server listening on port " ++ port
        pure sock

    acceptLoop sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected: " ++ show peer
        void $ forkFinally (handleClient conn initialEnv) (\_ -> close conn)

-- | クライアント接続を処理する
handleClient :: Socket -> Env -> IO ()
handleClient sock env = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    hSetBinaryMode h True
    clientLoop h env `catch` handleDisconnect h
  where
    handleDisconnect :: Handle -> SomeException -> IO ()
    handleDisconnect hdl _ = do
        putStrLn "Client disconnected."
        hClose hdl

-- | クライアントとの Swank プロトコルループ
clientLoop :: Handle -> Env -> IO ()
clientLoop h env = do
    mLen <- recvHeader h
    case mLen of
        Nothing -> putStrLn "Client disconnected."
        Just len -> do
            payload <- recvPayload h len
            case readExpr payload of
                Left err -> do
                    putStrLn $ "Parse error: " ++ err
                    clientLoop h env
                Right expr -> do
                    env' <- dispatchRPC h env expr
                    clientLoop h env'

-- | RPC ディスパッチ
dispatchRPC :: Handle -> Env -> Expr -> IO Env
dispatchRPC h env expr = case expr of
    -- (:emacs-rex form package thread-id request-id)
    EList [ESym ":emacs-rex", form, _package, _threadId, EInt reqId] ->
        handleSwankRequest h env form reqId

    -- その他のフォーマット
    _ -> do
        putStrLn $ "Unknown RPC format: " ++ show expr
        pure env
