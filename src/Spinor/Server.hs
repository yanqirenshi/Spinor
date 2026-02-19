{-# LANGUAGE OverloadedStrings #-}

module Spinor.Server (runServer) where

import Network.Socket
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Control.Exception (bracket, SomeException, catch)
import Data.List (nub)
import System.IO (Handle, IOMode(..), hSetBuffering, BufferMode(..), hClose, hSetBinaryMode)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Text.Printf (printf)
import Numeric (readHex)

import Spinor.Syntax (Expr(..), readExpr)
import Spinor.Eval (runEval, eval, valToExpr)
import Spinor.Val (Env, Val(..))
import Spinor.Expander (expand)
import Spinor.Lsp.Docs (primitiveDocs, DocEntry(..))
import qualified Data.Map.Strict as Map

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
-- Swank/Slynk Command Normalization
--------------------------------------------------------------------------------

-- | コマンド名を正規化する (slynk: -> swank:)
-- SLY は slynk: プレフィックスを使用するが、SLIME は swank: を使用する
-- 両方をサポートするため、slynk: を swank: に変換する
-- 例: slynk:connection-info -> swank:connection-info
--     slynk:slynk-add-load-paths -> swank:swank-add-load-paths
--     slynk-mrepl:create-mrepl -> swank:mrepl:create-mrepl
--     slynk-completion:flex-completions -> swank:completion:flex-completions
normalizeCommand :: Text -> Text
normalizeCommand cmd
    | "slynk-completion:" `T.isPrefixOf` cmd = "swank:completion:" <> T.drop 17 cmd
    | "slynk-mrepl:" `T.isPrefixOf` cmd = "swank:mrepl:" <> T.drop 12 cmd
    | "slynk:slynk-" `T.isPrefixOf` cmd = "swank:swank-" <> T.drop 12 cmd
    | "slynk:" `T.isPrefixOf` cmd       = "swank:" <> T.drop 6 cmd
    | otherwise                         = cmd

-- | Expr 内のコマンドシンボルを正規化する
normalizeForm :: Expr -> Expr
normalizeForm (EList (ESym cmd : rest)) = EList (ESym (normalizeCommand cmd) : rest)
normalizeForm other = other

--------------------------------------------------------------------------------
-- Swank RPC Dispatcher
--------------------------------------------------------------------------------

-- | Swank RPC リクエストを処理する
handleSwankRequest :: Handle -> Env -> Expr -> Integer -> IO Env
handleSwankRequest h env form reqId = case normalizeForm form of
    -- swank:connection-info - ハンドシェイク
    EList [ESym "swank:connection-info"] -> do
        let response = mkConnectionInfoResponse reqId
        sendPacket h (exprToText response)
        pure env

    -- swank:swank-require - モジュールロード
    -- モジュール名を大文字に変換して返す (Common Lisp の慣例)
    EList (ESym "swank:swank-require" : args) -> do
        let loadedModules = extractModules args
            response = mkOkResponse reqId (EList loadedModules)
        sendPacket h (exprToText response)
        pure env
      where
        extractModules [EList [ESym "quote", EList modules]] = map toUpperModule modules
        extractModules [EList modules] = map toUpperModule modules
        extractModules modules = map toUpperModule modules
        toUpperModule (EStr name) = EStr (T.toUpper name)
        toUpperModule (ESym name) = EStr (T.toUpper name)
        toUpperModule other = other

    -- swank:swank-add-load-paths - ロードパス追加（無視して成功を返す）
    EList (ESym "swank:swank-add-load-paths" : _) -> do
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

    -- swank:autodoc - 自動ドキュメント
    EList (ESym "swank:autodoc" : args) -> do
        let rawForms = extractAutodocForms args
            result = getAutodocInfo env rawForms
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env
      where
        extractAutodocForms (EList [ESym "quote", EList forms] : _) = forms
        extractAutodocForms (EList forms : _) = forms
        extractAutodocForms _ = []

    -- swank:operator-arglist - 引数リスト
    EList [ESym "swank:operator-arglist", EStr opName, _pkg] -> do
        let result = getArglist env opName
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:operator-arglist (その他の形式)
    EList (ESym "swank:operator-arglist" : _) -> do
        let response = mkOkResponse reqId (EStr "")
        sendPacket h (exprToText response)
        pure env

    -- slynk-mrepl:create-mrepl - MREPL作成
    EList (ESym "swank:mrepl:create-mrepl" : _) -> do
        -- チャンネルID 1 でREPLを作成
        let response = mkOkResponse reqId (EList [EInt 1, ESym ":prompt", EStr "SPINOR>"])
        sendPacket h (exprToText response)
        pure env

    -- slynk:buffer-first-change - バッファ変更通知 (無視して成功を返す)
    EList (ESym "swank:buffer-first-change" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:completions - 補完候補を返す
    EList [ESym "swank:completions", EStr prefix, _pkg] -> do
        let completions = getCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:completions - その他の形式
    EList (ESym "swank:completions" : _) -> do
        let response = mkOkResponse reqId (EList [EList [], EStr ""])
        sendPacket h (exprToText response)
        pure env

    -- swank:simple-completions - シンプルな補完候補
    EList [ESym "swank:simple-completions", EStr prefix, _pkg] -> do
        let completions = getSimpleCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:simple-completions - その他の形式
    EList (ESym "swank:simple-completions" : _) -> do
        let response = mkOkResponse reqId (EList [EList [], EList []])
        sendPacket h (exprToText response)
        pure env

    -- swank:completions-for-character - 文字の補完
    EList (ESym "swank:completions-for-character" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:completion:flex-completions - Flex 補完 (SLY の補完機能)
    EList [ESym "swank:completion:flex-completions", EStr prefix, _pkg] -> do
        let completions = getFlexCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:completion:flex-completions - その他の形式
    EList (ESym "swank:completion:flex-completions" : _) -> do
        let response = mkOkResponse reqId (EList [EList [], EList []])
        sendPacket h (exprToText response)
        pure env

    -- その他 - 未実装コマンド
    _ -> do
        let errMsg = "Unknown swank command: " <> exprToText form
            response = mkAbortResponse reqId errMsg
        sendPacket h (exprToText response)
        pure env

--------------------------------------------------------------------------------
-- Autodoc Helpers
--------------------------------------------------------------------------------

-- | autodoc 用の関数情報を取得する
-- 1. まず primitiveDocs から組み込み関数のシグネチャを探す
-- 2. なければ実行時環境 (Env) からユーザー定義関数を探す
getAutodocInfo :: Env -> [Expr] -> Expr
getAutodocInfo env forms =
    case extractFunctionName forms of
        Nothing -> EList [ESym ":not-available", ESym "t"]
        Just funcName ->
            -- まず primitiveDocs を検索
            case Map.lookup funcName primitiveDocs of
                Just docEntry ->
                    let argStr = "(" <> funcName <> " " <> docSignature docEntry <> ")"
                    in EList [EStr argStr, ESym "t"]
                Nothing ->
                    -- primitiveDocs になければ Env を検索
                    case Map.lookup funcName env of
                        Just (VFunc args _ _) ->
                            let argStr = "(" <> funcName <> " " <> T.intercalate " " args <> ")"
                            in EList [EStr argStr, ESym "t"]
                        Just (VPrim name _) ->
                            let argStr = "(" <> name <> " ...)"
                            in EList [EStr argStr, ESym "t"]
                        Just (VMacro args _ _) ->
                            let argStr = "(" <> funcName <> " " <> T.intercalate " " args <> ")"
                            in EList [EStr argStr, ESym "t"]
                        _ -> EList [ESym ":not-available", ESym "t"]

-- | autodoc フォームから関数名を抽出する
extractFunctionName :: [Expr] -> Maybe Text
extractFunctionName [] = Nothing
extractFunctionName (EList (ESym name : _) : _) = Just name
extractFunctionName (ESym name : _) = Just name
extractFunctionName (EStr name : _) = Just name
extractFunctionName (_ : rest) = extractFunctionName rest

-- | operator-arglist 用の引数リストを取得する
getArglist :: Env -> Text -> Expr
getArglist env funcName =
    -- まず primitiveDocs を検索
    case Map.lookup funcName primitiveDocs of
        Just docEntry -> EStr $ "(" <> docSignature docEntry <> ")"
        Nothing ->
            -- primitiveDocs になければ Env を検索
            case Map.lookup funcName env of
                Just (VFunc args _ _) ->
                    let argStr = "(" <> T.intercalate " " args <> ")"
                    in EStr argStr
                Just (VMacro args _ _) ->
                    let argStr = "(" <> T.intercalate " " args <> ")"
                    in EStr argStr
                Just (VPrim _ _) -> EStr "(...)"
                _ -> EStr ""

--------------------------------------------------------------------------------
-- Completion Helpers
--------------------------------------------------------------------------------

-- | 補完候補を取得する (swank:completions 形式)
-- 戻り値: ((候補リスト) 共通プレフィックス)
getCompletions :: Env -> Text -> Expr
getCompletions env prefix =
    let -- primitiveDocs からの候補
        primNames = Map.keys primitiveDocs
        -- Env からの候補
        envNames = Map.keys env
        -- 全候補からプレフィックスでフィルタ
        allNames = primNames ++ envNames
        matches = filter (prefix `T.isPrefixOf`) allNames
        -- 補完アイテム形式: (name signature)
        items = map mkCompletionItem matches
        -- 共通プレフィックスを計算
        commonPrefix = if null matches then prefix else findCommonPrefix matches
    in EList [EList items, EStr commonPrefix]
  where
    mkCompletionItem name =
        case Map.lookup name primitiveDocs of
            Just doc -> EList [EStr name, EStr (docSignature doc)]
            Nothing -> EList [EStr name, EStr ""]

-- | シンプルな補完候補を取得する (swank:simple-completions 形式)
-- 戻り値: ((候補文字列リスト) (パッケージリスト))
getSimpleCompletions :: Env -> Text -> Expr
getSimpleCompletions env prefix =
    let primNames = Map.keys primitiveDocs
        envNames = Map.keys env
        allNames = primNames ++ envNames
        matches = filter (prefix `T.isPrefixOf`) allNames
    in EList [EList (map EStr matches), EList []]

-- | 文字列リストの共通プレフィックスを見つける
findCommonPrefix :: [Text] -> Text
findCommonPrefix [] = ""
findCommonPrefix [x] = x
findCommonPrefix (x:xs) = foldl commonPrefix2 x xs
  where
    commonPrefix2 a b = T.pack $ map fst $ takeWhile (uncurry (==)) $ T.zip a b

-- | Flex 補完候補を取得する (slynk-completion:flex-completions 形式)
-- 戻り値: ((補完アイテムリスト) nil)
-- 各アイテム: (name score chunks classification)
getFlexCompletions :: Env -> Text -> Expr
getFlexCompletions env prefix =
    let primNames = Map.keys primitiveDocs
        envNames = Map.keys env
        -- 重複を除去 (Data.List.nub を使用)
        allNames = nub (primNames ++ envNames)
        -- Flex マッチング (プレフィックス + 部分文字列)
        matches = filter (flexMatch prefix) allNames
        items = map mkFlexItem matches
    in EList [EList items, EList []]
  where
    -- 簡易 flex マッチ: プレフィックスまたは部分文字列
    flexMatch pat name = pat `T.isPrefixOf` name || pat `T.isInfixOf` name
    -- Flex 補完アイテム: (name score chunks classification)
    mkFlexItem name =
        let score = if prefix `T.isPrefixOf` name then 100 else 50
            classification = case Map.lookup name primitiveDocs of
                Just _ -> ":function"
                Nothing -> case Map.lookup name env of
                    Just (VFunc _ _ _) -> ":function"
                    Just (VMacro _ _ _) -> ":macro"
                    _ -> ":variable"
        in EList
            [ EStr name           -- 補完テキスト
            , EInt score          -- スコア
            , EList []            -- chunks (マッチ位置、省略)
            , EStr classification -- 分類
            ]

--------------------------------------------------------------------------------
-- Eval/Compile Handlers
--------------------------------------------------------------------------------

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
                    , ESym ":name", EStr "spinor"
                    ]
                , ESym ":package"
                , EList [ESym ":name", EStr "user", ESym ":prompt", EStr "SPINOR>"]
                , ESym ":version", EStr "2.27"
                -- サポートするモジュールを事前に登録
                , ESym ":modules"
                , EList
                    [ EStr "SLYNK/ARGLISTS"
                    , EStr "SLYNK/FANCY-INSPECTOR"
                    , EStr "SLYNK/PACKAGE-FU"
                    , EStr "SLYNK/MREPL"
                    , EStr "SLYNK/TRACE-DIALOG"
                    , EStr "SLYNK/STICKERS"
                    , EStr "SLYNK/INDENTATION"
                    , EStr "SLYNK/RETRO"
                    ]
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
