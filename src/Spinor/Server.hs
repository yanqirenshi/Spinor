{-# LANGUAGE OverloadedStrings #-}

module Spinor.Server (runServer) where

import Network.Socket
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Control.Exception (bracket, SomeException, catch)
import Data.List (nub)
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
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
-- Trace State
--------------------------------------------------------------------------------

-- | トレース対象の関数名を保持するセット
type TracedFunctions = IORef (Set Text)

-- | 新しいトレース状態を作成する
newTracedFunctions :: IO TracedFunctions
newTracedFunctions = newIORef Set.empty

-- | 関数をトレース対象に追加する
addTracedFunction :: TracedFunctions -> Text -> IO ()
addTracedFunction ref name = modifyIORef' ref (Set.insert name)

-- | 関数をトレース対象から削除する
removeTracedFunction :: TracedFunctions -> Text -> IO ()
removeTracedFunction ref name = modifyIORef' ref (Set.delete name)

-- | 関数がトレース対象かどうかを確認する
isTraced :: TracedFunctions -> Text -> IO Bool
isTraced ref name = Set.member name <$> readIORef ref

-- | トレース対象の関数一覧を取得する
getTracedFunctions :: TracedFunctions -> IO [Text]
getTracedFunctions ref = Set.toList <$> readIORef ref

-- | 全てのトレースをクリアする
clearAllTraces :: TracedFunctions -> IO ()
clearAllTraces ref = writeIORef ref Set.empty

-- | トレース対象の関数名を Expr から抽出する
-- SLY は (slynk::from-string "name") という形式で送信する
extractTraceSpec :: Expr -> Text
extractTraceSpec (EStr s) = s
extractTraceSpec (ESym s) = s
extractTraceSpec (EList [ESym "swank::from-string", EStr s]) = s
extractTraceSpec (EList [ESym "slynk::from-string", EStr s]) = s
extractTraceSpec (EList [ESym _, EStr s]) = s  -- その他の from-string 形式
extractTraceSpec _ = ""

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
--     slynk-trace-dialog:dialog-toggle-trace -> swank:trace:dialog-toggle-trace
normalizeCommand :: Text -> Text
normalizeCommand cmd
    | "slynk-completion:" `T.isPrefixOf` cmd = "swank:completion:" <> T.drop 17 cmd
    | "slynk-mrepl:" `T.isPrefixOf` cmd = "swank:mrepl:" <> T.drop 12 cmd
    | "slynk-trace-dialog:" `T.isPrefixOf` cmd = "swank:trace:" <> T.drop 19 cmd
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
handleSwankRequest :: Handle -> Env -> TracedFunctions -> Expr -> Integer -> IO Env
handleSwankRequest h env tracedFns form reqId = case normalizeForm form of
    -- swank:connection-info - ハンドシェイク
    EList [ESym "swank:connection-info"] -> do
        let response = mkConnectionInfoResponse reqId
        sendPacket h (exprToText response)
        -- インデント情報を送信
        sendPacket h (exprToText mkIndentationUpdate)
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
    -- 引数: (create-mrepl channel-id)
    -- 戻り値: (channel-id thread-id)
    EList [ESym "swank:mrepl:create-mrepl", EInt channelId] -> do
        -- チャンネル作成成功を返す (channel-id, thread-id)
        let response = mkOkResponse reqId (EList [EInt channelId, EInt 0])
        sendPacket h (exprToText response)
        -- 初期プロンプトを送信
        sendMreplPrompt h channelId
        pure env

    -- slynk-mrepl:create-mrepl - その他の形式
    EList (ESym "swank:mrepl:create-mrepl" : _) -> do
        -- デフォルトでチャンネルID 1 を使用
        let response = mkOkResponse reqId (EList [EInt 1, EInt 0])
        sendPacket h (exprToText response)
        sendMreplPrompt h 1
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

    -- swank:eval-for-inspector - インスペクタコンテキストで評価
    -- 形式: (slynk:eval-for-inspector inspector-id thread-id (quote slynk:func) args...)
    EList (ESym "swank:eval-for-inspector" : _inspId : _threadId : EList [ESym "quote", ESym func] : args) -> do
        let normalizedFunc = normalizeCommand func
        case (normalizedFunc, args) of
            ("swank:init-inspector", [EStr code]) -> do
                result <- inspectValue env code
                let response = mkOkResponse reqId result
                sendPacket h (exprToText response)
                pure env
            _ -> do
                -- 未対応の inspector 関数
                let response = mkOkResponse reqId (EList [])
                sendPacket h (exprToText response)
                pure env

    -- swank:eval-for-inspector - その他の形式
    EList (ESym "swank:eval-for-inspector" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:init-inspector - 直接呼び出し形式
    EList [ESym "swank:init-inspector", EStr code] -> do
        result <- inspectValue env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:init-inspector - その他の形式
    EList (ESym "swank:init-inspector" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspect-nth-part - インスペクタの要素を検査
    EList [ESym "swank:inspect-nth-part", EInt _partId] -> do
        -- 簡易実装: 詳細な部品検査は未サポート
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-pop - 前の検査対象に戻る
    EList [ESym "swank:inspector-pop"] -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-next - 次の検査対象に進む
    EList [ESym "swank:inspector-next"] -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-reinspect - 現在のオブジェクトを再検査
    EList [ESym "swank:inspector-reinspect"] -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-range - インスペクタの範囲取得
    EList (ESym "swank:inspector-range" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-call-nth-action - アクション実行
    EList (ESym "swank:inspector-call-nth-action" : _) -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    -- swank:quit-inspector - インスペクタを閉じる
    EList [ESym "swank:quit-inspector"] -> do
        let response = mkOkResponse reqId (EList [])
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Trace Dialog Handlers
    --------------------------------------------------------------------------------

    -- swank:trace:dialog-toggle-trace - トレースのトグル
    -- 形式: (dialog-toggle-trace (slynk::from-string "name")) または (dialog-toggle-trace "name")
    EList [ESym "swank:trace:dialog-toggle-trace", specExpr] -> do
        let spec = extractTraceSpec specExpr
        traced <- isTraced tracedFns spec
        if traced
            then removeTracedFunction tracedFns spec
            else addTracedFunction tracedFns spec
        let newState = not traced
            -- SLY expects a string message about what happened
            msg = if newState
                  then spec <> " is now traced for trace dialog"
                  else spec <> " is now untraced for trace dialog"
            response = mkOkResponse reqId (EStr msg)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-traced-p - トレース状態を確認
    EList [ESym "swank:trace:dialog-traced-p", specExpr] -> do
        let spec = extractTraceSpec specExpr
        traced <- isTraced tracedFns spec
        let response = mkOkResponse reqId (EBool traced)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-untrace - トレースを解除
    EList [ESym "swank:trace:dialog-untrace", specExpr] -> do
        let spec = extractTraceSpec specExpr
        removeTracedFunction tracedFns spec
        let response = mkOkResponse reqId (EBool True)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-untrace-all - 全トレースを解除
    EList [ESym "swank:trace:dialog-untrace-all"] -> do
        tracedList <- getTracedFunctions tracedFns
        clearAllTraces tracedFns
        let response = mkOkResponse reqId (EList [EInt (fromIntegral $ length tracedList), EInt 0])
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-specs - トレース対象の関数一覧を返す
    EList [ESym "swank:trace:report-specs"] -> do
        tracedList <- getTracedFunctions tracedFns
        -- 各関数を (name . details) の形式で返す
        let specs = map (\name -> EList [EStr name]) tracedList
            response = mkOkResponse reqId (EList specs)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-total - トレース総数を返す
    EList [ESym "swank:trace:report-total"] -> do
        -- 現時点ではトレースエントリを保持していないので 0 を返す
        let response = mkOkResponse reqId (EInt 0)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-partial-tree - トレースツリーの一部を返す
    EList (ESym "swank:trace:report-partial-tree" : _) -> do
        -- 空のツリーを返す: (traces . remaining)
        let response = mkOkResponse reqId (EList [EList [], EInt 0])
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:clear-trace-tree - トレースツリーをクリア
    EList [ESym "swank:trace:clear-trace-tree"] -> do
        let response = mkOkResponse reqId (EBool True)
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
-- Inspector Helpers
--------------------------------------------------------------------------------

-- | 値を検査してインスペクタ形式で返す
inspectValue :: Env -> Text -> IO Expr
inspectValue env code =
    case readExpr code of
        Left err -> pure $ mkInspectorError (T.pack err)
        Right ast -> do
            result <- runEval env (expand ast >>= eval)
            case result of
                Left err -> pure $ mkInspectorError err
                Right (val, _) -> pure $ mkInspectorContent val

-- | インスペクタエラーを生成
mkInspectorError :: Text -> Expr
mkInspectorError err =
    EList
        [ ESym ":title", EStr "Error"
        , ESym ":id", EInt 0
        , ESym ":content", EList
            [ EList [EStr ("Error: " <> err)]  -- content parts
            , EInt 1  -- length
            , EInt 0  -- start
            , EInt 1  -- end
            ]
        ]

-- | 値のインスペクタ表示を生成
mkInspectorContent :: Val -> Expr
mkInspectorContent val =
    EList
        [ ESym ":title", EStr (valTitle val <> " [" <> valTypeName val <> "]")
        , ESym ":id", EInt 0
        , ESym ":content", EList
            [ EList [EStr (valContentText val)]  -- content parts
            , EInt 1  -- length
            , EInt 0  -- start
            , EInt 1  -- end
            ]
        ]

-- | 値のコンテンツをテキストとして生成
valContentText :: Val -> Text
valContentText (VInt n) = T.unlines
    [ "Value: " <> T.pack (show n)
    , "Hex: " <> T.pack (printf "0x%x" n)
    , "Octal: " <> T.pack (printf "0o%o" n)
    ]
valContentText (VBool b) = "Value: " <> (if b then "#t" else "#f")
valContentText (VStr s) = T.unlines
    [ "Value: \"" <> s <> "\""
    , "Length: " <> T.pack (show (T.length s))
    ]
valContentText (VSym s) = "Symbol: " <> s
valContentText VNil = "Empty list (nil)"
valContentText (VList xs) = T.unlines $
    ("Length: " <> T.pack (show (length xs))) :
    map (\(i, v) -> "  [" <> T.pack (show i) <> "] " <> valTitle v) (zip [(0::Int)..] xs)
valContentText (VFunc args _ _) = T.unlines
    [ "Arguments: (" <> T.intercalate " " args <> ")"
    , "Type: User-defined function"
    ]
valContentText (VMacro args _ _) = T.unlines
    [ "Arguments: (" <> T.intercalate " " args <> ")"
    , "Type: User-defined macro"
    ]
valContentText (VPrim name _) = T.unlines
    [ "Name: " <> name
    , "Type: Built-in primitive"
    ]
valContentText (VData name fields) = T.unlines $
    ("Constructor: " <> name) :
    map (\(i, v) -> "  [" <> T.pack (show i) <> "] " <> valTitle v) (zip [(0::Int)..] fields)
valContentText (VMVar _) = "Mutable variable (MVar)"

-- | 値のタイトルを取得
valTitle :: Val -> Text
valTitle (VInt n) = T.pack (show n)
valTitle (VBool True) = "#t"
valTitle (VBool False) = "#f"
valTitle (VStr s) = "\"" <> s <> "\""
valTitle (VSym s) = s
valTitle VNil = "nil"
valTitle (VList xs) = "(" <> T.pack (show (length xs)) <> " elements)"
valTitle (VFunc args _ _) = "(fn (" <> T.intercalate " " args <> ") ...)"
valTitle (VMacro args _ _) = "(macro (" <> T.intercalate " " args <> ") ...)"
valTitle (VPrim name _) = "<primitive: " <> name <> ">"
valTitle (VData name _) = "<" <> name <> ">"
valTitle (VMVar _) = "<mvar>"

-- | 値の型名を取得
valTypeName :: Val -> Text
valTypeName (VInt _) = "Integer"
valTypeName (VBool _) = "Boolean"
valTypeName (VStr _) = "String"
valTypeName (VSym _) = "Symbol"
valTypeName VNil = "Nil"
valTypeName (VList _) = "List"
valTypeName (VFunc _ _ _) = "Function"
valTypeName (VMacro _ _ _) = "Macro"
valTypeName (VPrim _ _) = "Primitive"
valTypeName (VData name _) = name
valTypeName (VMVar _) = "MVar"

--------------------------------------------------------------------------------
-- MREPL Channel Handlers
--------------------------------------------------------------------------------

-- | MREPL チャンネルメッセージを処理する
handleChannelMessage :: Handle -> Env -> Integer -> Expr -> IO Env
handleChannelMessage h env channelId msg = case msg of
    -- (:process "code") - コードを評価
    EList [ESym ":process", EStr code] -> do
        meplEvalAndRespond h env channelId code

    -- (:teardown) - チャンネルを閉じる
    EList [ESym ":teardown"] -> do
        putStrLn $ "MREPL channel " ++ show channelId ++ " teardown"
        pure env

    -- (:clear-repl-history) - 履歴をクリア
    EList [ESym ":clear-repl-history"] -> do
        pure env

    -- その他
    _ -> do
        putStrLn $ "Unknown channel message: " ++ show msg
        pure env

-- | MREPL 用の評価とレスポンス
meplEvalAndRespond :: Handle -> Env -> Integer -> Text -> IO Env
meplEvalAndRespond h env channelId code =
    case readExpr code of
        Left err -> do
            -- 評価エラー: :evaluation-aborted を送信
            let abortMsg = EList
                    [ ESym ":channel-send"
                    , EInt channelId
                    , EList [ESym ":evaluation-aborted", EStr (T.pack err)]
                    ]
            sendPacket h (exprToText abortMsg)
            -- プロンプトを送信
            sendMreplPrompt h channelId
            pure env
        Right ast -> do
            result <- runEval env (expand ast >>= eval)
            case result of
                Left err -> do
                    -- 評価エラー
                    let abortMsg = EList
                            [ ESym ":channel-send"
                            , EInt channelId
                            , EList [ESym ":evaluation-aborted", EStr err]
                            ]
                    sendPacket h (exprToText abortMsg)
                    sendMreplPrompt h channelId
                    pure env
                Right (val, env') -> do
                    -- 結果を送信
                    let resultStr = exprToText (valToExpr val)
                        -- :write-values の形式: ((pretty-printed entry-idx copy-string) ...)
                        writeValuesMsg = EList
                            [ ESym ":channel-send"
                            , EInt channelId
                            , EList
                                [ ESym ":write-values"
                                , EList
                                    [ EList
                                        [ EStr resultStr  -- pretty-printed value
                                        , EInt 0          -- history entry index
                                        , EStr resultStr  -- copy string
                                        ]
                                    ]
                                ]
                            ]
                    sendPacket h (exprToText writeValuesMsg)
                    -- プロンプトを送信
                    sendMreplPrompt h channelId
                    pure env'

-- | MREPL プロンプトを送信する
sendMreplPrompt :: Handle -> Integer -> IO ()
sendMreplPrompt h channelId = do
    -- :prompt の形式: (package-name package-short-name error-level history-length)
    let promptMsg = EList
            [ ESym ":channel-send"
            , EInt channelId
            , EList
                [ ESym ":prompt"
                , EStr "SPINOR"  -- package name
                , EStr "SPINOR"  -- package short name
                , EInt 0         -- error/debug level
                , EInt 0         -- history length
                ]
            ]
    sendPacket h (exprToText promptMsg)

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

-- | インデント情報を生成する
-- Spinor の特殊形式に対するインデントルールを Emacs に送信
mkIndentationUpdate :: Expr
mkIndentationUpdate =
    EList
        [ ESym ":indentation-update"
        , EList $ map mkIndentRule
            [ ("fn", 1)           -- (fn (args) body...) - body を 1 インデント
            , ("let", 1)          -- (let bindings body...) - body を 1 インデント
            , ("if", 1)           -- (if cond then else)
            , ("match", 2)        -- (match expr cases...) - case のように 2
            , ("define", 1)       -- (define name value)
            , ("data", 1)         -- (data TypeName constructors...)
            , ("begin", 0)        -- (begin forms...) - 全て同じレベル
            , ("when", 1)         -- (when cond body...)
            , ("unless", 1)       -- (unless cond body...)
            , ("module", 1)       -- (module name exports...)
            , ("import", 1)       -- (import name options...)
            ]
        ]
  where
    mkIndentRule :: (Text, Integer) -> Expr
    mkIndentRule (name, indent) = EList [EStr name, EInt indent]

--------------------------------------------------------------------------------
-- TCP Server
--------------------------------------------------------------------------------

-- | TCP サーバーを起動する
runServer :: String -> Env -> IO ()
runServer port initialEnv = do
    tracedFns <- newTracedFunctions
    addr <- resolve
    bracket (open addr) close (acceptLoop tracedFns)
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

    acceptLoop tracedFns sock = forever $ do
        (conn, peer) <- accept sock
        putStrLn $ "Client connected: " ++ show peer
        void $ forkFinally (handleClient conn initialEnv tracedFns) (\_ -> close conn)

-- | クライアント接続を処理する
handleClient :: Socket -> Env -> TracedFunctions -> IO ()
handleClient sock env tracedFns = do
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h NoBuffering
    hSetBinaryMode h True
    clientLoop h env tracedFns `catch` handleDisconnect h
  where
    handleDisconnect :: Handle -> SomeException -> IO ()
    handleDisconnect hdl _ = do
        putStrLn "Client disconnected."
        hClose hdl

-- | クライアントとの Swank プロトコルループ
clientLoop :: Handle -> Env -> TracedFunctions -> IO ()
clientLoop h env tracedFns = do
    mLen <- recvHeader h
    case mLen of
        Nothing -> putStrLn "Client disconnected."
        Just len -> do
            payload <- recvPayload h len
            case readExpr payload of
                Left err -> do
                    putStrLn $ "Parse error: " ++ err
                    clientLoop h env tracedFns
                Right expr -> do
                    env' <- dispatchRPC h env tracedFns expr
                    clientLoop h env' tracedFns

-- | RPC ディスパッチ
dispatchRPC :: Handle -> Env -> TracedFunctions -> Expr -> IO Env
dispatchRPC h env tracedFns expr = case expr of
    -- (:emacs-rex form package thread-id request-id)
    EList [ESym ":emacs-rex", form, _package, _threadId, EInt reqId] ->
        handleSwankRequest h env tracedFns form reqId

    -- (:emacs-channel-send channel-id message)
    -- MREPL のチャンネルメッセージを処理
    EList [ESym ":emacs-channel-send", EInt channelId, msg] ->
        handleChannelMessage h env channelId msg

    -- チャンネルIDが nil やシンボルの場合 (デフォルトチャンネル 1 を使用)
    EList [ESym ":emacs-channel-send", _, msg] ->
        handleChannelMessage h env 1 msg

    -- その他のフォーマット
    _ -> do
        putStrLn $ "Unknown RPC format: " ++ show expr
        pure env
