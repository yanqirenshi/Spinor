{-# LANGUAGE OverloadedStrings #-}

module Spinor.Server
  ( -- * Server
    runServer
    -- * Exported for testing
  , normalizeCommand
  , normalizeForm
  , extractTraceSpec
  , exprToText
    -- * Response builders (for testing)
  , mkOkResponse
  , mkAbortResponse
    -- * Trace State (for testing)
  , TracedFunctions
  , newTracedFunctions
  , addTracedFunction
  , removeTracedFunction
  , isTraced
  , getTracedFunctions
  , clearAllTraces
  ) where

import Network.Socket
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Control.Monad.State.Strict (get)
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

import Spinor.Syntax (Expr(..), Pattern(..), ConstructorDef(..), TypeExpr(..), readExpr, dummySpan)
import Spinor.Eval (Eval, runEval, eval, valToExpr, exprToVal, applyClosureBody)
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
extractTraceSpec (EStr _ s) = s
extractTraceSpec (ESym _ s) = s
extractTraceSpec (EList _ [ESym _ "swank::from-string", EStr _ s]) = s
extractTraceSpec (EList _ [ESym _ "slynk::from-string", EStr _ s]) = s
extractTraceSpec (EList _ [ESym _ _, EStr _ s]) = s  -- その他の from-string 形式
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
exprToText (EInt _ n)   = T.pack (show n)
exprToText (EBool _ True)  = "t"
exprToText (EBool _ False) = "nil"
exprToText (ESym _ s)   = s
exprToText (EStr _ s)   = "\"" <> escapeString s <> "\""
exprToText (EList _ []) = "nil"
exprToText (EList _ xs) = "(" <> T.intercalate " " (map exprToText xs) <> ")"
exprToText (ELet _ _ _) = "<let>"
exprToText (EData _ _ _) = "<data>"
exprToText (EMatch _ _ _) = "<match>"
exprToText (EModule _ _ _) = "<module>"
exprToText (EImport _ _ _) = "<import>"

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
--     slynk-stickers:fetch -> swank:stickers:fetch
--     slynk-profiler:toggle-timing -> swank:profiler:toggle-timing
--     slynk-package-fu:list-all-package-names -> swank:package-fu:list-all-package-names
--     slynk-macrostep:macrostep-expand-1 -> swank:macrostep:macrostep-expand-1
normalizeCommand :: Text -> Text
normalizeCommand cmd
    | "slynk-completion:" `T.isPrefixOf` cmd = "swank:completion:" <> T.drop 17 cmd
    | "slynk-mrepl:" `T.isPrefixOf` cmd = "swank:mrepl:" <> T.drop 12 cmd
    | "slynk-trace-dialog:" `T.isPrefixOf` cmd = "swank:trace:" <> T.drop 19 cmd
    | "slynk-stickers:" `T.isPrefixOf` cmd = "swank:stickers:" <> T.drop 15 cmd
    | "slynk-profiler:" `T.isPrefixOf` cmd = "swank:profiler:" <> T.drop 15 cmd
    | "slynk-package-fu:" `T.isPrefixOf` cmd = "swank:package-fu:" <> T.drop 17 cmd
    | "slynk-macrostep:" `T.isPrefixOf` cmd = "swank:macrostep:" <> T.drop 16 cmd
    | "slynk-apropos:" `T.isPrefixOf` cmd = "swank:apropos:" <> T.drop 14 cmd
    | "slynk-xref:" `T.isPrefixOf` cmd = "swank:xref:" <> T.drop 11 cmd
    | "slynk:slynk-" `T.isPrefixOf` cmd = "swank:swank-" <> T.drop 12 cmd
    | "slynk:" `T.isPrefixOf` cmd       = "swank:" <> T.drop 6 cmd
    | otherwise                         = cmd

-- | Expr 内のコマンドシンボルを正規化する
normalizeForm :: Expr -> Expr
normalizeForm (EList sp (ESym sp2 cmd : rest)) = EList sp (ESym sp2 (normalizeCommand cmd) : rest)
normalizeForm other = other

--------------------------------------------------------------------------------
-- Swank RPC Dispatcher
--------------------------------------------------------------------------------

-- | Swank RPC リクエストを処理する
handleSwankRequest :: Handle -> Env -> TracedFunctions -> Expr -> Integer -> IO Env
handleSwankRequest h env tracedFns form reqId = case normalizeForm form of
    -- swank:connection-info - ハンドシェイク
    EList _ [ESym _ "swank:connection-info"] -> do
        let response = mkConnectionInfoResponse reqId
        sendPacket h (exprToText response)
        -- インデント情報を送信
        sendPacket h (exprToText mkIndentationUpdate)
        pure env

    -- swank:swank-require - モジュールロード
    -- モジュール名を大文字に変換して返す (Common Lisp の慣例)
    EList _ (ESym _ "swank:swank-require" : args) -> do
        let loadedModules = extractModules args
            response = mkOkResponse reqId (EList dummySpan loadedModules)
        sendPacket h (exprToText response)
        pure env
      where
        extractModules [EList _ [ESym _ "quote", EList _ modules]] = map toUpperModule modules
        extractModules [EList _ modules] = map toUpperModule modules
        extractModules modules = map toUpperModule modules
        toUpperModule (EStr _ name) = EStr dummySpan (T.toUpper name)
        toUpperModule (ESym _ name) = EStr dummySpan (T.toUpper name)
        toUpperModule other = other

    -- swank:swank-add-load-paths - ロードパス追加（無視して成功を返す）
    EList _ (ESym _ "swank:swank-add-load-paths" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:create-repl - REPL 作成
    EList _ (ESym _ "swank:create-repl" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [EStr dummySpan "user", EStr dummySpan "SPINOR>"])
        sendPacket h (exprToText response)
        pure env

    -- swank:listener-eval - S式の評価
    EList _ [ESym _ "swank:listener-eval", EStr _ code] -> do
        evalAndRespond h env code reqId

    -- swank:interactive-eval - S式の評価
    EList _ [ESym _ "swank:interactive-eval", EStr _ code] -> do
        evalAndRespond h env code reqId

    -- swank:compile-string-for-emacs - バッファからのコンパイル
    EList _ (ESym _ "swank:compile-string-for-emacs" : EStr _ code : _) -> do
        compileAndRespond h env code reqId

    -- swank:autodoc - 自動ドキュメント
    EList _ (ESym _ "swank:autodoc" : args) -> do
        let rawForms = extractAutodocForms args
            result = getAutodocInfo env rawForms
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env
      where
        extractAutodocForms (EList _ [ESym _ "quote", EList _ forms] : _) = forms
        extractAutodocForms (EList _ forms : _) = forms
        extractAutodocForms _ = []

    -- swank:operator-arglist - 引数リスト
    EList _ [ESym _ "swank:operator-arglist", EStr _ opName, _pkg] -> do
        let result = getArglist env opName
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:operator-arglist (その他の形式)
    EList _ (ESym _ "swank:operator-arglist" : _) -> do
        let response = mkOkResponse reqId (EStr dummySpan "")
        sendPacket h (exprToText response)
        pure env

    -- slynk-mrepl:create-mrepl - MREPL作成
    -- 引数: (create-mrepl channel-id)
    -- 戻り値: (channel-id thread-id)
    EList _ [ESym _ "swank:mrepl:create-mrepl", EInt _ channelId] -> do
        -- チャンネル作成成功を返す (channel-id, thread-id)
        let response = mkOkResponse reqId (EList dummySpan [EInt dummySpan channelId, EInt dummySpan 0])
        sendPacket h (exprToText response)
        -- 初期プロンプトを送信
        sendMreplPrompt h channelId
        pure env

    -- slynk-mrepl:create-mrepl - その他の形式
    EList _ (ESym _ "swank:mrepl:create-mrepl" : _) -> do
        -- デフォルトでチャンネルID 1 を使用
        let response = mkOkResponse reqId (EList dummySpan [EInt dummySpan 1, EInt dummySpan 0])
        sendPacket h (exprToText response)
        sendMreplPrompt h 1
        pure env

    -- slynk:buffer-first-change - バッファ変更通知 (無視して成功を返す)
    EList _ (ESym _ "swank:buffer-first-change" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:completions - 補完候補を返す
    EList _ [ESym _ "swank:completions", EStr _ prefix, _pkg] -> do
        let completions = getCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:completions - その他の形式
    EList _ (ESym _ "swank:completions" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [EList dummySpan [], EStr dummySpan ""])
        sendPacket h (exprToText response)
        pure env

    -- swank:simple-completions - シンプルな補完候補
    EList _ [ESym _ "swank:simple-completions", EStr _ prefix, _pkg] -> do
        let completions = getSimpleCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:simple-completions - その他の形式
    EList _ (ESym _ "swank:simple-completions" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [EList dummySpan [], EList dummySpan []])
        sendPacket h (exprToText response)
        pure env

    -- swank:completions-for-character - 文字の補完
    EList _ (ESym _ "swank:completions-for-character" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:completion:flex-completions - Flex 補完 (SLY の補完機能)
    EList _ [ESym _ "swank:completion:flex-completions", EStr _ prefix, _pkg] -> do
        let completions = getFlexCompletions env prefix
            response = mkOkResponse reqId completions
        sendPacket h (exprToText response)
        pure env

    -- swank:completion:flex-completions - その他の形式
    EList _ (ESym _ "swank:completion:flex-completions" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [EList dummySpan [], EList dummySpan []])
        sendPacket h (exprToText response)
        pure env

    -- swank:eval-for-inspector - インスペクタコンテキストで評価
    -- 形式: (slynk:eval-for-inspector inspector-id thread-id (quote slynk:func) args...)
    EList _ (ESym _ "swank:eval-for-inspector" : _inspId : _threadId : EList _ [ESym _ "quote", ESym _ func] : args) -> do
        let normalizedFunc = normalizeCommand func
        case (normalizedFunc, args) of
            ("swank:init-inspector", [EStr _ code]) -> do
                result <- inspectValue env code
                let response = mkOkResponse reqId result
                sendPacket h (exprToText response)
                pure env
            _ -> do
                -- 未対応の inspector 関数
                let response = mkOkResponse reqId (EList dummySpan [])
                sendPacket h (exprToText response)
                pure env

    -- swank:eval-for-inspector - その他の形式
    EList _ (ESym _ "swank:eval-for-inspector" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:init-inspector - 直接呼び出し形式
    EList _ [ESym _ "swank:init-inspector", EStr _ code] -> do
        result <- inspectValue env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:init-inspector - その他の形式
    EList _ (ESym _ "swank:init-inspector" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspect-nth-part - インスペクタの要素を検査
    EList _ [ESym _ "swank:inspect-nth-part", EInt _ _partId] -> do
        -- 簡易実装: 詳細な部品検査は未サポート
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-pop - 前の検査対象に戻る
    EList _ [ESym _ "swank:inspector-pop"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-next - 次の検査対象に進む
    EList _ [ESym _ "swank:inspector-next"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-reinspect - 現在のオブジェクトを再検査
    EList _ [ESym _ "swank:inspector-reinspect"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-range - インスペクタの範囲取得
    EList _ (ESym _ "swank:inspector-range" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:inspector-call-nth-action - アクション実行
    EList _ (ESym _ "swank:inspector-call-nth-action" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:quit-inspector - インスペクタを閉じる
    EList _ [ESym _ "swank:quit-inspector"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Trace Dialog Handlers
    --------------------------------------------------------------------------------

    -- swank:trace:dialog-toggle-trace - トレースのトグル
    -- 形式: (dialog-toggle-trace (slynk::from-string "name")) または (dialog-toggle-trace "name")
    EList _ [ESym _ "swank:trace:dialog-toggle-trace", specExpr] -> do
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
            response = mkOkResponse reqId (EStr dummySpan msg)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-traced-p - トレース状態を確認
    EList _ [ESym _ "swank:trace:dialog-traced-p", specExpr] -> do
        let spec = extractTraceSpec specExpr
        traced <- isTraced tracedFns spec
        let response = mkOkResponse reqId (EBool dummySpan traced)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-untrace - トレースを解除
    EList _ [ESym _ "swank:trace:dialog-untrace", specExpr] -> do
        let spec = extractTraceSpec specExpr
        removeTracedFunction tracedFns spec
        let response = mkOkResponse reqId (EBool dummySpan True)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:dialog-untrace-all - 全トレースを解除
    EList _ [ESym _ "swank:trace:dialog-untrace-all"] -> do
        tracedList <- getTracedFunctions tracedFns
        clearAllTraces tracedFns
        let response = mkOkResponse reqId (EList dummySpan [EInt dummySpan (fromIntegral $ length tracedList), EInt dummySpan 0])
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-specs - トレース対象の関数一覧を返す
    EList _ [ESym _ "swank:trace:report-specs"] -> do
        tracedList <- getTracedFunctions tracedFns
        -- 各関数を (name . details) の形式で返す
        let specs = map (\name -> EList dummySpan [EStr dummySpan name]) tracedList
            response = mkOkResponse reqId (EList dummySpan specs)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-total - トレース総数を返す
    EList _ [ESym _ "swank:trace:report-total"] -> do
        -- 現時点ではトレースエントリを保持していないので 0 を返す
        let response = mkOkResponse reqId (EInt dummySpan 0)
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:report-partial-tree - トレースツリーの一部を返す
    EList _ (ESym _ "swank:trace:report-partial-tree" : _) -> do
        -- 空のツリーを返す: (traces . remaining)
        let response = mkOkResponse reqId (EList dummySpan [EList dummySpan [], EInt dummySpan 0])
        sendPacket h (exprToText response)
        pure env

    -- swank:trace:clear-trace-tree - トレースツリーをクリア
    EList _ [ESym _ "swank:trace:clear-trace-tree"] -> do
        let response = mkOkResponse reqId (EBool dummySpan True)
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Stickers Handlers
    --------------------------------------------------------------------------------

    -- swank:stickers:compile-for-stickers - スティッカー付きコードをコンパイル
    EList _ (ESym _ "swank:stickers:compile-for-stickers" : _) -> do
        -- スティッカーの完全な実装には評価器との統合が必要
        -- 現時点では成功を返す
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:kill-stickers - スティッカーを削除
    EList _ [ESym _ "swank:stickers:kill-stickers", _ids] -> do
        let response = mkOkResponse reqId (EBool dummySpan True)
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:toggle-break-on-stickers - ブレーク切り替え
    EList _ [ESym _ "swank:stickers:toggle-break-on-stickers"] -> do
        -- ブレーク機能は未サポート
        let response = mkOkResponse reqId (EBool dummySpan False)
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:total-recordings - 記録総数を返す
    EList _ [ESym _ "swank:stickers:total-recordings"] -> do
        let response = mkOkResponse reqId (EInt dummySpan 0)
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:search-for-recording - 記録を検索
    EList _ (ESym _ "swank:stickers:search-for-recording" : _) -> do
        -- 記録がないので nil を返す
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:fetch - スティッカー情報を取得
    EList _ [ESym _ "swank:stickers:fetch", _deadStickers] -> do
        -- スティッカーリスト (空)
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:fetch - その他の形式
    EList _ (ESym _ "swank:stickers:fetch" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:forget - 記録をクリア
    EList _ (ESym _ "swank:stickers:forget" : _) -> do
        let response = mkOkResponse reqId (EInt dummySpan 0)
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:find-recording-or-lose - 特定の記録を取得
    EList _ (ESym _ "swank:stickers:find-recording-or-lose" : _) -> do
        let response = mkAbortResponse reqId "No recordings available"
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:inspect-sticker - スティッカーを検査
    EList _ [ESym _ "swank:stickers:inspect-sticker", _stickerId] -> do
        let response = mkAbortResponse reqId "Sticker inspection not available"
        sendPacket h (exprToText response)
        pure env

    -- swank:stickers:inspect-sticker-recording - スティッカー記録を検査
    EList _ (ESym _ "swank:stickers:inspect-sticker-recording" : _) -> do
        let response = mkAbortResponse reqId "No recordings to inspect"
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Profiler Handlers
    --------------------------------------------------------------------------------

    -- swank:profiler:toggle-timing - タイミング計測の切り替え
    EList _ [ESym _ "swank:profiler:toggle-timing", specExpr] -> do
        let spec = extractTraceSpec specExpr
            -- プロファイリングは未サポートなので、トグル状態を返す
            msg = spec <> " timing toggled (profiling not yet implemented)"
        let response = mkOkResponse reqId (EStr dummySpan msg)
        sendPacket h (exprToText response)
        pure env

    -- swank:profiler:timed-spec-p - タイミング計測中か確認
    EList _ [ESym _ "swank:profiler:timed-spec-p", _specExpr] -> do
        -- 常に False を返す (プロファイリング未実装)
        let response = mkOkResponse reqId (EBool dummySpan False)
        sendPacket h (exprToText response)
        pure env

    -- swank:profiler:untime-all - 全てのタイミング計測を解除
    EList _ [ESym _ "swank:profiler:untime-all"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:profiler:report-latest-timings - タイミングレポートを取得
    EList _ [ESym _ "swank:profiler:report-latest-timings"] -> do
        -- 空のレポートを返す
        -- 形式: (partial-timings grand-total)
        let response = mkOkResponse reqId (EList dummySpan [EList dummySpan [], EInt dummySpan 0])
        sendPacket h (exprToText response)
        pure env

    -- swank:profiler:clear-timing-tree - タイミングデータをクリア
    EList _ [ESym _ "swank:profiler:clear-timing-tree"] -> do
        let response = mkOkResponse reqId (EBool dummySpan True)
        sendPacket h (exprToText response)
        pure env

    -- swank:profiler:report-specs - タイミング対象の一覧を返す
    EList _ [ESym _ "swank:profiler:report-specs"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Package-FU Handlers
    --------------------------------------------------------------------------------

    -- swank:package-fu:list-all-package-names - 全パッケージ名を返す
    -- Spinor はシングル名前空間なので、デフォルトパッケージのみ返す
    EList _ [ESym _ "swank:package-fu:list-all-package-names"] -> do
        let packages = EList dummySpan [EStr dummySpan "SPINOR", EStr dummySpan "USER", EStr dummySpan "COMMON-LISP"]
            response = mkOkResponse reqId packages
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:list-all-package-names - 引数あり形式
    EList _ (ESym _ "swank:package-fu:list-all-package-names" : _) -> do
        let packages = EList dummySpan [EStr dummySpan "SPINOR", EStr dummySpan "USER", EStr dummySpan "COMMON-LISP"]
            response = mkOkResponse reqId packages
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:set-package - パッケージを設定
    -- Spinor ではパッケージの切り替えは実質的にサポートしないが、成功を返す
    EList _ [ESym _ "swank:package-fu:set-package", EStr _ _pkgName] -> do
        -- (package-name prompt-string) の形式で返す
        let result = EList dummySpan [EStr dummySpan "SPINOR", EStr dummySpan "SPINOR"]
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:set-package - その他の形式
    EList _ (ESym _ "swank:package-fu:set-package" : _) -> do
        let result = EList dummySpan [EStr dummySpan "SPINOR", EStr dummySpan "SPINOR"]
            response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:read-package-name - パッケージ名を読み取る
    EList _ (ESym _ "swank:package-fu:read-package-name" : _) -> do
        let response = mkOkResponse reqId (EStr dummySpan "SPINOR")
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:apropos-package - パッケージを検索
    EList _ [ESym _ "swank:package-fu:apropos-package", EStr _ query] -> do
        -- 簡易実装: クエリが "SPINOR" を含む場合のみマッチ
        let matches = if T.isInfixOf (T.toUpper query) "SPINOR"
                      then EList dummySpan [EStr dummySpan "SPINOR"]
                      else EList dummySpan []
            response = mkOkResponse reqId matches
        sendPacket h (exprToText response)
        pure env

    -- swank:package-fu:apropos-package - その他の形式
    EList _ (ESym _ "swank:package-fu:apropos-package" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [EStr dummySpan "SPINOR"])
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Macrostep Handlers
    --------------------------------------------------------------------------------

    -- swank:macrostep:macrostep-expand-1 - マクロを1段階展開
    -- 引数: (code &key environment)
    EList _ (ESym _ "swank:macrostep:macrostep-expand-1" : EStr _ code : _) -> do
        result <- macrostepExpand1 env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:macrostep:macrostep-expand - マクロを完全展開
    EList _ (ESym _ "swank:macrostep:macrostep-expand" : EStr _ code : _) -> do
        result <- macrostepExpandFull env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:macrostep:compiler-macroexpand-1 - コンパイラマクロ展開 (単一ステップ)
    EList _ (ESym _ "swank:macrostep:compiler-macroexpand-1" : EStr _ code : _) -> do
        -- Spinor にはコンパイラマクロがないので、通常のマクロ展開を行う
        result <- macrostepExpand1 env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    -- swank:macrostep:compiler-macroexpand - コンパイラマクロ展開 (完全)
    EList _ (ESym _ "swank:macrostep:compiler-macroexpand" : EStr _ code : _) -> do
        result <- macrostepExpandFull env code
        let response = mkOkResponse reqId result
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Apropos Handlers
    --------------------------------------------------------------------------------

    -- swank:apropos:apropos-list-for-emacs - シンボル検索
    -- 引数: (pattern &optional external-only case-sensitive package)
    EList _ (ESym _ "swank:apropos:apropos-list-for-emacs" : EStr _ pattern : _) -> do
        let results = aproposSearch env pattern
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    -- swank:apropos:apropos-list-for-emacs - 引数なし
    EList _ [ESym _ "swank:apropos:apropos-list-for-emacs"] -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:apropos-list-for-emacs - 旧形式 (SLIME 互換)
    EList _ (ESym _ "swank:apropos-list-for-emacs" : EStr _ pattern : _) -> do
        let results = aproposSearch env pattern
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Xref Handlers (Cross-Reference)
    --------------------------------------------------------------------------------

    -- swank:xref - クロスリファレンス検索
    -- 引数: (type name)
    -- type: :calls, :references, :binds, :sets, :macroexpands, :specializes, :callers, :callees
    EList _ [ESym _ "swank:xref", ESym _ xrefType, EStr _ name] -> do
        let results = xrefSearch env xrefType name
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    -- swank:xref - シンボル形式の名前
    EList _ [ESym _ "swank:xref", ESym _ xrefType, ESym _ name] -> do
        let results = xrefSearch env xrefType name
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    -- swank:xref - その他の形式
    EList _ (ESym _ "swank:xref" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:xrefs - 複数タイプの xref 検索
    EList _ (ESym _ "swank:xrefs" : _) -> do
        let response = mkOkResponse reqId (EList dummySpan [])
        sendPacket h (exprToText response)
        pure env

    -- swank:xref:xref - slynk-xref 形式
    EList _ [ESym _ "swank:xref:xref", ESym _ xrefType, EStr _ name] -> do
        let results = xrefSearch env xrefType name
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    -- swank:xref:xref - シンボル形式
    EList _ [ESym _ "swank:xref:xref", ESym _ xrefType, ESym _ name] -> do
        let results = xrefSearch env xrefType name
            response = mkOkResponse reqId results
        sendPacket h (exprToText response)
        pure env

    --------------------------------------------------------------------------------
    -- Disassemble Handlers
    --------------------------------------------------------------------------------

    -- swank:disassemble-form - フォームをディスアセンブル
    -- Spinor はインタープリタなので、関数の内部表現を表示
    EList _ [ESym _ "swank:disassemble-form", EStr _ code] -> do
        let result = disassembleCode env code
            response = mkOkResponse reqId (EStr dummySpan result)
        sendPacket h (exprToText response)
        pure env

    -- swank:disassemble-form - その他の形式
    EList _ (ESym _ "swank:disassemble-form" : _) -> do
        let response = mkOkResponse reqId (EStr dummySpan "No code to disassemble")
        sendPacket h (exprToText response)
        pure env

    -- swank:disassemble-symbol - シンボルをディスアセンブル
    EList _ [ESym _ "swank:disassemble-symbol", EStr _ name] -> do
        let result = disassembleSymbol env name
            response = mkOkResponse reqId (EStr dummySpan result)
        sendPacket h (exprToText response)
        pure env

    -- swank:disassemble-symbol - シンボル形式
    EList _ [ESym _ "swank:disassemble-symbol", ESym _ name] -> do
        let result = disassembleSymbol env name
            response = mkOkResponse reqId (EStr dummySpan result)
        sendPacket h (exprToText response)
        pure env

    -- swank:disassemble-symbol - その他の形式
    EList _ (ESym _ "swank:disassemble-symbol" : _) -> do
        let response = mkOkResponse reqId (EStr dummySpan "No symbol to disassemble")
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
        Nothing -> EList dummySpan [ESym dummySpan ":not-available", ESym dummySpan "t"]
        Just funcName ->
            -- まず primitiveDocs を検索
            case Map.lookup funcName primitiveDocs of
                Just docEntry ->
                    let argStr = "(" <> funcName <> " " <> docSignature docEntry <> ")"
                    in EList dummySpan [EStr dummySpan argStr, ESym dummySpan "t"]
                Nothing ->
                    -- primitiveDocs になければ Env を検索
                    case Map.lookup funcName env of
                        Just (VFunc args _ _) ->
                            let argStr = "(" <> funcName <> " " <> T.intercalate " " args <> ")"
                            in EList dummySpan [EStr dummySpan argStr, ESym dummySpan "t"]
                        Just (VPrim name _) ->
                            let argStr = "(" <> name <> " ...)"
                            in EList dummySpan [EStr dummySpan argStr, ESym dummySpan "t"]
                        Just (VMacro args _ _) ->
                            let argStr = "(" <> funcName <> " " <> T.intercalate " " args <> ")"
                            in EList dummySpan [EStr dummySpan argStr, ESym dummySpan "t"]
                        _ -> EList dummySpan [ESym dummySpan ":not-available", ESym dummySpan "t"]

-- | autodoc フォームから関数名を抽出する
extractFunctionName :: [Expr] -> Maybe Text
extractFunctionName [] = Nothing
extractFunctionName (EList _ (ESym _ name : _) : _) = Just name
extractFunctionName (ESym _ name : _) = Just name
extractFunctionName (EStr _ name : _) = Just name
extractFunctionName (_ : rest) = extractFunctionName rest

-- | operator-arglist 用の引数リストを取得する
getArglist :: Env -> Text -> Expr
getArglist env funcName =
    -- まず primitiveDocs を検索
    case Map.lookup funcName primitiveDocs of
        Just docEntry -> EStr dummySpan $ "(" <> docSignature docEntry <> ")"
        Nothing ->
            -- primitiveDocs になければ Env を検索
            case Map.lookup funcName env of
                Just (VFunc args _ _) ->
                    let argStr = "(" <> T.intercalate " " args <> ")"
                    in EStr dummySpan argStr
                Just (VMacro args _ _) ->
                    let argStr = "(" <> T.intercalate " " args <> ")"
                    in EStr dummySpan argStr
                Just (VPrim _ _) -> EStr dummySpan "(...)"
                _ -> EStr dummySpan ""

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
    in EList dummySpan [EList dummySpan items, EStr dummySpan commonPrefix]
  where
    mkCompletionItem name =
        case Map.lookup name primitiveDocs of
            Just doc -> EList dummySpan [EStr dummySpan name, EStr dummySpan (docSignature doc)]
            Nothing -> EList dummySpan [EStr dummySpan name, EStr dummySpan ""]

-- | シンプルな補完候補を取得する (swank:simple-completions 形式)
-- 戻り値: ((候補文字列リスト) (パッケージリスト))
getSimpleCompletions :: Env -> Text -> Expr
getSimpleCompletions env prefix =
    let primNames = Map.keys primitiveDocs
        envNames = Map.keys env
        allNames = primNames ++ envNames
        matches = filter (prefix `T.isPrefixOf`) allNames
    in EList dummySpan [EList dummySpan (map (EStr dummySpan) matches), EList dummySpan []]

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
    in EList dummySpan [EList dummySpan items, EList dummySpan []]
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
        in EList dummySpan
            [ EStr dummySpan name           -- 補完テキスト
            , EInt dummySpan score          -- スコア
            , EList dummySpan []            -- chunks (マッチ位置、省略)
            , EStr dummySpan classification -- 分類
            ]

--------------------------------------------------------------------------------
-- Apropos Helpers
--------------------------------------------------------------------------------

-- | パターンにマッチするシンボルを検索
-- 戻り値: ((:designator (package-name symbol-name) :function "doc") ...)
aproposSearch :: Env -> Text -> Expr
aproposSearch env pattern =
    let primNames = Map.keys primitiveDocs
        envNames = Map.keys env
        allNames = nub (primNames ++ envNames)
        -- パターンにマッチするシンボルをフィルタ (大文字小文字無視)
        patternLower = T.toLower pattern
        matches = filter (\name -> patternLower `T.isInfixOf` T.toLower name) allNames
        -- 各シンボルの情報を生成
        items = map mkAproposItem matches
    in EList dummySpan items
  where
    mkAproposItem name =
        let info = getSymbolInfo name
            -- designator は (package-name symbol-name) のリスト形式
            designator = EList dummySpan [EStr dummySpan "SPINOR", EStr dummySpan name]
        in EList dummySpan $
            [ ESym dummySpan ":designator", designator ] ++ info

    getSymbolInfo name =
        case Map.lookup name primitiveDocs of
            Just docEntry ->
                [ ESym dummySpan ":function", EStr dummySpan (docDescription docEntry) ]
            Nothing ->
                case Map.lookup name env of
                    Just (VFunc _ _ _) ->
                        [ ESym dummySpan ":function", EStr dummySpan "User-defined function" ]
                    Just (VMacro _ _ _) ->
                        [ ESym dummySpan ":macro", EStr dummySpan "User-defined macro" ]
                    Just (VPrim pname _) ->
                        [ ESym dummySpan ":function", EStr dummySpan ("Built-in: " <> pname) ]
                    Just (VInt _) ->
                        [ ESym dummySpan ":variable", EStr dummySpan "Integer value" ]
                    Just (VBool _) ->
                        [ ESym dummySpan ":variable", EStr dummySpan "Boolean value" ]
                    Just (VStr _) ->
                        [ ESym dummySpan ":variable", EStr dummySpan "String value" ]
                    _ ->
                        [ ESym dummySpan ":variable", EStr dummySpan "" ]

--------------------------------------------------------------------------------
-- Xref Helpers
--------------------------------------------------------------------------------

-- | クロスリファレンス検索
-- Spinor は静的解析機能を持たないため、現時点では空の結果を返す
-- 将来的には AST 解析を追加して実装可能
-- 戻り値: ((label location) ...)
xrefSearch :: Env -> Text -> Text -> Expr
xrefSearch _env xrefType name =
    -- xrefType: :calls, :references, :binds, :sets, :macroexpands,
    --           :specializes, :callers, :callees
    -- 現時点では全て空リストを返す (静的解析未実装)
    -- ただし、シンボルが存在することは確認する
    let _typeInfo = case xrefType of
            ":calls"        -> "Functions called by " <> name
            ":callers"      -> "Functions calling " <> name
            ":references"   -> "References to " <> name
            ":binds"        -> "Bindings of " <> name
            ":sets"         -> "Assignments to " <> name
            ":macroexpands" -> "Macroexpansions of " <> name
            ":specializes"  -> "Specializations of " <> name
            ":callees"      -> "Functions called by " <> name
            _               -> "Unknown xref type"
    in EList dummySpan []  -- 空の結果 (静的解析未実装)

--------------------------------------------------------------------------------
-- Disassemble Helpers
--------------------------------------------------------------------------------

-- | コードをディスアセンブル (パースして AST を表示)
-- quoted シンボル ('symbol) の場合は環境から検索
disassembleCode :: Env -> Text -> Text
disassembleCode env code =
    case readExpr code of
        Left err -> "Parse error: " <> T.pack err
        Right ast -> case ast of
            -- 'symbol の場合は環境から検索
            EList _ [ESym _ "quote", ESym _ name] -> disassembleSymbol env name
            -- シンボル単体の場合も環境から検索
            ESym _ name -> disassembleSymbol env name
            -- その他は AST を表示
            _ -> T.unlines
                [ "; Spinor Disassembly (AST representation)"
                , "; ========================================"
                , "; Source: " <> code
                , ";"
                , "; AST:"
                , T.pack (show ast)
                ]

-- | シンボルをディスアセンブル (環境から関数を取得して表示)
disassembleSymbol :: Env -> Text -> Text
disassembleSymbol env name =
    case Map.lookup name env of
        Nothing -> "Symbol not found: " <> name
        Just val -> T.unlines
            [ "; Spinor Disassembly"
            , "; ========================================"
            , "; Symbol: " <> name
            , "; Type: " <> valTypeName val
            , ";"
            , formatValForDisassembly val
            ]

-- | 値をディスアセンブリ形式でフォーマット
formatValForDisassembly :: Val -> Text
formatValForDisassembly (VFunc args body closureEnv) = T.unlines
    [ "; Function"
    , "; Arguments: (" <> T.intercalate " " args <> ")"
    , "; Closure environment: " <> T.pack (show (length (Map.keys closureEnv))) <> " bindings"
    , ";"
    , "; Body:"
    , exprToLispText body
    ]
formatValForDisassembly (VMacro args body closureEnv) = T.unlines
    [ "; Macro"
    , "; Arguments: (" <> T.intercalate " " args <> ")"
    , "; Closure environment: " <> T.pack (show (length (Map.keys closureEnv))) <> " bindings"
    , ";"
    , "; Body:"
    , exprToLispText body
    ]
formatValForDisassembly (VPrim name _) = T.unlines
    [ "; Primitive function"
    , "; Name: " <> name
    , "; (Native implementation - no AST available)"
    ]
formatValForDisassembly (VInt n) = "; Integer value: " <> T.pack (show n)
formatValForDisassembly (VBool b) = "; Boolean value: " <> if b then "#t" else "#f"
formatValForDisassembly (VStr s) = "; String value: \"" <> s <> "\""
formatValForDisassembly (VSym s) = "; Symbol: " <> s
formatValForDisassembly VNil = "; Nil"
formatValForDisassembly (VList xs) = "; List with " <> T.pack (show (length xs)) <> " elements"
formatValForDisassembly (VData name fields) = T.unlines
    [ "; Data constructor: " <> name
    , "; Fields: " <> T.pack (show (length fields))
    ]
formatValForDisassembly (VMVar _) = "; Mutable variable (MVar)"
formatValForDisassembly (VFloat f) = "; Float value: " <> T.pack (show f)
formatValForDisassembly (VMatrix rows cols _) = T.unlines
    [ "; Matrix " <> T.pack (show rows) <> "x" <> T.pack (show cols)
    , "; (row-major storage)"
    ]
formatValForDisassembly (VCLContext _ _) = "; OpenCL Context"
formatValForDisassembly (VCLBuffer _ n) = "; OpenCL Buffer (size=" <> T.pack (show n) <> ")"
formatValForDisassembly (VCLKernel _ name) = "; OpenCL Kernel: " <> name
formatValForDisassembly (VWindow _) = "; GLFW Window"

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
    EList dummySpan
        [ ESym dummySpan ":title", EStr dummySpan "Error"
        , ESym dummySpan ":id", EInt dummySpan 0
        , ESym dummySpan ":content", EList dummySpan
            [ EList dummySpan [EStr dummySpan ("Error: " <> err)]  -- content parts
            , EInt dummySpan 1  -- length
            , EInt dummySpan 0  -- start
            , EInt dummySpan 1  -- end
            ]
        ]

-- | 値のインスペクタ表示を生成
mkInspectorContent :: Val -> Expr
mkInspectorContent val =
    EList dummySpan
        [ ESym dummySpan ":title", EStr dummySpan (valTitle val <> " [" <> valTypeName val <> "]")
        , ESym dummySpan ":id", EInt dummySpan 0
        , ESym dummySpan ":content", EList dummySpan
            [ EList dummySpan [EStr dummySpan (valContentText val)]  -- content parts
            , EInt dummySpan 1  -- length
            , EInt dummySpan 0  -- start
            , EInt dummySpan 1  -- end
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
valContentText (VFloat f) = "Value: " <> T.pack (show f)
valContentText (VMatrix rows cols _) = T.unlines
    [ "Dimensions: " <> T.pack (show rows) <> " x " <> T.pack (show cols)
    , "Type: Matrix (row-major)"
    ]
valContentText (VCLContext _ _) = "Type: OpenCL Context"
valContentText (VCLBuffer _ n) = "Size: " <> T.pack (show n) <> " elements"
valContentText (VCLKernel _ name) = "Kernel: " <> name
valContentText (VWindow _) = "Type: GLFW Window"

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
valTitle (VFloat f) = T.pack (show f)
valTitle (VMatrix rows cols _) = "<matrix " <> T.pack (show rows) <> "x" <> T.pack (show cols) <> ">"
valTitle (VCLContext _ _) = "<CLContext>"
valTitle (VCLBuffer _ n) = "<CLBuffer:size=" <> T.pack (show n) <> ">"
valTitle (VCLKernel _ name) = "<CLKernel:" <> name <> ">"
valTitle (VWindow _) = "<Window>"

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
valTypeName (VFloat _) = "Float"
valTypeName (VMatrix _ _ _) = "Matrix"
valTypeName (VCLContext _ _) = "CLContext"
valTypeName (VCLBuffer _ _) = "CLBuffer"
valTypeName (VCLKernel _ _) = "CLKernel"
valTypeName (VWindow _) = "Window"

--------------------------------------------------------------------------------
-- Macrostep Helpers
--------------------------------------------------------------------------------

-- | マクロを1段階展開する
-- 戻り値: (:expansion expanded-code :macroform? t/nil)
macrostepExpand1 :: Env -> Text -> IO Expr
macrostepExpand1 env code =
    case readExpr code of
        Left err -> pure $ mkMacrostepError (T.pack err)
        Right ast -> do
            result <- runEval env (macroExpandOnce ast)
            case result of
                Left err -> pure $ mkMacrostepError err
                Right (expanded, _) ->
                    let expandedStr = exprToLispText expanded
                        -- マクロ展開が行われたかどうか
                        isMacro = expandedStr /= code
                    in pure $ EList dummySpan
                        [ ESym dummySpan ":expansion", EStr dummySpan expandedStr
                        , ESym dummySpan ":macroform?", EBool dummySpan isMacro
                        ]

-- | マクロを完全展開する
macrostepExpandFull :: Env -> Text -> IO Expr
macrostepExpandFull env code =
    case readExpr code of
        Left err -> pure $ mkMacrostepError (T.pack err)
        Right ast -> do
            result <- runEval env (expand ast)
            case result of
                Left err -> pure $ mkMacrostepError err
                Right (expanded, _) ->
                    let expandedStr = exprToLispText expanded
                        isMacro = expandedStr /= code
                    in pure $ EList dummySpan
                        [ ESym dummySpan ":expansion", EStr dummySpan expandedStr
                        , ESym dummySpan ":macroform?", EBool dummySpan isMacro
                        ]

-- | 1段階のみマクロ展開を行う (再帰展開しない)
macroExpandOnce :: Expr -> Eval Expr
macroExpandOnce (EList sp (ESym _ name : args)) = do
    env <- get
    case Map.lookup name env of
        Just (VMacro params body closureEnv) -> do
            -- マクロ適用: 引数を評価せず Val に変換して適用
            let argVals = map exprToVal args
            result <- applyClosureBody params body closureEnv argVals
            -- 展開結果を Expr に変換 (再帰展開しない)
            pure $ valToExpr result
        _ -> pure $ EList sp (ESym dummySpan name : args)  -- マクロでなければそのまま
macroExpandOnce e = pure e  -- リスト以外はそのまま

-- | macrostep エラーを生成
mkMacrostepError :: Text -> Expr
mkMacrostepError err = EList dummySpan [ESym dummySpan ":error", EStr dummySpan err]

-- | Expr を Lisp 形式のテキストに変換 (macrostep 用)
-- exprToText とほぼ同じだが、より読みやすい形式で出力
exprToLispText :: Expr -> Text
exprToLispText (EInt _ n)   = T.pack (show n)
exprToLispText (EBool _ True)  = "#t"
exprToLispText (EBool _ False) = "#f"
exprToLispText (ESym _ s)   = s
exprToLispText (EStr _ s)   = "\"" <> escapeString s <> "\""
exprToLispText (EList _ []) = "nil"
exprToLispText (EList _ [ESym _ "quote", x]) = "'" <> exprToLispText x
exprToLispText (EList _ xs) = "(" <> T.intercalate " " (map exprToLispText xs) <> ")"
exprToLispText (ELet _ bindings body) =
    "(let (" <> T.intercalate " " (map bindingToText bindings) <> ") " <> exprToLispText body <> ")"
  where
    bindingToText (name, val) = "(" <> name <> " " <> exprToLispText val <> ")"
exprToLispText (EData _ name ctors) =
    "(data " <> name <> " " <> T.intercalate " " (map ctorToText ctors) <> ")"
  where
    ctorToText (ConstructorDef cname []) = cname
    ctorToText (ConstructorDef cname args) =
        "(" <> cname <> " " <> T.intercalate " " (map typeExprToText args) <> ")"
    typeExprToText (TEVar v) = v
    typeExprToText (TEApp c []) = c
    typeExprToText (TEApp c args) =
        "(" <> c <> " " <> T.intercalate " " (map typeExprToText args) <> ")"
exprToLispText (EMatch _ target branches) =
    "(match " <> exprToLispText target <> " " <>
    T.intercalate " " (map branchToText branches) <> ")"
  where
    branchToText (pat, body) = "(" <> patternToText pat <> " " <> exprToLispText body <> ")"
    patternToText (PVar v) = v
    patternToText (PCon cname []) = cname
    patternToText (PCon cname pats) =
        "(" <> cname <> " " <> T.intercalate " " (map patternToText pats) <> ")"
    patternToText (PLit e) = exprToLispText e
    patternToText PWild = "_"
exprToLispText (EModule _ name _) = "(module " <> name <> " ...)"
exprToLispText (EImport _ name _) = "(import " <> name <> " ...)"

--------------------------------------------------------------------------------
-- MREPL Channel Handlers
--------------------------------------------------------------------------------

-- | MREPL チャンネルメッセージを処理する
handleChannelMessage :: Handle -> Env -> Integer -> Expr -> IO Env
handleChannelMessage h env channelId msg = case msg of
    -- (:process "code") - コードを評価
    EList _ [ESym _ ":process", EStr _ code] -> do
        meplEvalAndRespond h env channelId code

    -- (:teardown) - チャンネルを閉じる
    EList _ [ESym _ ":teardown"] -> do
        putStrLn $ "MREPL channel " ++ show channelId ++ " teardown"
        pure env

    -- (:clear-repl-history) - 履歴をクリア
    EList _ [ESym _ ":clear-repl-history"] -> do
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
            let abortMsg = EList dummySpan
                    [ ESym dummySpan ":channel-send"
                    , EInt dummySpan channelId
                    , EList dummySpan [ESym dummySpan ":evaluation-aborted", EStr dummySpan (T.pack err)]
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
                    let abortMsg = EList dummySpan
                            [ ESym dummySpan ":channel-send"
                            , EInt dummySpan channelId
                            , EList dummySpan [ESym dummySpan ":evaluation-aborted", EStr dummySpan err]
                            ]
                    sendPacket h (exprToText abortMsg)
                    sendMreplPrompt h channelId
                    pure env
                Right (val, env') -> do
                    -- 結果を送信
                    let resultStr = exprToText (valToExpr val)
                        -- :write-values の形式: ((pretty-printed entry-idx copy-string) ...)
                        writeValuesMsg = EList dummySpan
                            [ ESym dummySpan ":channel-send"
                            , EInt dummySpan channelId
                            , EList dummySpan
                                [ ESym dummySpan ":write-values"
                                , EList dummySpan
                                    [ EList dummySpan
                                        [ EStr dummySpan resultStr  -- pretty-printed value
                                        , EInt dummySpan 0          -- history entry index
                                        , EStr dummySpan resultStr  -- copy string
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
    let promptMsg = EList dummySpan
            [ ESym dummySpan ":channel-send"
            , EInt dummySpan channelId
            , EList dummySpan
                [ ESym dummySpan ":prompt"
                , EStr dummySpan "SPINOR"  -- package name
                , EStr dummySpan "SPINOR"  -- package short name
                , EInt dummySpan 0         -- error/debug level
                , EInt dummySpan 0         -- history length
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
                        response = mkOkResponse reqId (EStr dummySpan resultStr)
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
                    let successMsg = EList dummySpan [EStr dummySpan "Compilation finished.", ESym dummySpan "t"]
                        response = mkOkResponse reqId successMsg
                    sendPacket h (exprToText response)
                    pure env'

--------------------------------------------------------------------------------
-- Response Builders
--------------------------------------------------------------------------------

-- | :return (:ok result) request-id
mkOkResponse :: Integer -> Expr -> Expr
mkOkResponse reqId result =
    EList dummySpan [ESym dummySpan ":return", EList dummySpan [ESym dummySpan ":ok", result], EInt dummySpan reqId]

-- | :return (:abort message) request-id
mkAbortResponse :: Integer -> Text -> Expr
mkAbortResponse reqId msg =
    EList dummySpan [ESym dummySpan ":return", EList dummySpan [ESym dummySpan ":abort", EStr dummySpan msg], EInt dummySpan reqId]

-- | connection-info レスポンスを生成する
mkConnectionInfoResponse :: Integer -> Expr
mkConnectionInfoResponse reqId =
    EList dummySpan
        [ ESym dummySpan ":return"
        , EList dummySpan
            [ ESym dummySpan ":ok"
            , EList dummySpan
                [ ESym dummySpan ":pid", EInt dummySpan 0
                , ESym dummySpan ":style", ESym dummySpan ":spawn"
                , ESym dummySpan ":encoding", EList dummySpan [ESym dummySpan ":coding-systems", EList dummySpan [EStr dummySpan "utf-8-unix"]]
                , ESym dummySpan ":lisp-implementation"
                , EList dummySpan
                    [ ESym dummySpan ":type", EStr dummySpan "Spinor"
                    , ESym dummySpan ":version", EStr dummySpan "0.1.0"
                    , ESym dummySpan ":program", EStr dummySpan "spinor"
                    , ESym dummySpan ":name", EStr dummySpan "spinor"
                    ]
                , ESym dummySpan ":package"
                , EList dummySpan [ESym dummySpan ":name", EStr dummySpan "user", ESym dummySpan ":prompt", EStr dummySpan "SPINOR>"]
                , ESym dummySpan ":version", EStr dummySpan "2.27"
                -- サポートするモジュールを事前に登録
                , ESym dummySpan ":modules"
                , EList dummySpan
                    [ EStr dummySpan "SLYNK/ARGLISTS"
                    , EStr dummySpan "SLYNK/FANCY-INSPECTOR"
                    , EStr dummySpan "SLYNK/PACKAGE-FU"
                    , EStr dummySpan "SLYNK/MREPL"
                    , EStr dummySpan "SLYNK/TRACE-DIALOG"
                    , EStr dummySpan "SLYNK/STICKERS"
                    , EStr dummySpan "SLYNK/PROFILER"
                    , EStr dummySpan "SLYNK/INDENTATION"
                    -- , EStr dummySpan "SLYNK/MACROSTEP"  -- TODO: tasks/todo_202602201030_macrostep.md
                    , EStr dummySpan "SLYNK/RETRO"
                    ]
                ]
            ]
        , EInt dummySpan reqId
        ]

-- | インデント情報を生成する
-- Spinor の特殊形式に対するインデントルールを Emacs に送信
mkIndentationUpdate :: Expr
mkIndentationUpdate =
    EList dummySpan
        [ ESym dummySpan ":indentation-update"
        , EList dummySpan $ map mkIndentRule
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
    mkIndentRule (name, indent) = EList dummySpan [EStr dummySpan name, EInt dummySpan indent]

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
    EList _ [ESym _ ":emacs-rex", form, _package, _threadId, EInt _ reqId] ->
        handleSwankRequest h env tracedFns form reqId

    -- (:emacs-channel-send channel-id message)
    -- MREPL のチャンネルメッセージを処理
    EList _ [ESym _ ":emacs-channel-send", EInt _ channelId, msg] ->
        handleChannelMessage h env channelId msg

    -- チャンネルIDが nil やシンボルの場合 (デフォルトチャンネル 1 を使用)
    EList _ [ESym _ ":emacs-channel-send", _, msg] ->
        handleChannelMessage h env 1 msg

    -- その他のフォーマット
    _ -> do
        putStrLn $ "Unknown RPC format: " ++ show expr
        pure env
