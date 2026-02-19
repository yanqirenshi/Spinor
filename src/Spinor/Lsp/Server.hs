{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Spinor.Lsp.Server
  ( runLspServer
  ) where

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

import Language.LSP.Server
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.VFS (virtualFileText)

import Spinor.Syntax (parseFileWithErrors, SpinorParseError(..))

-- | LSP サーバーのエントリーポイント
--   終了コードを返す (0: 正常終了)
runLspServer :: IO Int
runLspServer = runServer serverDef

-- | サーバー定義
serverDef :: ServerDefinition ()
serverDef = ServerDefinition
  { parseConfig = const $ const $ Right ()
  , onConfigChange = const $ pure ()
  , defaultConfig = ()
  , configSection = "spinor"
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = const handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
      { optTextDocumentSync = Just syncOptions
      }
  }

-- | テキスト同期オプション
--   Full: ファイル変更時に全文を受信する
syncOptions :: TextDocumentSyncOptions
syncOptions = TextDocumentSyncOptions
  { _openClose = Just True
  , _change = Just TextDocumentSyncKind_Full
  , _willSave = Nothing
  , _willSaveWaitUntil = Nothing
  , _save = Nothing
  }

-- | 通知ハンドラ
handlers :: Handlers (LspM ())
handlers = mconcat
  [ notificationHandler SMethod_TextDocumentDidOpen $ \msg -> do
      let doc = msg ^. L.params . L.textDocument
          uri = doc ^. L.uri
          content = doc ^. L.text
      publishDiagnosticsForDocument uri content

  , notificationHandler SMethod_TextDocumentDidChange $ \msg -> do
      let uri = msg ^. L.params . L.textDocument . L.uri
      -- VFS から最新のドキュメント内容を取得
      mVirtualFile <- getVirtualFile (toNormalizedUri uri)
      case mVirtualFile of
        Just vf -> do
          let content = virtualFileText vf
          publishDiagnosticsForDocument uri content
        Nothing -> pure ()

  , notificationHandler SMethod_TextDocumentDidClose $ \msg -> do
      let uri = msg ^. L.params . L.textDocument . L.uri
      -- ファイルが閉じられたら診断をクリア
      sendNotification SMethod_TextDocumentPublishDiagnostics $
        PublishDiagnosticsParams uri Nothing []
  ]

-- | ドキュメントの診断を実行して結果を送信
publishDiagnosticsForDocument :: Uri -> Text -> LspM () ()
publishDiagnosticsForDocument uri content = do
  let diagnostics = parseAndDiagnose content
  sendNotification SMethod_TextDocumentPublishDiagnostics $
    PublishDiagnosticsParams uri Nothing diagnostics

-- | ソースコードをパースして診断を生成
parseAndDiagnose :: Text -> [Diagnostic]
parseAndDiagnose content =
  case parseFileWithErrors content of
    Right _ -> []  -- パース成功: エラーなし
    Left errs -> map toDiagnostic errs

-- | SpinorParseError を LSP Diagnostic に変換
toDiagnostic :: SpinorParseError -> Diagnostic
toDiagnostic err =
  let line = fromIntegral (errorLine err - 1)  -- LSP は 0-indexed
      col = fromIntegral (errorColumn err - 1)
      pos = Position line col
      range = Range pos pos
  in Diagnostic
       { _range = range
       , _severity = Just DiagnosticSeverity_Error
       , _code = Nothing
       , _codeDescription = Nothing
       , _source = Just "spinor"
       , _message = errorMessage err
       , _tags = Nothing
       , _relatedInformation = Nothing
       , _data_ = Nothing
       }
