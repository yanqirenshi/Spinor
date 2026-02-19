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
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T

import Language.LSP.Server
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Lens qualified as L
import Language.LSP.VFS (virtualFileText)

import Spinor.Syntax (parseFileWithErrors, SpinorParseError(..))
import Spinor.Lsp.Docs (lookupDoc, DocEntry(..), allDocEntries)

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

  -- Hover ハンドラ
  , requestHandler SMethod_TextDocumentHover $ \req responder -> do
      let uri = req ^. L.params . L.textDocument . L.uri
          pos = req ^. L.params . L.position
          line = pos ^. L.line
          col  = pos ^. L.character

      mVirtualFile <- getVirtualFile (toNormalizedUri uri)
      case mVirtualFile of
        Nothing -> responder $ Right $ InR Null
        Just vf -> do
          let content = virtualFileText vf
              linesOfText = T.lines content
          if fromIntegral line >= length linesOfText
            then responder $ Right $ InR Null
            else do
              let lineText = linesOfText !! fromIntegral line
              case extractWordAt lineText col of
                Nothing -> responder $ Right $ InR Null
                Just word -> case lookupDoc word of
                  Nothing -> responder $ Right $ InR Null
                  Just doc -> do
                    let md = "```lisp\n(" <> word <> ") :: " <> docSignature doc <> "\n```\n\n" <> docDescription doc
                        hover = Hover
                          { _contents = InL $ MarkupContent MarkupKind_Markdown md
                          , _range = Nothing
                          }
                    responder $ Right $ InL hover

  -- Completion ハンドラ
  , requestHandler SMethod_TextDocumentCompletion $ \_req responder -> do
      let items = map toCompletionItem allDocEntries
      responder $ Right $ InL items
  ]

-- | ドキュメントエントリを CompletionItem に変換
toCompletionItem :: (Text, DocEntry) -> CompletionItem
toCompletionItem (name, doc) = CompletionItem
  { _label = name
  , _labelDetails = Nothing
  , _kind = Just (docKind doc)
  , _tags = Nothing
  , _detail = Just (docSignature doc)
  , _documentation = Just $ InR $ MarkupContent MarkupKind_Markdown (docDescription doc)
  , _deprecated = Nothing
  , _preselect = Nothing
  , _sortText = Nothing
  , _filterText = Nothing
  , _insertText = Nothing
  , _insertTextFormat = Nothing
  , _insertTextMode = Nothing
  , _textEdit = Nothing
  , _textEditText = Nothing
  , _additionalTextEdits = Nothing
  , _commitCharacters = Nothing
  , _command = Nothing
  , _data_ = Nothing
  }

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

-- | Lisp シンボル構成文字かどうか
isSymbolChar :: Char -> Bool
isSymbolChar c = isAlphaNum c || c `elem` ("_+-*/%=<>?!" :: String)

-- | カーソル位置の単語を抽出
extractWordAt :: Text -> UInt -> Maybe Text
extractWordAt line col
  | col' >= T.length line = Nothing
  | not (isSymbolChar (T.index line col')) = Nothing
  | otherwise =
      let before = T.takeWhileEnd isSymbolChar (T.take (col' + 1) line)
          after  = T.takeWhile isSymbolChar (T.drop (col' + 1) line)
      in Just (before <> after)
  where
    col' = fromIntegral col
