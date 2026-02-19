# Step 37: LSP Server Foundation - 実装指示書

## 概要

Haskell の `lsp` パッケージを使用して、Spinor 言語の構文エラーをエディタ上に表示する LSP サーバーの基盤を構築してください。

## Steps

### 1. 依存ライブラリの追加

- `package.yaml` の `dependencies` に以下を追加してください。
  - `lsp` (バージョン 2.x 系の最新安定版)
  - `lens`
  - `mtl`
- `cabal build` を実行し、依存パッケージが解決できるか確認してください。

### 2. LSP サーバーモジュールの作成

- `src/Spinor/Lsp/Server.hs` を作成してください。
- `Language.LSP.Server` モジュールを使用し、`runServer` を呼び出すエントリーポイント関数 `runLspServer :: IO Int` を実装してください。
- サーバーのケイパビリティとして `textDocumentSync = Just (TdSyncOptions (Just TdSyncFull) ...)` を設定してください。

### 3. 診断 (Diagnostics) ロジックの実装

- `textDocument/didOpen` および `textDocument/didChange` の通知ハンドラ (`notificationHandler`) を実装してください。
- ハンドラ内で以下の処理を行ってください。
  1. クライアントから送られてきたドキュメントの URI とテキスト（全文）を取得。
  2. `Spinor.Parser` を用いてテキストをパース。
  3. エラーがなければ `publishDiagnostics` に空リストを送信。
  4. エラーがあれば、パーサが返す位置情報 (Line, Column) から `Range` を構築し、`publishDiagnostics` でエラーメッセージを送信。
  *(※ 現在のパーサが詳細な位置情報を返さない場合は、ファイルの先頭や簡易的な位置情報で仮置きし、後でパーサを拡張すること)*

### 4. CLI への統合

- `app/Main.hs` を修正し、`spinor lsp` というサブコマンドで `runLspServer` が呼び出されるようにしてください。

### 5. 検証手順

- コンパイル: `cabal build`
- 実行確認: `cabal run spinor -- lsp` (実行後、標準入力を待機する状態になればOK。`Ctrl-C` で終了)

## 実装報告

### Implementation Policy (実装方針)
*(実装完了後、ここに記述してください)*

### Implementation Details (実装内容)
*(実装完了後、ここに記述してください)*
