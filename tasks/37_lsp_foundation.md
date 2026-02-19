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

1. **lsp パッケージ 2.x 系を採用**: 最新の LSP プロトコル仕様に対応した `lsp` (2.8.0.0) と `lsp-types` (2.4.0.0) を使用。
2. **VFS (Virtual File System) の活用**: `textDocument/didChange` 時は VFS からドキュメント内容を取得することで、Union 型の複雑な処理を回避。
3. **パーサーの拡張**: `Spinor.Syntax` モジュールに `SpinorParseError` 型と `parseFileWithErrors` 関数を追加し、エラー位置情報を取得可能に。

### Implementation Details (実装内容)

#### 変更ファイル一覧

1. **spinor.cabal**
   - `lsp >= 2.4 && < 3`
   - `lens >= 5.0 && < 6`
   - `co-log-core >= 0.3 && < 1`
   - `stm >= 2.5 && < 3`
   - `Spinor.Lsp.Server` を exposed-modules に追加

2. **src/Spinor/Syntax.hs**
   - `SpinorParseError` データ型を追加 (行番号、列番号、エラーメッセージ)
   - `parseFileWithErrors :: Text -> Either [SpinorParseError] [Expr]` を追加

3. **src/Spinor/Lsp/Server.hs** (新規作成)
   - `runLspServer :: IO Int` エントリーポイント
   - `ServerDefinition` 設定 (TextDocumentSyncKind_Full)
   - `SMethod_TextDocumentDidOpen` / `SMethod_TextDocumentDidChange` / `SMethod_TextDocumentDidClose` ハンドラ
   - `parseAndDiagnose` / `toDiagnostic` 関数で診断生成

4. **app/Main.hs**
   - `["lsp"]` ケースを追加
   - `lspMode` 関数で `runLspServer` を呼び出し

#### 検証結果

- `cabal build`: 成功
- `cabal run spinor -- lsp`: 正常起動 (`[Info] Server starting` 出力)
- `cabal test`: 全79テストパス
