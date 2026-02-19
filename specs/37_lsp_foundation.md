# Step 37: LSP Server Foundation - 技術仕様

## 1. 概要

Spinor 言語のための Language Server Protocol (LSP) サーバー `spinor-lsp` を実装する。
初期段階として、クライアント（エディタ）との接続確立と、リアルタイムな構文エラー表示（Diagnostics）機能を提供する。

## 2. アーキテクチャ

### 2.1. サーバープロセス

- Spinor 実行ファイル (`spinor`) のサブコマンド `lsp` として起動する。
  - 実行例: `spinor lsp`
- 標準入力 (stdin) と標準出力 (stdout) を用いて、JSON-RPC でエディタと通信を行う。

### 2.2. 利用ライブラリ

- `lsp`: Haskell 用の LSP サーバー実装フレームワーク。
- `text`, `lens`: `lsp` パッケージと組み合わせて JSON や LSP 型を扱うため。

## 3. 実装機能

### 3.1. ライフサイクル管理

- `initialize`: クライアントからの初期化リクエストに応答し、サーバーのケイパビリティ（対応機能）を通知する。
  - Text Document Sync: `Full` （ファイル変更時に全文を受信する最もシンプルな方式）

### 3.2. テキスト同期 (Text Synchronization)

- `textDocument/didOpen`: エディタでファイルが開かれた際に、その内容を受け取る。
- `textDocument/didChange`: エディタで内容が編集された際に、変更内容を受け取る。

### 3.3. 診断機能 (Diagnostics)

ファイルが開かれたり変更されたりする度に、以下の処理を行う。
1. エディタから送られてきたソースコード文字列を `Spinor.Parser` でパースする。
2. パースに成功した場合は、空の診断リストを送信し、既存のエラー表示を消去する。
3. パースエラーが発生した場合 (`Left ParseError`)、エラーの発生した行 (Line) と列 (Column) を特定する。
4. `textDocument/publishDiagnostics` 通知を用いて、クライアントにエラー情報を送信する。
   - エラーレベル: `Error`
   - メッセージ: パーサが返したエラー詳細

## 4. 将来の拡張への備え

本ステップでは状態（State）を極力持たない構成とするが、将来的には「型推論の結果」や「環境（Env）」をサーバー側でキャッシュし、Hover や Go to Definition に利用できるよう、ハンドラの構造を柔軟に設計する。
