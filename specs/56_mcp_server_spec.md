# 56: Spinor MCP Server (Model Context Protocol) Specification

## 1. 概要
Spinor を MCP (Model Context Protocol) サーバーとして動作させる機能を実装する。これにより、Claude Code 等の AI エージェントが Spinor の実行環境（評価器、型推論器、マクロ展開器）と標準入出力を介して対話し、プロジェクトの状態を正確に把握できるようにする。

## 2. 通信プロトコル
- **トランスポート:** 標準入出力 (stdio)。
- **メッセージ形式:** JSON-RPC 2.0 に基づく MCP 規格。
- **CLI コマンド:** `spinor mcp`

## 3. 提供ツール (Tools)
AI エージェントが呼び出し可能な以下のツールを定義する。

### `eval`
- **引数:** `code` (string)
- **機能:** 指定された Spinor コードを現在のプロジェクトコンテキストで評価する。

### `typecheck`
- **引数:** `code` (string)
- **機能:** 指定された式の型を推論する。

### `macroexpand`
- **引数:** `code` (string)
- **機能:** マクロを展開した結果の S 式を返す。

### `list-symbols`
- **引数:** `package` (string, optional)
- **機能:** 利用可能なシンボル一覧を返す。

## 4. 状態管理
- MCP サーバーは起動中、ひとつの `EvalState` および `TypeEnv` を維持する。
- ツール呼び出しによって定義が行われた場合、その変更は以降の呼び出しに引き継がれる。
