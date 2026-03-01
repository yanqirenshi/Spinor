# 56: Spinor MCP Server の実装指示書

## 1. 目的
AI エージェントが Spinor 処理系と対話するための MCP サーバーを実装し、AI-Native な開発体験を実現する。

## 2. 実装手順

### Step 1: MCP プロトコルハンドラの作成
- `src/Spinor/MCP.hs` を新規作成する。
- `aeson` を使用して JSON-RPC 構造体とイベントループを実装。

### Step 2: ツールロジックの結線
- `eval`, `typecheck`, `macroexpand` を既存のコアロジックと結線。
- `SpinorError` を JSON 形式のエラーレスポンスに変換。

### Step 3: CLI への統合 (`app/Main.hs`)
- `spinor mcp` サブコマンドを追加。
- 起動時に Twister ライブラリをロード。

### Step 4: `CLAUDE.md` の更新
- `src/Spinor/Template.hs` を更新し、MCP サーバーの登録手順を追記。

### Step 5: 検証
- JSON-RPC リクエストを用いた結合テストを実施。

## 3. 完了条件
- `spinor mcp` が正常に動作し、外部 MCP クライアントからツールを呼び出せること。

---
## 実装報告

### 実装方針

MCP (Model Context Protocol) サーバーを JSON-RPC 2.0 over stdio で実装し、AI エージェント (Claude Code 等) が Spinor 処理系と対話できるようにする。4つのツール (eval, typecheck, macroexpand, list-symbols) を提供し、EvalState と TypeEnv を維持することで、定義が以降の呼び出しに引き継がれる状態管理を実現。

### 実装内容

#### 1. MCP プロトコルハンドラ (`src/Spinor/MCP.hs`)

**JSON-RPC 2.0 型定義:**
- `JsonRpcRequest`: リクエスト構造体 (jsonrpc, id, method, params)
- `JsonRpcResponse`: レスポンス構造体 (jsonrpc, id, result, error)
- `JsonRpcError`: エラー構造体 (code, message, data)
- `McpTool`: ツール定義構造体 (name, description, inputSchema)

**イベントループ:**
```haskell
runMcpServer :: IO ()
runMcpServer = do
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  stateRef <- newIORef =<< initMcpState
  forever $ do
    line <- TIO.getLine
    case eitherDecode (BL.fromStrict $ TE.encodeUtf8 line) of
      Left err -> sendResponse (errorResponse ...)
      Right req -> handleRequest stateRef req >>= sendResponse
```

**MCP 状態:**
```haskell
data McpState = McpState
  { mcpContext :: Context   -- パッケージコンテキスト
  , mcpTypeEnv :: TypeEnv   -- 型環境
  }
```

#### 2. ツール実装

**eval:**
- コードをパースし、マクロ展開と評価を実行
- 状態変更 (定義の追加) を維持

**typecheck:**
- Hindley-Milner 型推論を実行
- 推論された型を文字列として返却

**macroexpand:**
- マクロを展開し、結果の S 式を Lisp 形式で返却
- `exprToLispText` を使用

**list-symbols:**
- 指定パッケージまたは全パッケージのシンボル一覧を返却
- エクスポートされたシンボルとバインディングを区別

#### 3. CLI 統合 (`app/Main.hs`)

```haskell
["mcp"] -> mcpMode

mcpMode :: IO ()
mcpMode = runMcpServer
```

ヘルプメッセージにも追記:
```
mcp                    Start MCP server (for AI agent integration)
```

#### 4. Template.hs 更新

`claudeMd` に以下を追加:

- Quick Reference に `spinor mcp` コマンド
- MCP Server セクション (Available Tools, Setup for Claude Code)

```markdown
## MCP Server (AI Agent Integration)

### Available Tools
- **eval**: Evaluate Spinor code in the current context
- **typecheck**: Infer the type of a Spinor expression
- **macroexpand**: Expand macros and return the resulting S-expression
- **list-symbols**: List available symbols in a package

### Setup for Claude Code
```json
{
  "mcpServers": {
    "spinor": {
      "command": "spinor",
      "args": ["mcp"]
    }
  }
}
```
```

#### 5. 検証

**ビルド確認:**
```
cabal build  # 成功
```

**ヘルプ確認:**
```
$ spinor --help
  mcp                    Start MCP server (for AI agent integration)
```

**テスト:**
```
229 examples, 0 failures
```

### 完了日
2026-03-01
