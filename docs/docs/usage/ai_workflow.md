# Spinor による AI ネイティブ開発

Spinor は、Claude Code、Gemini CLI などの AI コーディングアシスタントとシームレスに連携するよう、ゼロから設計されています。このガイドでは、Spinor の AI ネイティブ機能を活用した革新的な開発体験について解説します。

## なぜ AI ネイティブなのか？

### 最高の組み合わせ：Lisp 構文 + 静的型

Spinor は、AI 支援開発に最適な2つの特性を兼ね備えています：

1. **S 式構文**: Lisp の一貫した括弧ベースの構造は、パースが容易で曖昧さがありません。AI モデルは複雑な構文規則に混乱することなく、コードを生成・操作できます。

2. **静的型システム**: Hindley-Milner 型推論により、コンパイル時にエラーを検出します。AI が誤ったコードを書いても、型チェッカーが即座に問題を特定します—実行時にバグを発見する必要はありません。

この組み合わせにより、強力なワークフローが実現します：

> **人間が型で意図を表現し、AI がコードを実装し、コンパイラが正しさを検証する。**

### 従来の問題点

動的型付け言語では、AI が生成したコードは一見正しく見えても、実行時に暗号のようなエラーで失敗することがあります。複雑な構文を持つ言語では、AI が構文的に無効なコードを生成し、手動での修正が必要になることがあります。

Spinor は両方の問題を解決します：シンプルな構文によりパースエラーを防ぎ、型システムが実行前に論理エラーを検出します。

---

## `spinor init` で始める

### AI 対応プロジェクトの作成

```bash
spinor init my-project
cd my-project
```

以下の構造が作成されます：

```
my-project/
  src/
    main.spin          # エントリーポイント
  test/
    test.spin          # テストスイート
  CLAUDE.md            # AI コンテキストファイル
  .agents/             # Agent Teams ディレクトリ
    TEAMS.md           # マルチエージェントプロトコル
    tasks/
      todo/
      in-progress/
      review/
      done/
    mailboxes/
  .gitignore
```

### `CLAUDE.md` ファイル

`CLAUDE.md` ファイルは AI アシスタントにとって極めて重要です。AI がプロジェクトを開くと、このファイルを読んで以下を理解します：

- プロジェクトのビルド方法と実行方法
- 言語の構文と型システム
- コーディング規約とベストプラクティス
- 検証に使用できるコマンド

**重要なポイント**: AI は Spinor をゼロから学ぶ必要はありません—`CLAUDE.md` が必要なすべてのコンテキストを提供します。

---

## 自己修復ループ

### 機械可読なエラー出力

`spinor check` コマンドは、実行せずに型チェックのみを行います：

```bash
spinor check src/main.spin
```

AI が利用する場合は、`--json` フラグを使用します：

```bash
spinor check --json src/main.spin
```

### 成功時のレスポンス

```json
{
  "status": "success",
  "command": "check",
  "message": "Type check passed. 5 expressions analyzed."
}
```

### エラー時のレスポンス

```json
{
  "status": "error",
  "errors": [
    {
      "file": "src/main.spin",
      "line": 12,
      "col": 5,
      "message": "Type mismatch: expected Int, got Str",
      "code": "TYPE_ERROR"
    }
  ]
}
```

### ループの動作

AI アシスタントがコードを修正する際は、以下の手順に従います：

1. コードの変更を **書く**
2. `spinor check --json <file>` を **実行する**
3. JSON レスポンスを **パースする**
4. 報告された行/列のエラーを **修正する**
5. `status: "success"` になるまで **繰り返す**

これにより、AI が型システムが満足するまで出力を反復的に改善する自律的な修正サイクルが生まれます。

### エラーコード

| コード | 説明 |
|--------|------|
| `PARSE_ERROR` | 構文エラー（括弧の不一致など） |
| `UNDEFINED_SYMBOL` | 未定義の変数への参照 |
| `TYPE_ERROR` | 型の不一致 |
| `ARITY_ERROR` | 引数の数が不正 |
| `ERROR` | その他のエラー |

---

## MCP サーバー連携

### MCP とは？

MCP（Model Context Protocol）により、AI アシスタントは外部ツールとリアルタイムで対話できます。Spinor の MCP サーバーを使用すると、AI は言語ランタイムに直接問い合わせることができ、型や動作に関するハルシネーションを防止できます。

### MCP サーバーの起動

```bash
spinor mcp
```

サーバーは stdio 経由の JSON-RPC 2.0 で通信します。

### Claude Code の設定

Claude Code の MCP 設定（`~/.config/claude-code/settings.json` またはプロジェクトローカル）に以下を追加します：

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

### 利用可能なツール

#### `eval` - Spinor コードの実行

```json
{
  "jsonrpc": "2.0",
  "method": "tools/call",
  "params": {
    "name": "eval",
    "arguments": { "code": "(+ 1 2)" }
  },
  "id": 1
}
```

レスポンス：
```json
{
  "jsonrpc": "2.0",
  "result": {
    "content": [{ "type": "text", "text": "3" }]
  },
  "id": 1
}
```

#### `typecheck` - 式の型推論

```json
{
  "params": {
    "name": "typecheck",
    "arguments": { "code": "(fn (x) (+ x 1))" }
  }
}
```

レスポンス：
```json
{
  "result": {
    "content": [{ "type": "text", "text": "Int -> Int" }]
  }
}
```

#### `macroexpand` - マクロ展開

```json
{
  "params": {
    "name": "macroexpand",
    "arguments": { "code": "(defun foo (x) (+ x 1))" }
  }
}
```

完全に展開された S 式を返します。

#### `list-symbols` - パッケージシンボルの一覧

```json
{
  "params": {
    "name": "list-symbols",
    "arguments": { "package": "spinor-user" }
  }
}
```

指定されたパッケージ内のすべてのシンボルを一覧表示します。

### メリット

- **ハルシネーションなし**: AI はリアルタイムで型と動作を検証できる
- **インタラクティブな探索**: AI はコードを書く前に実験できる
- **正確なドキュメント**: AI は実際の関数シグネチャを問い合わせできる

---

## マルチエージェント開発（Agent Teams）

### 概要

複雑なプロジェクトでは、Agent Teams ワークフローを使用して複数の AI エージェントが並行して作業できます。このファイルベースのシステムは、**Team Lead** と複数の **Teammate** 間の作業を調整します。

### ディレクトリ構造

```
.agents/
  TEAMS.md              # プロトコルドキュメント
  tasks/
    todo/               # 未割り当てのタスク
    in-progress/        # 現在作業中
    review/             # 完了、レビュー待ち
    done/               # 完了したタスク
  mailboxes/            # エージェント間メッセージング
```

### 役割

#### Team Lead

- プロジェクト目標を `todo/` 内のタスクファイルに **分解する**
- `review/` 内の完了した作業を **レビューする**
- `spinor check --json` を実行して提出物を検証する
- 承認したタスクを `done/` に移動するか、コメント付きでリジェクトする

#### Teammate（開発者）

- `todo/` から `in-progress/` にファイルを移動してタスクを **取得する**
- 必要な機能を **実装する**
- `spinor check --json` で **自己検証する**
- タスクを `review/` に移動して **提出する**

### タスクファイル形式

```markdown
# Task: map 関数の実装

## Description
リストの各要素に f を適用する map 関数を作成する。

## Acceptance Criteria
- [ ] 空リストで動作する
- [ ] 非空リストで動作する
- [ ] 型チェックが通る

## Assigned To
teammate-1

## Status
in-progress

## Notes
パターンマッチを使った再帰で実装。
```

### ロックプロトコル

ファイルシステムがアトミックなロックを提供します：

1. `todo/` 内のタスクファイルは取得可能
2. ファイルを `in-progress/` に移動する操作はアトミック（1つのエージェントのみ成功）
3. これにより重複作業を防止

### チームセッションの開始

1. Team Lead エージェントを起動：
   ```bash
   claude --agent-mode
   # "あなたは Team Lead です。.agents/TEAMS.md を読んでタスクを管理してください。"
   ```

2. Teammate エージェントを起動：
   ```bash
   claude --agent-mode
   # "あなたは Teammate です。.agents/tasks/todo/ で利用可能な作業を確認してください。"
   ```

各エージェントは独立して動作し、ファイルシステムを通じて通信します。

---

## ベストプラクティス

### 1. 型システムを信頼する

型システムが既に保証していることに対して、ランタイムチェックを追加しないでください。AI に自由にコードを書かせ、エラーの検出は `spinor check` に任せましょう。

### 2. 型を先に書く

実装の前にデータ型と関数シグネチャを定義します：

```lisp
;; データの形を定義
(data Tree
  (Leaf a)
  (Node (Tree a) a (Tree a)))

;; 型注釈で関数シグネチャを定義（オプションだが有用）
(defun tree-sum (t)  ; AI が推論: Tree Int -> Int
  (match t
    ((Leaf x) x)
    ((Node l v r) (+ (tree-sum l) (+ v (tree-sum r))))))
```

### 3. 複雑なクエリには MCP を使用

動作が不明な場合は、AI に MCP を使って仮説をテストさせます：

```
AI: "この式の型を確認させてください..."
    -> MCP typecheck: (map (fn (x) (+ x 1)) '(1 2 3))
    -> 結果: List Int
```

### 4. 自己修復ループで反復

変更のたびに `spinor check --json` を実行するよう AI に促します。これにより緊密なフィードバックループが生まれます：

```
書く -> チェック -> 修正 -> チェック -> 成功
```

### 5. 大規模プロジェクトには Agent Teams を活用

複数ファイルの変更や機能開発の場合：
- 作業を小さな独立したタスクに分割
- エージェントを並行して動作させる
- レビューステップで品質管理

---

## トラブルシューティング

### AI がパースできないコードを生成する

以下を確認：
- 括弧の不一致
- 文字列を囲む引用符の欠落
- 無効なエスケープシーケンス

JSON 出力の `PARSE_ERROR` コードが問題の場所を示します。

### AI が関数を見つけられない

以下を確認：
- 関数が使用前に定義されている
- パッケージが `(:use :package-name)` で正しくインポートされている
- 関数が定義元パッケージからエクスポートされている

### MCP サーバーが応答しない

以下を確認：
- サーバーが実行中（`spinor mcp`）
- JSON-RPC リクエスト形式が正しい
- ツール名のスペルが正しい（`eval`、`typecheck` など）

### Agent Teams のタスクが停滞する

以下を確認：
- タスクファイルが有効な Markdown である
- エージェントがプロトコルのために `TEAMS.md` を読んでいる
- 作業せずにタスクを保持しているエージェントがいない

---

## まとめ

Spinor の AI ネイティブ設計は、ソフトウェア開発の新しいパラダイムを実現します：

| 機能 | 人間の役割 | AI の役割 |
|------|-----------|----------|
| 型システム | 意図を定義 | 実装を生成 |
| 自己修復ループ | 結果をレビュー | エラーを反復修正 |
| MCP サーバー | 設定する | リアルタイムで問い合わせ |
| Agent Teams | 目標を設定 | 並行実行 |

プログラミングの未来は協調的です。Spinor はその協調をシームレスにします。
