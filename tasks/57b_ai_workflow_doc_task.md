# 57b: AI-Native Workflow Documentation の執筆指示書

## 1. 目的
Spinor の強力な AI 協調開発機能をユーザーに周知し、最高峰の AI-Native 開発体験を提供するためのドキュメントを執筆・統合する。

## 2. 実装手順

### Step 1: ガイドの執筆
- `manual/public/docs/ai_workflow.md` を新規作成し、Specs に基づいて執筆する。
- コード例（MCP 設定、JSON 出力、Agent Tasks 等）を含めること。

### Step 2: サイドバーの改修
- `manual/src/components/Sidebar.tsx` を編集し、新設した 「AI Integration」 カテゴリを追加する。

### Step 3: Web サイトの動作確認
- マニュアルサイトを実行し、新規ページが正しく表示されるか確認する。
- `cabal run spinor -- docgen` との整合性を確認。

## 3. 完了条件
- `ai_workflow.md` が作成され、マニュアルのナビゲーションからアクセス可能であること。
- 全ての AI 連携機能の使い方が正確に解説されていること。

---
## 実装報告

### 実装方針

Spinor の AI 協調開発機能群（Self-Healing Loop, MCP Server, Agent Teams）を、人間のユーザーが理解し設定できるための包括的なガイドを作成する。単なる機能解説にとどまらず、AI との協働開発における「人間が型で意図を定義し、AI が実装を生成する」という新しいパラダイムを提示する。

### 実装内容

#### 1. マニュアルの執筆 (`manual/public/docs/ai_workflow.md`)

以下の構成で AI-Native 開発ガイドを執筆:

1. **Why AI-Native?**
   - Lisp 構文と静的型システムの組み合わせが AI に適している理由
   - 「人間が型を定義し、AI が実装する」開発スタイルの提案

2. **Getting Started with `spinor init`**
   - プロジェクト生成とディレクトリ構造
   - `CLAUDE.md` の役割と重要性

3. **The Self-Healing Loop (`spinor check --json`)**
   - `--json` オプションによる機械可読エラー出力
   - Write → Check → Fix → Repeat のサイクル解説
   - エラーコード一覧（PARSE_ERROR, TYPE_ERROR 等）

4. **MCP Server Integration (`spinor mcp`)**
   - MCP の概要と利点
   - Claude Code への設定方法
   - 4つのツール（eval, typecheck, macroexpand, list-symbols）の使用例

5. **Multi-Agent Development (Agent Teams)**
   - `.agents/` ディレクトリ構造
   - Team Lead と Teammate の役割
   - タスクファイル形式とロックプロトコル

6. **Best Practices & Troubleshooting**
   - 型システムの活用法
   - よくある問題と解決策

#### 2. サイドバーの更新 (`manual/src/components/Sidebar.tsx`)

「Experimental」の手前に「AI Integration」カテゴリを追加:

```typescript
{
  label: 'AI Integration',
  items: [
    { label: 'AI-Native Workflow', to: '/docs/ai_workflow' },
  ],
},
```

#### 3. 動作確認

```bash
$ spinor docgen
Generating documentation...
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-03-01
