# 57b: AI-Native Workflow Documentation Specification

## 1. 概要
Spinor のユニークな AI 協調開発機能群を、人間のユーザーが理解し設定できるようにするための専用ガイドを作成する。単なる機能解説にとどまらず、Claude Code や Gemini CLI とどのように対話すべきかのベストプラクティスを提示する。

## 2. ドキュメント構成 (`ai_workflow.md`)

### 2.1 Introduction: Why AI-Native?
- Spinor の Lisp 構文と静的型システムの組み合わせが、なぜ AI にとって扱いやすいのかの解説。
- 「AI がコードを書き、人間が型で意図を強制する」という開発スタイルの提案。

### 2.2 Getting Started with `spinor init`
- プロジェクト生成と `CLAUDE.md` の重要性。
- AI エージェントがプロジェクトを開いた瞬間に、その文脈（ビルド方法、型システム、ルール）を理解する仕組み。

### 2.3 The Self-Healing Loop (`spinor check --json`)
- `--json` オプションによる機械可読なエラー出力の説明。
- AI がコンパイルエラーを自律的にパースし、修正案を再考するプロセスの解説。

### 2.4 MCP Server Integration (`spinor mcp`)
- `spinor mcp` コマンドによる対話型サーバーの起動。
- Claude Code 等の MCP クライアントへの登録方法（`claude.json` の設定）。
- AI が「実行時の型情報」や「マクロ展開結果」を直接問い合わせることによる、ハルシネーションの防止。

### 2.5 Multi-Agent Development (`Agent Teams`)
- `.agents/` ディレクトリを用いた、複数の AI（Team Lead, Teammate）による並行開発ワークフロー。
- ファイルシステムを介したタスク管理とレビュープロトコルの解説。

## 3. ナビゲーションの統合
- `Sidebar.tsx` に 「AI Integration」 という独立したカテゴリを設け、`ai_workflow.md` へのリンクを配置する。
- 配置場所は 「Experimental」 の手前とする。
