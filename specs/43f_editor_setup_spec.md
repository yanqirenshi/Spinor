# 43f: Editor Setup Guide (エディタ環境構築ガイド) 仕様書

## 1. 概要
Spinor を用いた開発において、効率的かつ対話的な環境を構築するためのガイドラインを提供する。特に Emacs を核とした Lisp 開発の伝統と、LSP および AI アシスタントを組み合わせた現代的なワークフローを解説する。

## 2. ドキュメント構成

### 2.1 Emacs & SLY Integration
- `spinor-mode` のインストールと設定。
- SLY を用いた TCP 経由の REPL 接続 (`M-x sly-connect`)。
- インタラクティブな評価 (`C-c C-c`, `C-x C-e`) のワークフロー。

### 2.2 Language Server Protocol (LSP)
- `spinor lsp` をバックエンドとした `lsp-mode` (または `eglot`) の設定。
- リアルタイムな型チェック、定義ジャンプ、ホバーによるドキュメント表示。

### 2.3 Project & Task Management
- Org mode を活用した文芸的プログラミング (Literate Programming) とプロジェクト管理。
- Spinor プロジェクト内でのタスク追跡と成果物の管理。

### 2.4 AI-Assisted Development
- **Claude Code / Gemini CLI:** コマンドラインからの自律的なコード修正、テスト実行、リファクタリング。
- **Google Antigravity:** プロジェクト全体のコードベース理解と設計支援。
- AI エージェントが Spinor の型システムやマクロを理解するためのコンテキスト設定のコツ。

## 3. ナビゲーション
- `Sidebar.tsx` の `Build Guide` の直後に `Editor Setup` を配置。
- ドキュメントパス: `manual/public/docs/emacs_setup.md`
