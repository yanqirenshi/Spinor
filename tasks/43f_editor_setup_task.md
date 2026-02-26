# 43f: Editor Setup Guide の実装指示書

## 1. 目的
Spinor の強力な開発環境（Emacs, LSP, AI）をユーザーが構築できるように、詳細なガイドを作成する。

## 2. 実装手順

### Step 1: ドキュメントの執筆
- `manual/public/docs/emacs_setup.md` を作成し、Specs に基づいて執筆する。
- 以下の Elisp スニペットを含めること：
    - `use-package` を使った `spinor-mode` と `sly` の設定。
    - `lsp-mode` で `spinor lsp` を認識させるための設定。
- AI ツール（Claude Code 等）をプロジェクトで活用するためのエイリアスや設定例を紹介する。

### Step 2: サイドバーへの登録
- `manual/src/components/Sidebar.tsx` を編集し、`sections` 配列の `Build Guide` の直後にリンクを追加する。
- ラベルは 「Editor Setup」 とする。

### Step 3: 整合性確認
- マニュアルサイトを実行し、エディタ設定の Elisp コードが正しくレンダリングされているか、リンクが機能しているかを確認する。

### Step 4: ビルド確認
- `cabal run spinor -- docgen` を実行し、マニュアル全体が整合性を持って出力されることを確認。

## 3. 完了条件
- `emacs_setup.md` が作成され、マニュアルからアクセス可能であること。
- Emacs (SLY/LSP) および AI ツールを用いた開発フローが具体的に解説されていること。

---
## 実装報告

### 実装方針

Emacs を中心としたエディタ環境構築ガイドを作成。初心者でもすぐに試せるように、`use-package` を使った具体的な Elisp 設定例を豊富に含め、SLY/LSP の両方のワークフローをカバー。AI アシスタント（Claude Code 等）との連携についても実践的な使用例を追加した。

### 実装内容

#### 作成ファイル

**`manual/public/docs/emacs_setup.md`**
- **Emacs & spinor-mode**
  - インストール方法
  - `use-package` を使った設定例
  - キーバインド一覧
  - REPL の起動と使用方法
- **SLY Integration**
  - Spinor サーバーの起動方法
  - SLY からの接続設定
  - キーバインド（`C-c C-c`, `M-.` 等）
- **LSP Mode**
  - `lsp-mode` の設定（Spinor LSP クライアント登録）
  - `eglot` の設定（軽量な代替）
  - 機能一覧（Diagnostics, Hover, Completion）
- **Project Management**
  - Projectile との統合
  - Org mode によるリテラルプログラミング
  - タスク管理の例
- **AI-Assisted Development**
  - Claude Code のインストールと使用方法
  - CLAUDE.md によるコンテキスト設定
  - Gemini CLI の紹介
  - gptel による Emacs + AI 統合
- **完全な設定例**
  - 上記すべてを統合した `init.el` のサンプル
- **トラブルシューティング**
  - REPL, LSP, SLY の接続問題への対処法

#### 変更ファイル

**`manual/src/components/Sidebar.tsx`**
- `Build Guide` の直後に `Editor Setup` へのリンクを追加

#### docgen 結果

```
Generated 70 reference files.
Documentation generated successfully.
```

### 完了日
2026-02-26
