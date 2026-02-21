# Task 40: React Manual Site Setup

## 1. Objective

Spinor の公式マニュアルサイトの SPA 基盤を構築する。
`specs/40_manual_setup.md` の仕様に基づき、Vite + React (TypeScript) プロジェクトをセットアップし、基本的なレイアウトとルーティング、Markdown 表示機能を実装する。

## 2. Implementation Steps

### Step 1: Initialize Vite Project

プロジェクトルートで以下のコマンドを実行し、`manual` ディレクトリに React (TypeScript) プロジェクトを作成してください。

```sh
npm create vite@latest manual -- --template react-ts
```

### Step 2: Install Dependencies

作成された `manual` ディレクトリに移動し、必要な npm パッケージをインストールしてください。

```sh
cd manual
npm install react-router-dom react-markdown remark-gfm highlight.js
npm install @types/highlight.js --save-dev
```

*Note: シンタックスハイライトには `highlight.js` を使用します。*

### Step 3: Implement Basic Layout and Routing

`manual/src/App.tsx` を編集し、基本的な2カラムレイアウトとルーティングを実装してください。

- `react-router-dom` を用いて `BrowserRouter` をセットアップします。
- 左側に `Sidebar` コンポーネント、右側に `Main Content` を表示するレイアウトを作成します。
- ルート `/` と `/docs/:slug` を定義します。

### Step 4: Create Markdown Viewer Component

URL の `:slug` パラメータに基づいて、`public/` ディレクトリから対応する Markdown ファイルを動的に取得して表示するコンポーネントを作成します。

- **File:** `manual/src/components/MarkdownViewer.tsx`
- **Functionality:**
    - `useParams` フックで `slug` を取得します。
    - `useEffect` フック内で `fetch` API を使い、`/docs/${slug}.md` の内容を読み込みます。（`public` ディレクトリはルートとして扱われます）
    - 取得した Markdown テキストを `react-markdown` コンポーネントでレンダリングします。
    - `remark-gfm` プラグインを適用して GFM (GitHub Flavored Markdown) をサポートします。
    - `highlight.js` を使ってコードブロックのシンタックスハイライトを有効にしてください。

### Step 5: Create Sample Content

動作確認のため、サンプル用の Markdown ファイルを作成します。

- **File:** `manual/public/docs/sample.md`
- **Content:**
  ```markdown
  # Sample Document

  This is a sample document for testing.

  ## Code Block

  ```lisp
  (def (add x y) (+ x y))
  (print (add 1 2))
  ```

  - List item 1
  - List item 2
  ```

### Step 6: Verification

`manual` ディレクトリで以下のコマンドを実行し、開発サーバーを起動してください。

```sh
npm run dev
```

ブラウザで以下の URL にアクセスし、表示が正しいことを確認してください。

- `http://localhost:5173/` (または Vite が指定したポート)
- `http://localhost:5173/docs/sample`

`/docs/sample` では、`sample.md` の内容がシンタックスハイライト付きで表示されることを確認してください。

## 3. Implementation Report

このタスクが完了したら、この Markdown ファイルの末尾に以下のセクションを追加し、実装内容を報告してください。

### 実装方針

- Vite の `react-ts` テンプレートで `manual/` ディレクトリにプロジェクトを生成し、仕様書通りの技術スタックを採用した。
- `BrowserRouter` を `main.tsx` に配置し、`App.tsx` でルーティング定義と2カラムレイアウト (Sidebar + Main Content) を一元管理する構成とした。
- `MarkdownViewer` コンポーネントは URL パラメータ `:slug` に基づいて `public/docs/` 配下の Markdown ファイルを `fetch` で取得し、`react-markdown` + `remark-gfm` でレンダリングする。
- シンタックスハイライトは `highlight.js` を使用し、Markdown レンダリング後に `hljs.highlightElement()` で適用する方式とした。
- CSS は CLHS スタイルを意識したシンプルな2カラムレイアウトとし、ダークモードにも対応した。

### 実装内容

- **新規作成ファイル:**
  - `manual/` - Vite + React (TypeScript) プロジェクト全体 (`npm create vite@latest manual -- --template react-ts`)
  - `manual/src/components/Sidebar.tsx` - ナビゲーション用サイドバー (Home, Introduction, Syntax, API Reference, Index, Sample へのリンク)
  - `manual/src/components/HomePage.tsx` - ルート (`/`) 用のホームページコンポーネント
  - `manual/src/components/MarkdownViewer.tsx` - `/docs/:slug` 用の Markdown 表示コンポーネント (fetch + react-markdown + highlight.js)
  - `manual/public/docs/sample.md` - 動作確認用サンプルドキュメント
- **変更ファイル:**
  - `manual/src/main.tsx` - `BrowserRouter` でラップ
  - `manual/src/App.tsx` - 2カラムレイアウト + `Routes` による `/` と `/docs/:slug` のルーティング
  - `manual/src/App.css` - 2カラムレイアウト用 CSS (サイドバー 240px + メインコンテンツ flex)
  - `manual/src/index.css` - `body` の `display: flex; place-items: center` を除去 (ドキュメントサイト用に修正)
- **インストールした依存パッケージ:**
  - `react-router-dom`, `react-markdown`, `remark-gfm`, `highlight.js` (dependencies)
  - `@types/highlight.js` (devDependencies)
- **検証:** `npm run build` (TypeScript コンパイル + Vite ビルド) がエラーなく完了することを確認済み。
