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

- （ここに実装の概要やアーキテクチャ上の判断を記述）

### 実装内容

- （変更したファイル、追加したコンポーネント、直面した課題などを具体的に記述）
