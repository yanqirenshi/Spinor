# Spec 40: React Manual Site Setup

## 1. Overview

Spinor の公式リファレンスマニュアルサイトを構築する。このサイトは Common Lisp HyperSpec (CLHS) のような、検索可能でナビゲーションしやすいインターフェースを提供することを目標とする。
本仕様では、そのための SPA (Single Page Application) 基盤を Vite + React + TypeScript を用いてセットアップする。

## 2. Directory Structure

プロジェクトルートに `manual/` ディレクトリを新設し、Vite の `react-ts` テンプレートを用いてプロジェクトを初期化する。

```
Spinor/
├── manual/
│   ├── public/
│   │   └── (Markdown files for docs will be placed here)
│   ├── src/
│   │   ├── App.tsx
│   │   ├── main.tsx
│   │   └── ...
│   ├── package.json
│   └── vite.config.ts
└── ...
```

## 3. Technology Stack

- **Build Tool:** Vite
- **Framework:** React
- **Language:** TypeScript
- **Routing:** `react-router-dom`
- **Markdown Rendering:** `react-markdown` with `remark-gfm`
- **Syntax Highlighting:** `highlight.js`

## 4. Layout and UI/UX (CLHS Style)

アプリケーションは2カラムレイアウトを基本とする。

- **Sidebar (Left):** ナビゲーション用の目次。`Introduction`, `Syntax`, `API Reference`, `Index` などのセクションへのリンクを配置する。初期段階では静的なリストでよい。
- **Main Content (Right):** 選択されたページのコンテンツを表示するエリア。Markdown ファイルの内容がここにレンダリングされる。

## 5. Routing

`react-router-dom` を用いて以下のルーティングを実装する。

- `/`: ホームページ。マニュアルサイトの概要や導入を表示する。
- `/docs/:slug`: 各ドキュメントページ。URL の `:slug` 部分に対応する Markdown ファイル (`/manual/public/docs/:slug.md`) を動的に読み込み、表示する。

## 6. Content Delivery

- ドキュメントのソースは Markdown ファイルとする。
- Markdown ファイルは `manual/public/` ディレクトリ配下に配置する。これにより、アプリケーションは `fetch` API を用いて容易にファイル内容を取得できる。
