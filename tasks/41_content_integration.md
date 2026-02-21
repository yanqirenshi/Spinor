# Task 41: Content Architecture & DocGen Integration

## 1. Objective

`specs/41_content_integration.md` の仕様に基づき、Haskell 製ドキュメントジェネレーター (`DocGen.hs`) と React マニュアルサイトを統合する。
`DocGen.hs` の出力先とリンク形式を改修し、React アプリ側でネストしたパスを扱えるようにルーティングを修正する。

## 2. Implementation Steps

### Step 1: Modify `src/Spinor/DocGen.hs`

`DocGen.hs` を開き、出力ディレクトリとリンク形式を変更してください。

- **出力ディレクトリの作成:** `createDirectoryIfMissing True "docs/ref"` を `createDirectoryIfMissing True "manual/public/ref"` に変更します。
- **個別ファイルのパス:** `generateEntryFile` 内の `path` を `"manual/public/ref/" ++ T.unpack slug ++ ".md"` に変更します。
- **インデックスファイルのパス:** `generateIndexFile` 内の `TIO.writeFile` の書き込み先を `"docs/reference.md"` から `"manual/public/docs/api-index.md"` に変更します。
- **リンク形式:** `renderLink` 内の `- [" <> name <> "](doc.html?src=ref/" <> docSlug entry <> ".md)"` を `- [" <> name <> "](/docs/ref/" <> docSlug entry <> ")"` に変更します。（URL の末尾の `.md` は不要です）

### Step 2: Run DocGen and Verify

プロジェクトルートで `docgen` コマンドを実行し、ファイルが期待通りに生成されることを確認してください。

```sh
cabal run spinor -- docgen
```

- `manual/public/ref/` ディレクトリに `.md` ファイルが生成されていること。
- `manual/public/docs/api-index.md` ファイルが生成されていること。
- `api-index.md` 内のリンクが `/docs/ref/...` の形式になっていること。

### Step 3: Update React Router in `App.tsx`

`manual/src/App.tsx` のルーティング定義を、ネストされたパスに対応できるよう変更します。

- `<Route path="/docs/:slug" element={<MarkdownViewer />} />` を以下の行に置き換えてください。
  ```tsx
  <Route path="/docs/*" element={<MarkdownViewer />} />
  ```

### Step 4: Update `MarkdownViewer.tsx`

`manual/src/components/MarkdownViewer.tsx` を修正し、ワイルドカードで渡されたパスからファイルを `fetch` できるようにします。

- `useParams` の使い方を `const { slug } = useParams...` から `const { '*': path } = useParams<{ '*': string }>()` に変更します。
- `useEffect` の `fetch` URL を `fetch(`/docs/${slug}.md`)` から `fetch(`/${path}.md`)` に変更します。
- `useEffect` の依存配列を `[slug]` から `[path]` に変更します。

### Step 5: Create Static Content Placeholders

静的なドキュメントのプレースホルダーを作成します。

- **File:** `manual/public/docs/introduction.md`
  - **Content:** `# Introduction`
- **File:** `manual/public/docs/syntax.md`
  - **Content:** `# Syntax`

### Step 6: Update `Sidebar.tsx`

`manual/src/components/Sidebar.tsx` を編集し、新しいコンテンツ構造に合わせてリンクを更新します。

- `sections` 配列を以下のように変更してください。

  ```tsx
  const sections = [
    { label: 'Home', to: '/' },
    { label: 'Introduction', to: '/docs/introduction' },
    { label: 'Syntax', to: '/docs/syntax' },
    { label: 'API Reference', to: '/docs/api-index' },
    // { label: 'Sample', to: '/docs/sample' }, // sample は不要であれば削除
  ]
  ```

### Step 7: Verification

`manual` ディレクトリで開発サーバーを起動し、全体の動作を確認してください。

```sh
cd manual
npm run dev
```

- ブラウザでサイドバーの "Introduction", "Syntax", "API Reference" をクリックし、各ページが表示されることを確認します。
- "API Reference" ページ (`/docs/api-index`) に表示されたリンクをクリックし、個別の API ドキュメント（例: `/docs/ref/add`）に正しく遷移できることを確認します。

## 3. Implementation Report

このタスクが完了したら、この Markdown ファイルの末尾に以下のセクションを追加し、実装内容を報告してください。

### 実装方針

- 仕様書 (`specs/41_content_integration.md`) に従い、DocGen の出力先を `manual/public/` 配下に変更し、React SPA のルーティングをワイルドカード方式に統一するアーキテクチャを採用した。
- `manual/public/` を Vite の静的ファイル配信ルートとして活用し、`docs/` (静的コンテンツ + api-index) と `ref/` (DocGen 生成の API リファレンス) を分離する構造とした。
- React Router のワイルドカードルート (`/docs/*`) を導入し、`/docs/introduction`、`/docs/ref/add` のようなネストしたパスを単一の `MarkdownViewer` コンポーネントで処理する。

### 実装内容

**変更したファイル:**

1. **`src/Spinor/DocGen.hs`** — 出力ディレクトリを `docs/` → `manual/public/` に変更。インデックスファイルのパスを `docs/reference.md` → `manual/public/docs/api-index.md` に変更。リンク形式を `doc.html?src=ref/<slug>.md` → `/docs/ref/<slug>` に変更。
2. **`manual/src/App.tsx`** — Route を `/docs/:slug` → `/docs/*` (ワイルドカードルート) に変更。
3. **`manual/src/components/MarkdownViewer.tsx`** — `useParams` を `{ slug }` → `{ '*': path }` に変更。`fetch` URL を `/docs/${slug}.md` → `/${path}.md` に変更。依存配列も `[path]` に更新。
4. **`manual/src/components/Sidebar.tsx`** — ナビゲーションリンクを仕様に合わせて `Home`, `Introduction`, `Syntax`, `API Reference` の 4 項目に整理。`API Reference` のリンク先を `/docs/api-index` に変更。
5. **`cabal.project`** — WSL2 環境でのビルドエラー (Windows パスの `network` パッケージ設定) を修正。

**追加したファイル:**

6. **`manual/public/docs/introduction.md`** — Introduction ページのプレースホルダー。
7. **`manual/public/docs/syntax.md`** — Syntax ページのプレースホルダー。

**生成されたファイル (DocGen 実行結果):**

- `manual/public/ref/` — 43 個の API リファレンス `.md` ファイル (add.md, sub.md, cons.md 等)
- `manual/public/docs/api-index.md` — カテゴリ別にリンクが整理された API インデックス

**確認結果:**

- `cabal run spinor -- docgen` → 43 ファイル正常生成。`api-index.md` 内のリンクが `/docs/ref/<slug>` 形式であることを確認。
- `npm run build` (manual/) → TypeScript コンパイル・Vite ビルドともに成功。
