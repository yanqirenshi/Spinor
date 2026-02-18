# Step 33-B: Documentation Viewer - 技術仕様

## 1. 概要
`docs/` ディレクトリ内のドキュメント（Markdownファイル）を、個別に HTML 化することなく、Webブラウザ上で共通のデザイン（`style.css`）を適用して閲覧可能にするためのビューワーを構築する。

## 2. アーキテクチャ

### 2.1. クライアントサイド・レンダリング
サーバーサイドのビルドプロセスを不要にするため、ブラウザ上で Markdown を HTML に変換する方式を採用する。

- **変換ライブラリ:** [marked.js](https://marked.js.org/) (CDN経由) を使用。
- **データ取得:** `fetch` API を使用して、指定された Markdown ファイルを取得する。

### 2.2. docs/doc.html (ビューワー本体)
ドキュメント表示用の共通テンプレートとなる HTML ファイル。

- **デザインの一貫性:** `docs/index.html` と同じヘッダー、ナビゲーション、フッター、および `style.css` を適用する。
- **動的コンテンツ:** URLパラメータ `src` を解析し、表示すべきファイルを特定する。
    - 例: `doc.html?src=reference.md`
- **処理フロー:**
    1. ページロード時に `URLSearchParams` で `src` パラメータを取得。
    2. `fetch(src)` でドキュメントの内容を取得。
    3. `marked.parse(text)` で Markdown を HTML に変換。
    4. メイン領域の `id="content"` 要素に生成した HTML を挿入。
    5. ドキュメントのタイトル（h1タグのテキスト）をブラウザの `document.title` に反映。

## 3. リンクの更新
`docs/index.html` および各ドキュメント間のリンクを、以下の形式に変更する。

- 修正前: `<a href="reference.md">`
- 修正後: `<a href="doc.html?src=reference.md">`

## 4. セキュリティ・フォールバック
- 指定されたファイルが存在しない場合や取得に失敗した場合は、エラーメッセージを表示する。
- 基本的に同一ドメイン内の `docs/` 配下のファイルのみを許可する。
