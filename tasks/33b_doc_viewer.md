# Step 33-B: Documentation Viewer - 実装指示書

## 概要
Markdown ドキュメントをブラウザで美しく表示するための `doc.html` を作成し、リンクを整備してください。

## Steps

### 1. docs/doc.html の作成
- `docs/index.html` の構造（Header, Nav, Footer）をコピーして作成する。
- メインコンテンツ領域を空の `<section id="content" class="markdown-body"></section>` に置き換える。
- `marked.js` を読み込む script タグを追加する。
- 以下の JavaScript ロジックを実装する:
    - URL の `src` パラメータを取得。
    - Markdown を fetch。
    - HTML に変換して `#content` に注入。
- `style.css` のスタイルが Markdown 由来のタグ（h1, h2, ul, code等）に適用されるように調整する。

### 2. docs/index.html のリンク修正
- ドキュメントへのリンクを `doc.html?src=...` 形式に更新する。
- 例: `Reference` -> `doc.html?src=reference.md`
- 例: `Emacs Guide` -> `doc.html?src=emacs.md`

### 3. スタイルの微調整 (docs/style.css)
- `markdown-body` クラス（またはそれに類するコンテナ）内の要素に対して、余白やフォントサイズなどの一般的な Markdown スタイルを追加する。
- 特に `pre > code` のシンタックスハイライト（Step 33 で定義したカラー）が機能することを確認する。

### 4. 動作確認
- ローカルサーバーを起動: `python -m http.server` または `npx http-server`.
- ブラウザで `index.html` を開き、リンクをクリックして各 Markdown ファイルの内容が正しく表示されることを確認する。

---
## 実装報告

### 実装方針
- **marked.js:** CDN 経由で読み込み (`https://cdn.jsdelivr.net/npm/marked/marked.min.js`)
- **セキュリティ対策:**
  - ディレクトリトラバーサル防止: `src` パラメータを正規表現 `/^[a-zA-Z0-9_-]+\.md$/` で検証
  - パス区切り文字（`/`, `..`）を含むソースは拒否
  - 同一ドメイン内の `.md` ファイルのみ許可
- **エラーハンドリング:** ファイル取得失敗時にエラーメッセージを表示し、ホームへのリンクを提供

### 実装内容

#### 1. docs/doc.html
- `index.html` と共通のヘッダー/フッター構造を採用
- メインコンテンツ領域: `<article id="content" class="markdown-body">`
- JavaScript 処理フロー:
  1. `URLSearchParams` で `src` パラメータを取得
  2. `isValidSource()` でセキュリティ検証
  3. `fetch(src)` でMarkdownを取得
  4. `marked.parse()` でHTMLに変換
  5. `#content` に挿入
  6. 最初の `<h1>` から `document.title` を設定

#### 2. docs/index.html の変更
- ナビゲーションにドキュメントリンクを追加:
  - `Reference` → `doc.html?src=reference.md`
  - `Emacs` → `doc.html?src=emacs.md`

#### 3. docs/style.css の追加スタイル
- `.markdown-body` コンテナ: 最大幅800px、適切な余白とline-height
- 見出し (`h1`-`h4`): サイズ・マージン・ボーダー
- コードブロック (`pre > code`): 既存のシンタックスハイライトカラーを継承
- テーブル、引用、リストなどの標準Markdownスタイル
- `.doc-error` クラス: エラー表示用スタイル

#### 変更ファイル一覧
| ファイル | 変更 |
|---------|------|
| `docs/doc.html` | 新規作成 |
| `docs/index.html` | ナビゲーションにReference/Emacsリンク追加 |
| `docs/style.css` | Markdown用スタイル追加（約120行）|

#### 動作確認
- ローカルサーバー起動: `python -m http.server 8000`
- `http://localhost:8000/docs/index.html` → Reference/Emacs リンクをクリック
- `http://localhost:8000/docs/doc.html?src=reference.md` → Language Reference が表示される
- `http://localhost:8000/docs/doc.html?src=emacs.md` → Emacs Integration が表示される
- 不正なソース（`doc.html?src=../secret.md`）→ エラーメッセージが表示される
