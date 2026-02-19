# Step 33-C: Documentation UI Overhaul - 実装指示書

## 概要
ドキュメントビューワー (`docs/doc.html`) の見た目を改善し、シンタックスハイライトとサイドバーナビゲーションを追加してください。

## Steps

### 1. CDN ライブラリの追加 (docs/doc.html)

`<head>` セクションに以下を追加してください。

```html
<!-- GitHub Markdown CSS -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown-light.min.css">

<!-- Highlight.js Theme -->
<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css">
```

`</body>` の直前に以下を追加してください。

```html
<!-- Highlight.js Core + Languages -->
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/lisp.min.js"></script>
<script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js"></script>
```

### 2. HTML 構造の変更 (docs/doc.html)

`<main>` セクションを以下の構造に変更してください。

```html
<main class="doc-layout">
  <!-- Sidebar Navigation -->
  <aside class="doc-sidebar">
    <nav class="sidebar-nav">
      <h3>Documentation</h3>
      <ul>
        <li><a href="index.html">Home</a></li>
        <li><a href="doc.html?src=reference.md" data-src="reference.md">Language Reference</a></li>
        <li><a href="doc.html?src=emacs.md" data-src="emacs.md">Emacs Integration</a></li>
      </ul>
    </nav>
  </aside>

  <!-- Main Content -->
  <section class="doc-main">
    <article id="content" class="markdown-body">
      <p>Loading...</p>
    </article>
  </section>
</main>
```

### 3. CSS レイアウトの追加 (docs/style.css)

以下のスタイルを追加してください。

```css
/* ===== Documentation Layout ===== */
.doc-layout {
  display: flex;
  min-height: calc(100vh - 64px - 80px); /* header + footer */
  padding-top: 64px; /* header height */
}

/* Sidebar */
.doc-sidebar {
  width: 250px;
  flex-shrink: 0;
  background: var(--bg-secondary);
  border-right: 1px solid var(--border);
  position: sticky;
  top: 64px;
  height: calc(100vh - 64px);
  overflow-y: auto;
}

.sidebar-nav {
  padding: 1.5rem;
}

.sidebar-nav h3 {
  font-size: 0.75rem;
  font-weight: 600;
  text-transform: uppercase;
  letter-spacing: 0.05em;
  color: var(--text-secondary);
  margin-bottom: 1rem;
}

.sidebar-nav ul {
  list-style: none;
  padding: 0;
  margin: 0;
}

.sidebar-nav li {
  margin-bottom: 0.25rem;
}

.sidebar-nav a {
  display: block;
  padding: 0.5rem 0.75rem;
  border-radius: 6px;
  font-size: 0.9rem;
  color: var(--text-primary);
  transition: background-color 0.15s, color 0.15s;
}

.sidebar-nav a:hover {
  background: var(--accent-glow);
  color: var(--accent-light);
}

.sidebar-nav a.active {
  background: var(--accent-glow);
  color: var(--accent);
  font-weight: 500;
}

/* Main Content */
.doc-main {
  flex: 1;
  min-width: 0;
  padding: 2rem 3rem;
  max-width: 900px;
}

/* Override #doc-content padding (remove old styles) */
#doc-content {
  padding-top: 0;
  min-height: auto;
}

/* ===== Highlight.js Overrides ===== */
.markdown-body pre {
  background: #f6f8fa;
  border-radius: 8px;
  padding: 1rem;
}

.markdown-body pre code {
  font-size: 0.875rem;
  line-height: 1.6;
}

/* ===== Responsive: Mobile ===== */
@media (max-width: 900px) {
  .doc-sidebar {
    display: none;
  }

  .doc-main {
    padding: 1.5rem;
  }
}
```

### 4. JavaScript 更新 (docs/doc.html)

既存の `loadDocument` 関数を更新し、highlight.js を適用してください。

```javascript
async function loadDocument() {
  if (!isValidSource(src)) {
    showError('Invalid document source. Please specify a valid Markdown file.');
    return;
  }

  try {
    const response = await fetch(src);
    if (!response.ok) {
      throw new Error('Document not found: ' + src);
    }

    const markdown = await response.text();
    const html = marked.parse(markdown);

    const content = document.getElementById('content');
    content.innerHTML = html;

    // Extract title from first h1 and set document title
    const firstH1 = content.querySelector('h1');
    if (firstH1) {
      document.title = firstH1.textContent + ' - Spinor';
    }

    // Apply syntax highlighting
    content.querySelectorAll('pre code').forEach((block) => {
      hljs.highlightElement(block);
    });

    // Update active nav link
    updateActiveLink();

  } catch (error) {
    showError(error.message);
  }
}

// Highlight active navigation link
function updateActiveLink() {
  const links = document.querySelectorAll('.sidebar-nav a[data-src]');
  links.forEach(link => {
    link.classList.remove('active');
    if (link.getAttribute('data-src') === src) {
      link.classList.add('active');
    }
  });
}
```

### 5. Lisp コードブロックの指定 (docs/*.md)

`reference.md` などのドキュメントで、Spinor コードには ` ```lisp ` を指定してください。

```markdown
​```lisp
(def greet (fn (name)
  (string-append "Hello, " name "!")))

(print (greet "World"))
​```
```

### 6. 検証手順

#### 6.1. ローカルサーバーで確認
```bash
cd docs
python -m http.server 8000
# または
npx serve .
```

ブラウザで以下を開いて確認:
- `http://localhost:8000/doc.html?src=reference.md`
- `http://localhost:8000/doc.html?src=emacs.md`

#### 6.2. チェックリスト
- [ ] サイドバーが左側に固定表示されている
- [ ] サイドバーのリンクが正しく動作する
- [ ] 現在のドキュメントがサイドバーでハイライトされている
- [ ] Lisp コードブロックがシンタックスハイライトされている
- [ ] モバイル幅 (900px以下) でサイドバーが非表示になる
- [ ] スクロール時にサイドバーが固定されたままである

---

## 実装報告

### Implementation Policy (実装方針)

- **テーマ:** GitHub Light テーマを採用。`github-markdown-css` (v5.5.1) で Markdown の見た目を GitHub 風に統一し、`highlight.js` (v11.9.0) の `github` テーマでコードブロックを色付け。
- **レスポンシブ対応:** 900px 以下でサイドバーを `display: none` で非表示にし、メインコンテンツのパディングを縮小。デスクトップではサイドバーを `position: sticky` で固定表示。

### Implementation Details (実装内容)

#### `docs/doc.html` の変更点
1. **`<head>` に CDN リンクを追加:**
   - `github-markdown-css` (5.5.1) — Markdown スタイル
   - `highlight.js/styles/github.min.css` (11.9.0) — シンタックスハイライトテーマ
2. **`</body>` 前に highlight.js スクリプトを追加:**
   - `highlight.min.js` (コアライブラリ)
   - `languages/lisp.min.js` (Spinor コード用)
   - `languages/haskell.min.js` (Haskell コード用)
3. **HTML 構造を変更:**
   - `<main>` を `<main class="doc-layout">` (flex コンテナ) に変更
   - `<aside class="doc-sidebar">` にサイドバーナビゲーション (Home, Language Reference, Emacs Integration) を追加
   - メインコンテンツを `<section class="doc-main">` に配置
4. **JavaScript を更新:**
   - `loadDocument()` 内で `hljs.highlightElement()` を全 `<pre><code>` ブロックに適用
   - `updateActiveLink()` 関数を追加し、URL の `src` パラメータに基づいてサイドバーのアクティブリンクをハイライト

#### `docs/style.css` の変更点
1. **Documentation Layout セクションを追加:**
   - `.doc-layout` — flex コンテナ (header 高さ分の `padding-top: 64px`)
   - `.doc-sidebar` — 幅 250px、`position: sticky`、背景色 `var(--bg-secondary)`
   - `.sidebar-nav` — リンクスタイル (ホバー・アクティブ状態のハイライト)
   - `.doc-main` — `flex: 1`、`max-width: 900px`
2. **Highlight.js Overrides セクションを追加:**
   - `.markdown-body pre` — 背景色・角丸を highlight.js テーマに合わせて調整
3. **レスポンシブルールを追加:**
   - `@media (max-width: 900px)` でサイドバー非表示、コンテンツパディング縮小

#### ブラウザでの表示確認結果
- `python3 -m http.server 8000` でローカルサーバーを起動
- `http://localhost:8000/doc.html?src=reference.md` および `?src=emacs.md` に対して HTTP 200 を確認
- HTML 構造: サイドバー (`<aside class="doc-sidebar">`) が正しく出力されている
- JavaScript: `hljs.highlightElement()` 呼び出しと `updateActiveLink()` が `loadDocument()` 内に含まれている
- CSS: サイドバーレイアウト、アクティブリンクスタイル、レスポンシブルール (900px) が適用されている
- 既存の Markdown ファイル (`reference.md`, `emacs.md`) は既に ` ```lisp ` / ` ```elisp ` / ` ```bash ` でコードブロックを指定済みのため、追加修正は不要
