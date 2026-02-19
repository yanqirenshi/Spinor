# Step 33-D: Add Project Logo - 実装指示書

## 概要
ユーザーが作成したロゴ画像 `spinor.png` をプロジェクトに統合してください。
README とドキュメントサイトのヘッダーにロゴを配置します。

## 前提条件
- ユーザーがロゴ画像 `spinor.png` を提供済みであること
- 画像がプロジェクトルートまたは指定された場所に存在すること

## Steps

### 1. アセットディレクトリの作成とファイル配置

```bash
mkdir -p docs/assets
# ユーザーが提供した spinor.png を docs/assets/ に配置
cp spinor.png docs/assets/spinor.png
```

### 2. README.md の更新

ファイルの最上部（`# Spinor` の前）に以下を追加してください。

```markdown
<p align="center">
  <img src="docs/assets/spinor.png" width="200" alt="Spinor Logo">
</p>

# Spinor (スピノル)
```

**注意:** 既存の `# Spinor (スピノル)` 行は残し、その上にロゴを挿入します。

### 3. docs/index.html の更新

ヘッダー内のロゴ部分を以下のように変更してください。

**Before:**
```html
<a href="#" class="logo">Spinor</a>
```

**After:**
```html
<a href="index.html" class="logo">
  <img src="assets/spinor.png" alt="Spinor" class="logo-img">
  <span class="logo-text">Spinor</span>
</a>
```

### 4. docs/doc.html の更新

同様に、ヘッダー内のロゴ部分を変更してください。

**Before:**
```html
<a href="index.html" class="logo">Spinor</a>
```

**After:**
```html
<a href="index.html" class="logo">
  <img src="assets/spinor.png" alt="Spinor" class="logo-img">
  <span class="logo-text">Spinor</span>
</a>
```

### 5. docs/style.css の更新

`.logo` セクションを以下のように更新してください。

**既存のスタイルを置き換え:**
```css
.logo {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  text-decoration: none;
}

.logo-img {
  height: 32px;
  width: auto;
}

.logo-text {
  font-size: 1.35rem;
  font-weight: 700;
  color: var(--text-primary);
  letter-spacing: -0.02em;
  transition: color 0.2s;
}

.logo:hover .logo-text {
  color: var(--accent-light);
}
```

**レスポンシブ対応 (768px 以下):**
```css
@media (max-width: 768px) {
  .logo-text {
    /* モバイルでもテキスト表示 (必要なら非表示に変更可) */
    font-size: 1.2rem;
  }

  .logo-img {
    height: 28px;
  }
}
```

### 6. 検証手順

#### 6.1. ローカル確認
```bash
cd docs
python -m http.server 8000
```

ブラウザで以下を開いて確認:
- `http://localhost:8000/index.html`
- `http://localhost:8000/doc.html?src=reference.md`

#### 6.2. GitHub 確認
- README.md のプレビューでロゴが表示されるか確認
- `docs/assets/spinor.png` へのパスが正しいか確認

#### 6.3. チェックリスト
- [ ] `docs/assets/spinor.png` が存在する
- [ ] README.md のトップにロゴが表示される (GitHub 上)
- [ ] `docs/index.html` のヘッダーにロゴが表示される
- [ ] `docs/doc.html` のヘッダーにロゴが表示される
- [ ] ロゴとテキストが縦方向に揃っている
- [ ] ホバー時にテキストの色が変わる
- [ ] モバイル幅でも適切に表示される

---

## 実装報告

### Implementation Policy (実装方針)

- **画像フォーマット:** ユーザー提供の PNG (`spinor-logo.png`) を使用。
- **配置パス:** `docs/assets/spinor-logo.png` に配置。README からは `docs/assets/spinor-logo.png`、docs 内の HTML からは `assets/spinor-logo.png` の相対パスで参照。
- **CSS 方針:**
  - **ヘッダー:** 既存の `.logo` クラスを flexbox ベースに変更し、`.logo-img` (32px) と `.logo-text` の子要素でロゴ画像とテキストを横並び配置。
  - **Hero セクション:** タイトル「Spinor」の上にロゴ画像を独立要素として配置 (`.hero-logo`, height: 8em)。
- **レスポンシブ:** 768px 以下でヘッダーロゴを 28px、テキストを 1.2rem に縮小。

### Implementation Details (実装内容)

**変更ファイル一覧:**

| ファイル | 変更内容 |
|----------|----------|
| `docs/assets/spinor-logo.png` | ユーザー提供のロゴ画像を配置 |
| `README.md` | 先頭にセンタリングされたロゴ画像を追加 (`<p align="center">`) |
| `docs/index.html` | ヘッダーの `<a class="logo">` を `<img>` + `<span>` 構造に変更。Hero セクションのタイトル上にロゴ画像を追加 |
| `docs/doc.html` | ヘッダーの `<a class="logo">` を `<img>` + `<span>` 構造に変更 |
| `docs/style.css` | `.logo` を flexbox に変更、`.logo-img` / `.logo-text` / `.hero-logo` スタイル追加、レスポンシブ対応追加 |

**ローカルサーバーでの表示確認:**
- `docs/index.html`: ヘッダー左側にロゴアイコン + "Spinor" テキストが横並びで表示される。Hero セクションではタイトルの上にロゴ画像 (8em) が表示される
- `docs/doc.html`: ヘッダーにロゴアイコン + テキストが表示される
- `README.md`: GitHub 上でタイトル上部にセンタリングされたロゴが表示される
- ホバー時にヘッダーのテキスト色が `var(--accent-light)` に変化する
- 768px 以下でヘッダーロゴサイズが適切に縮小される
