# Step 33-D: Add Project Logo - 技術仕様

## 1. 概要
ユーザーが作成したロゴ画像 `spinor.png` をプロジェクト全体に統合し、ブランディングを強化する。
README、ドキュメントサイトのヘッダーにロゴを配置する。

## 2. アセット配置

### 2.1. ディレクトリ構造
```
docs/
├── assets/
│   └── spinor.png    ← ロゴ画像
├── index.html
├── doc.html
└── style.css
```

### 2.2. 画像要件
| 項目 | 推奨値 |
|------|--------|
| フォーマット | PNG (透過対応) |
| 解像度 | 512x512 以上 (高DPI対応) |
| ファイルサイズ | 100KB 以下 (最適化済み) |

## 3. README.md の更新

### 3.1. ロゴ配置位置
- ファイルの最上部（タイトル `# Spinor` の前）
- センタリング表示

### 3.2. マークアップ
```markdown
<p align="center">
  <img src="docs/assets/spinor.png" width="200" alt="Spinor Logo">
</p>

# Spinor (スピノル)
```

### 3.3. 考慮事項
- `width="200"` で適切なサイズに調整（原寸が大きい場合）
- `alt` 属性でアクセシビリティ確保
- GitHub 上でも正しく表示されることを確認

## 4. ドキュメントサイト (docs/) の更新

### 4.1. ヘッダーレイアウト変更

**Before:**
```
┌─────────────────────────────────────────────────┐
│  Spinor (text)     Reference | Emacs | GitHub  │
└─────────────────────────────────────────────────┘
```

**After:**
```
┌─────────────────────────────────────────────────┐
│  [Logo] Spinor     Reference | Emacs | GitHub  │
└─────────────────────────────────────────────────┘
```

### 4.2. HTML 変更 (index.html, doc.html)

```html
<a href="index.html" class="logo">
  <img src="assets/spinor.png" alt="Spinor" class="logo-img">
  <span class="logo-text">Spinor</span>
</a>
```

### 4.3. CSS スタイル (style.css)

```css
/* Logo styling */
.logo {
  display: flex;
  align-items: center;
  gap: 0.5rem;
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
}

.logo:hover .logo-text {
  color: var(--accent-light);
}
```

### 4.4. レスポンシブ対応
| 画面幅 | ロゴ表示 |
|--------|----------|
| > 768px | アイコン + テキスト |
| ≤ 768px | アイコンのみ (テキスト非表示も検討) |

## 5. パフォーマンス考慮

### 5.1. 画像最適化
- PNG の場合: `pngquant` や `optipng` で圧縮
- SVG への変換も検討 (スケーラビリティ向上)

### 5.2. キャッシュ
- 静的アセットとして長期キャッシュ可能

## 6. ファビコン (将来の拡張)
- `docs/favicon.ico` または `docs/favicon.png` を追加
- `<link rel="icon" href="favicon.png">` を HTML に追加
- 本 Step では対象外とするが、同じロゴアセットから派生可能

## 7. 対象ファイル一覧

| ファイル | 変更内容 |
|----------|----------|
| `docs/assets/spinor.png` | 新規追加 (ロゴ画像) |
| `README.md` | ロゴ画像を挿入 |
| `docs/index.html` | ヘッダーにロゴ追加 |
| `docs/doc.html` | ヘッダーにロゴ追加 |
| `docs/style.css` | ロゴ用スタイル追加 |
