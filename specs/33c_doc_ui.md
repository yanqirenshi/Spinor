# Step 33-C: Documentation UI Overhaul - 技術仕様

## 1. 概要
Step 33-B で作成したドキュメントビューワー (`docs/doc.html`) の見た目を大幅に改善し、プログラミング言語の公式ドキュメントとして遜色のないレベルに引き上げる。

## 2. 目標
- **プロフェッショナルな外観:** Rust, Go, Elixir などの公式ドキュメントに匹敵する見た目
- **シンタックスハイライト:** Lisp/Spinor コードが見やすく色付けされる
- **直感的なナビゲーション:** サイドバーで各ドキュメント間を素早く移動

## 3. Visual Design (見た目の改善)

### 3.1. CSS Framework
- **採用:** `github-markdown-css` (CDN)
- **理由:** GitHub 風の洗練された Markdown スタイルを即座に適用できる
- **CDN URL:**
  ```html
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.5.1/github-markdown-light.min.css">
  ```

### 3.2. Layout (レイアウト構造)

```
┌─────────────────────────────────────────────────────────┐
│  Header (既存のナビゲーションバー)                       │
├──────────────┬──────────────────────────────────────────┤
│              │                                          │
│  Sidebar     │         Main Content                     │
│  (固定)      │         (スクロール可能)                  │
│              │                                          │
│  - Home      │    # Title                               │
│  - Reference │                                          │
│  - Emacs     │    ## Section                            │
│              │    ...                                   │
│              │                                          │
├──────────────┴──────────────────────────────────────────┤
│  Footer                                                 │
└─────────────────────────────────────────────────────────┘
```

### 3.3. サイドバー仕様
| 項目 | 仕様 |
|------|------|
| 幅 | 250px (デスクトップ)、非表示 (モバイル) |
| 位置 | 固定 (position: fixed または sticky) |
| 背景色 | `var(--bg-secondary)` |
| 内容 | ドキュメントリンクリスト |

### 3.4. レスポンシブ対応
- **デスクトップ (>768px):** サイドバー表示 + メインコンテンツ
- **モバイル (≤768px):** サイドバー非表示、ハンバーガーメニューでトグル (Optional)

## 4. Syntax Highlighting (コード色付け)

### 4.1. Library
- **採用:** `highlight.js` (CDN)
- **バージョン:** 11.x (最新安定版)
- **CDN URL:**
  ```html
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/github.min.css">
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
  <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/lisp.min.js"></script>
  ```

### 4.2. Language Support
- **Primary:** `lisp` (Spinor コード用)
- **Secondary:** `haskell`, `bash`, `javascript` (ドキュメント内の他のコード用)

### 4.3. Theme Options
| テーマ | 特徴 | 推奨用途 |
|--------|------|----------|
| `github` | ライトモード、GitHub 風 | **デフォルト採用** |
| `github-dark` | ダークモード | 将来のダークモード対応 |
| `atom-one-light` | Atom エディタ風 | 代替案 |

### 4.4. 適用方法
```javascript
// marked.parse() 後に呼び出す
document.querySelectorAll('pre code').forEach((block) => {
  hljs.highlightElement(block);
});
```

## 5. Navigation (ナビゲーション)

### 5.1. サイドバーリンク一覧
```html
<nav class="sidebar">
  <ul class="sidebar-nav">
    <li><a href="index.html">Home</a></li>
    <li><a href="doc.html?src=reference.md">Language Reference</a></li>
    <li><a href="doc.html?src=emacs.md">Emacs Integration</a></li>
  </ul>
</nav>
```

### 5.2. アクティブ状態の表示
- 現在表示中のドキュメントリンクをハイライト
- CSS クラス: `.active` を動的に付与

## 6. ファイル構成

### 変更対象
| ファイル | 変更内容 |
|----------|----------|
| `docs/doc.html` | CDN追加、HTML構造変更、JS更新 |
| `docs/style.css` | サイドバーレイアウト追加、既存スタイル調整 |

### 追加ファイル (なし)
- CDN を使用するため、新規ファイルは不要

## 7. パフォーマンス考慮

- **CDN Caching:** cdnjs の CDN はグローバルにキャッシュされる
- **Lazy Load:** highlight.js は DOMContentLoaded 後に実行
- **Bundle Size:** highlight.js (core + lisp) ≈ 40KB (gzip)

## 8. 将来の拡張
- ダークモード切り替え
- 目次 (Table of Contents) の自動生成
- ドキュメント内検索機能
- バージョン切り替え
