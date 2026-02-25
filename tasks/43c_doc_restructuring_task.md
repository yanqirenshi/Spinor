# Task 43c: Documentation Restructuring の実装

`syntax.md` の分割と、サイドバーの階層化表示を実装してください。

## ステップ

### 1. Markdown ファイルの分割
- `manual/public/docs/syntax/` ディレクトリを作成してください。
- `manual/public/docs/syntax.md` の内容を、`specs/43c_doc_restructuring_spec.md` で定義された 6 つのサブファイルに分割してください。

### 2. サイドバーコンポーネントの改修 (`Sidebar.tsx`)
- `manual/src/components/Sidebar.tsx` のリンク定義を階層構造に変更してください。
- 「Syntax」カテゴリの下にサブページ群を配置してください。
- 子項目には左パディングなどのスタイルを適用して階層を表現してください。

### 3. ルーティングの確認
- `manual/src/App.tsx` の `/docs/*` 経由で、`syntax/atoms` などの深い階層のファイルがフェッチできるか確認してください。

### 4. 既存リンクの修正
- `introduction.md` などからのリンクを新しいパスに修正してください。

### 5. 動作確認
- サイドバーの階層表示と、全てのサブページが正しく表示されることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**サイドバーのデータ構造:**
- `NavItem` インターフェースに `items?: NavItem[]` を追加し、ネスト可能な構造を実現
- カテゴリ項目は `to` (リンク先) と `items` (子要素) の両方を持てる設計
- 子要素の存在する場所への遷移時に自動展開する機能を実装

**Markdown 分割の方針:**
- 元の `syntax.md` の見出し構造に基づいて6つのトピックに分割
- 各ページ末尾に「次のステップ」として関連ページへのリンクを追加
- 元の `syntax.md` はサブページへのインデックスとして再構成

### 実装内容

**新規作成ファイル:**

| ファイル | 内容 |
|---------|------|
| `manual/public/docs/syntax/atoms.md` | 数値、シンボル、文字列、特殊シンボル |
| `manual/public/docs/syntax/type-system.md` | 型推論、多相性、基本型 |
| `manual/public/docs/syntax/evaluation.md` | リストの評価規則、クオート |
| `manual/public/docs/syntax/definitions.md` | def, fn, mac, クロージャ |
| `manual/public/docs/syntax/control-flow.md` | if, let, begin, setq |
| `manual/public/docs/syntax/data-types.md` | data, match, パターンマッチ |

**変更ファイル:**

| ファイル | 変更内容 |
|---------|---------|
| `manual/src/components/Sidebar.tsx` | `NavItem` インターフェース追加、`NavItemComponent` で階層表示を実装 |
| `manual/src/App.css` | サイドバー階層表示用のスタイル追加 (`.sidebar-category-*` クラス) |
| `manual/public/docs/syntax.md` | サブページへのインデックスページに変更 |
| `manual/public/docs/introduction.md` | Syntax リンクを `syntax/atoms` に更新 |
| `manual/public/docs/build.md` | Syntax リンクを `syntax/atoms` に更新 |

**React コンポーネント構造:**

```typescript
interface NavItem {
  label: string
  to?: string       // リンク先 (オプション)
  items?: NavItem[] // 子メニュー (オプション)
}

function NavItemComponent({ item, depth }: NavItemProps)
  // - hasChildren の場合: 展開/折りたたみボタン + リンク or ラベル
  // - それ以外: 通常のリンク
  // - depth に応じた左パディングで階層を視覚化
```

**UI 機能:**
- ▶/▼ アイコンで展開状態を表示
- 現在のパスに一致する子要素がある場合、自動的に親を展開
- カテゴリ名自体もクリック可能 (インデックスページへ遷移)
- ダークモード対応
