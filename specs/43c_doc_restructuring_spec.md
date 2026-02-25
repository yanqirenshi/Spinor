# Spec 43c: Documentation Restructuring

## 概要
マニュアルサイトのコンテンツ増加に伴い、単一ファイルだった `syntax.md` を複数のサブページに分割し、React サイドバーを階層型（ネストメニュー）に拡張する。

## 1. コンテンツの再構成
`manual/public/docs/syntax.md` の内容を、以下のディレクトリ構造に分割・展開する。

```text
manual/public/docs/
  └── syntax/
      ├── atoms.md         # 数値、シンボル、文字列、nil/t
      ├── type-system.md   # 型推論、多相性、基本型
      ├── evaluation.md    # リストの評価規則、クオート
      ├── definitions.md   # def, fn, mac, クロージャ
      ├── control-flow.md  # if, let, begin, setq
      └── data-types.md    # data, match, リストのマッチ
```

## 2. サイドバーの階層化設計 (`Sidebar.tsx`)
サイドバーの項目を定義するデータ構造を拡張し、子要素 (`items`) を持てるようにする。

```typescript
interface NavItem {
  label: string;
  to?: string;      // リンク先
  items?: NavItem[]; // 子メニュー
}
```

### UI の挙動
- カテゴリ（親項目）をクリックすると、子項目が展開/折りたたみされるか、インデントして表示。
- デザインはモダンなドキュメントサイト（VitePress 等）を参考にする。

## 3. ルーティングとフェッチ (`App.tsx`)
- `/docs/*` ワイルドカードルーティングにより、ネストされたパスを `MarkdownViewer` へ渡す。
- `MarkdownViewer` は `public/docs/${path}.md` を読み込む。

## 4. 相互リンクの調整
分割された各ページ間で、適切な相互リンクを Markdown 内に記述する。
