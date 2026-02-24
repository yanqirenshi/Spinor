# Task 43: CLHS Format Documentation & Core Pages の実装

`src/Spinor/DocGen.hs` を改修して CLHS スタイルの詳細なリファレンスを生成し、言語の基本解説ページを作成してください。

## ステップ

### 1. ジェネレーターの改修 (DocGen.hs)
- `src/Spinor/DocGen.hs` の `renderEntry` 関数を修正してください。
- `DocEntry` 型の全フィールド（`docSyntax`, `docArgumentsAndValues`, `docExamples`, `docExceptionalSituations` 等）を、`specs/43_clhs_documentation_spec.md` で定義された順序とフォーマットで Markdown に書き出すようにしてください。
- `docSeeAlso` (リスト形式) から、他のリファレンスページへのリンクを自動生成するロジックを追加してください。

### 2. コアページの作成
- 以下のファイルを `manual/public/docs/` (または現在のドキュメントルート) に新規作成してください。
    - `introduction.md`: インストールガイド、クイックスタートを含む導入ドキュメント。
    - `syntax.md`: Spinor の文法、型システム、Lisp構文とHaskell意味論の融合についての解説ドキュメント。
- 内容は、これまでの開発経緯 (Step 1〜42) を踏まえた正確な仕様を記述してください。

### 3. データモデルの最終確認 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` を確認し、主要な関数 (`+`, `def`, `fn`, `if`, `m+`, `cl-enqueue` 等) の `docSyntax` や `docExamples` が適切に埋まっているか確認し、不足があれば補完してください。

### 4. 動作確認
- `cabal run spinor -- docgen` を実行してください。
- 生成された `manual/public/docs/ref/*.md` を開き、CLHS 風の美しいレイアウトになっていることを確認してください。
- ブラウザ上でマニュアルサイトを閲覧し、`Introduction` や `Syntax` ページへのリンクが機能し、内容が正しく表示されることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**DocGen.hs の改修:**
- CLHS 仕様に準拠し、`**Signature:**` フィールドを基本情報セクションに追加
- 条件付きセクション出力: `docSideEffects`, `docAffectedBy`, `docExceptionalSituations` は値が `"None."` の場合は出力しない (`renderOptionalSection` 関数を追加)
- See Also は既存のリンク生成ロジックを維持

**コアページの改善:**
- `introduction.md` は既に包括的な内容を持っていたため、大きな変更なし
- `syntax.md` に **型システム** セクションを追加 (Hindley-Milner 型推論、基本型、型表示、多相性の解説)

### 実装内容

**変更ファイル:**

1. **src/Spinor/DocGen.hs**
   - `renderEntry` 関数: Signature フィールドを追加
   - `renderOptionalSection` 関数: 条件付きセクション出力 ("None." 以外のみ)

2. **manual/public/docs/syntax.md**
   - 型システムセクションを追加 (型推論、基本型テーブル、多相性の解説)

**生成確認:**

```
$ cabal run spinor -- docgen
Generating documentation...
Generated 60 reference files.
Documentation generated successfully.
```

**出力フォーマット例 (`ref/add.md`):**
- Title: `# +`
- Kind/Signature: 基本情報
- Syntax/Arguments/Description/Examples: 必須セクション
- Exceptional Situations: 条件付き表示 (内容あり)
- See Also: リンク付きリスト

**出力フォーマット例 (`ref/def.md`):**
- Side Effects: 条件付き表示 (内容あり)
- Notes: 条件付き表示 (内容あり)
- Affected By/Exceptional Situations: 非表示 ("None." のため)
