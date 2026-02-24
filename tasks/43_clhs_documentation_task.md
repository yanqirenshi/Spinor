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
(CLHS フォーマットの再現で工夫した点や、解説ページの執筆意図など)

### 実装内容
(変更したファイルの一覧、追加したドキュメントの概要など)
