# Task 43: CLHS Format Documentation & Core Pages

リファレンスフォーマットの CLHS 化と、コアドキュメントの作成を行ってください。

## ステップ

### 1. データモデルの更新 (Docs.hs)
- `src/Spinor/Lsp/Docs.hs` を開き、`DocEntry` 型を `specs/43_clhs_docs.md` で定義されたフィールドを含むように拡張してください。
- `primitiveDocs` 内の全エントリを更新してください。
    - **主要関数 (`+`, `cons`, `def`, `fn`, `if` 等):** 意味のある説明と例を記述してください。
    - **その他:** `"TBD"` や `"None."` などのプレースホルダーを使用してコンパイルが通る状態にしてください。

### 2. ジェネレーターの改修 (DocGen.hs)
- `src/Spinor/DocGen.hs` を修正し、新しい `DocEntry` のフィールドをすべて出力するように `renderEntry` 関数を更新してください。
- `See Also` セクションでは、slug リストから `[name](name)` 形式のリンクを生成するようにしてください。
- 出力先ディレクトリ構造 (`manual/public/docs/ref/`) に変更がないか確認してください。

### 3. コアページの作成
- `manual/public/docs/` ディレクトリに以下のファイルを作成してください。
    - `introduction.md`: インストールと利用方法のガイド。
    - `syntax.md`: 言語仕様の概説。
- 内容は `specs/43_clhs_docs.md` の構成案に従い、ユーザーが Spinor を使い始めるのに十分な情報を記述してください。

### 4. 動作確認
- `cabal run spinor -- docgen` を実行してください。
- 生成された `manual/public/docs/ref/` 下の Markdown ファイルが意図したセクション構成になっていることを確認してください。
- `introduction.md` と `syntax.md` が正しく配置されていることを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針
(データモデル拡張の際に工夫した点や、コアページの記述で重点を置いた点など)

### 実装内容
(変更したファイルの一覧、新しく追加したドキュメントの概要など)
