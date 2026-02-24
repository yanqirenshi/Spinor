# Spec 43: CLHS Format Documentation & Core Pages

## 概要
Spinor のドキュメントシステムを、Common Lisp HyperSpec (CLHS) に準拠した詳細な構造に刷新する。
これにより、開発者が関数の挙動、引数の型、例外条件、使用例を一つのページで網羅的に把握できるようにする。

## 1. リファレンスの出力フォーマット (Markdown)
`src/Spinor/DocGen.hs` は、`DocEntry` の各フィールドを以下の順序で Markdown セクションとして出力する。

### レイアウト構成
1. **タイトル**: `# <Function Name>`
2. **基本情報**: `**Kind:** <Function/Special Form>`, `**Signature:** <docSignature>`
3. **Syntax**:
   ```markdown
   ### Syntax:
   `<docSyntax>`
   ```
4. **Arguments and Values**:
   ```markdown
   ### Arguments and Values:
   <docArgumentsAndValues>
   ```
5. **Description**:
   ```markdown
   ### Description:
   <docDescription>
   ```
6. **Examples**:
   ```markdown
   ### Examples:
   <docExamples>
   ```
7. **Side Effects / Affected By / Exceptional Situations**:
   (値が "None." 以外の場合のみ出力)
8. **See Also**:
   (slug リストから内部リンクを生成)
9. **Notes**:
   (存在する場合のみ)

## 2. コア解説ページの構成案

### `docs/introduction.md` (導入)
- **コンセプト**: "Static Lisp with Haskell Semantics" の説明。
- **インストール**: `cabal build` 手順。
- **クイックスタート**: REPL の起動方法と、簡単な Hello World 実行。
- **ツールチェーン**: Emacs (SLY/LSP) との連携。

### `docs/syntax.md` (構文とセマンティクス)
- **S式**: アトム、リスト、クオート。
- **型システム**: 静的型付け、型推論、`::` による型表示。
- **定義**: `def`, `fn`, `mac` (マクロ)。
- **制御構造**: `if`, `let` (逐次束縛), `match` (パターンマッチ)。
- **データ型**: ADT (`data`) の定義と利用。

## 3. 技術的要件
- `DocGen.hs` は `DocEntry` の `docSlug` をファイル名として使用し、`docs/ref/` ディレクトリに各 Markdown を生成する。
- 相互参照 (`docSeeAlso`) は `[name](slug.md)` 形式のリンクとしてレンダリングする。
