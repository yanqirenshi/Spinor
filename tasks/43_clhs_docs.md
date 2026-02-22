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

---

## 実装報告

### 実装方針

1. **DocEntry 型の拡張**
   - CLHS スタイルに必要な全 8 フィールドを追加 (`docSyntax`, `docArgumentsAndValues`, `docExamples`, `docSideEffects`, `docAffectedBy`, `docExceptionalSituations`, `docSeeAlso`, `docNotes`)
   - ヘルパー関数 `mkDoc` と `mkDocTBD` を導入し、エントリ定義の冗長性を削減
   - `mkDocTBD` は TBD プレースホルダー付きのエントリを簡潔に定義できる

2. **主要関数の詳細ドキュメント化**
   - 特殊形式: `def`, `fn`, `if`, `let`, `cons` など 15 エントリに完全な説明・例を記述
   - 算術・比較演算子: `+`, `-`, `*`, `%`, `=`, `<`, `>` に説明と例を記述
   - リスト操作: `cons`, `car`, `cdr`, `list`, `null?`, `eq`, `equal` に詳細な説明

3. **コアページの構成**
   - `introduction.md`: インストール、クイックスタート、コマンド一覧、開発環境 (SLY/LSP) へのガイド
   - `syntax.md`: アトム、リストと評価、定義、制御構造、ADT とパターンマッチの解説

### 実装内容

**変更ファイル:**

| ファイル | 変更内容 |
|:---------|:---------|
| `src/Spinor/Lsp/Docs.hs` | `DocEntry` 型を 8 フィールド拡張、ヘルパー関数追加、全 43 エントリを更新 |
| `src/Spinor/DocGen.hs` | `renderEntry` を CLHS フォーマット対応に改修、See Also リンク生成追加 |
| `manual/public/docs/introduction.md` | インストール・クイックスタートガイドを記述 |
| `manual/public/docs/syntax.md` | 言語仕様の概説を記述 |

**生成される CLHS フォーマット:**

```markdown
# <関数名>

**Kind:** Function | Special Form

### Syntax:
(関数呼び出し形式)

### Arguments and Values:
(引数と戻り値の説明)

### Description:
(関数の説明)

### Examples:
(使用例)

### Side Effects:
### Affected By:
### Exceptional Situations:
### See Also:
### Notes:
```

**備考:**
- ビルド時に `network` ライブラリの Windows 環境依存問題が発生したため、動作確認は環境修復後に実施予定
- 文字列・I/O・並行処理関連の 15 エントリは `mkDocTBD` でプレースホルダー定義済み（後日詳細化可能）
