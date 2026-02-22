# Spec 43: CLHS Format Documentation & Core Pages

## 概要
Spinor の関数リファレンスを、Common Lisp HyperSpec (CLHS) スタイルの詳細なフォーマットに改修する。また、言語の導入および仕様を説明するコアページを作成し、マニュアルとしての完成度を高める。

## データモデルの拡張
`src/Spinor/Lsp/Docs.hs` の `DocEntry` 型に以下のフィールドを追加し、SSoT (Single Source of Truth) として管理する情報を拡充する。

| フィールド名 | 型 | 説明 |
| :--- | :--- | :--- |
| `docSyntax` | `Text` | 具体的な呼び出し形式 (例: `(cons x lst)`) |
| `docArgumentsAndValues` | `Text` | 引数と戻り値の詳細な説明 |
| `docExamples` | `Text` | 使用例 (Markdown 形式) |
| `docSideEffects` | `Text` | 副作用の有無と内容 (デフォルト: `"None."`) |
| `docAffectedBy` | `Text` | 状態や環境による影響 (デフォルト: `"None."`) |
| `docExceptionalSituations` | `Text` | 例外・エラーが発生する条件 |
| `docSeeAlso` | `[Text]` | 関連関数の slug リスト |
| `docNotes` | `Text` | 補足事項、歴史的な背景など |

## リファレンスの出力フォーマット (`DocGen.hs`)
生成される Markdown ファイル (`docs/ref/<slug>.md`) は、以下のセクション構成を持つものとする。

1. **Title** (`# <Name>`)
2. **Kind** (`**Kind:** <Function | Special Form>`)
3. **Syntax** (`### Syntax:`)
4. **Arguments and Values** (`### Arguments and Values:`)
5. **Description** (`### Description:`) - 既存の `docDescription` を使用
6. **Examples** (`### Examples:`)
7. **Side Effects** (`### Side Effects:`)
8. **Affected By** (`### Affected By:`)
9. **Exceptional Situations** (`### Exceptional Situations:`)
10. **See Also** (`### See Also:`) - Slug を元にリンクを生成
11. **Notes** (`### Notes:`)

## コアページの構成
マニュアルの基盤として以下の 2 ページを `manual/public/docs/` に作成する。

### 1. Introduction (`introduction.md`)
- **Spinor とは:** プロジェクトの目的と哲学。
- **インストール:** ソースからのビルド方法。
- **クイックスタート:** REPL の起動、スクリプトの実行、ビルドコマンド。
- **開発環境:** Emacs (SLY / LSP) のセットアップガイドへのリンク。

### 2. Syntax (`syntax.md`)
- **アトム:** 数値、シンボル、文字列。
- **リストと評価:** 評価規則、クオート。
- **定義:** `def`, `fn`, `mac` の基本。
- **制御構造:** `if`, `let`, `begin` 等。
- **データ型:** ADT (`data`) とパターンマッチ (`match`)。

## 考慮事項
- 既存の LSP サーバー（Hover 表示）への影響を確認し、必要に応じて表示内容を調整する。
- リンク切れが発生しないよう、`docSeeAlso` の slug の妥当性を検証することが望ましい。
