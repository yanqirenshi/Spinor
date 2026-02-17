# 実装指示: ステップ1 (ASTとパーサー)

以下の仕様に基づいて、Spinor の基本的な構文解析モジュールを実装してください。
ファイル名は `Syntax.hs` と `Main.hs` に分けたいと思います。

## 1. Syntax.hs の仕様

* 必要なライブラリ: `megaparsec`, `text`, `void`
* データ型 `Expr` を定義:
    * `EInt Integer`
    * `EBool Bool` (リテラルは `#t`, `#f`)
    * `ESym Text` (標準的なLispシンボル。`+` や `-` 等の記号も許容)
    * `EList [Expr]`
* パーサーの実装:
    * `sc` (Space Consumer): 空白とコメント (`;` から行末まで) をスキップする。
    * `parseExpr :: Parser Expr`: S式をパースするメイン関数。
    * `readExpr :: Text -> Either String Expr`: パース実行用のヘルパー関数。

## 2. Main.hs の仕様

* 標準入力から S式 を読み込み、パース結果(`Show` インスタンス)を表示するだけのシンプルな REPL。
* `loop` 関数で入力を待ち受ける。

## 出力形式

* それぞれのファイルの中身をコードブロックで提示してください。
* コンパイルに必要な `.cabal` ファイルまたは `stack` 用の記述は不要です（ghc で直接コンパイルします）。

# 実装方針

## 概要

Spinor 言語の最初のステップとして、S式をパースして AST に変換するパーサーモジュールと、パース結果を表示する簡易 REPL を実装する。

## 設計判断

### Megaparsec の採用

Haskell のパーサーライブラリとして `Megaparsec` を使用。Parsec に比べてエラーメッセージが優れており、`Text` ベースのパースに対応している。

### AST の設計

`Expr` 型を最小限のコンストラクタで定義:
- `EInt Integer` — 整数リテラル
- `EBool Bool` — 真偽値 (`#t`, `#f`)
- `ESym Text` — シンボル（変数名、演算子含む）
- `EList [Expr]` — リスト（関数適用・特殊形式を統一的に表現）

Lisp の「すべては S式」という原則に従い、`if` や `define` も `EList` で表現する（特殊形式の判別は評価器側で行う）。

### モジュール構成

- `src/Spinor/Syntax.hs` — AST 定義 + パーサー（`readExpr` をエントリポイントとする）
- `app/Main.hs` — 標準入力から読み込んでパース結果を表示する REPL ループ

### パーサーの構成

- `sc` (Space Consumer): 空白 + セミコロンコメント (`; ...`) をスキップ
- 各リテラルパーサー (`pInt`, `pBool`, `pSym`) を `lexeme` でラップ
- `pList`: `(` と `)` で囲まれた `many parseExpr`
- `parseExpr`: 各パーサーを `<|>` で合成（`try` で必要に応じてバックトラック）

