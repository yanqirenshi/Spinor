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
