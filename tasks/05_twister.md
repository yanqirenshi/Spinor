# タスク: ステップ5 - Twister (ブートストラップ・ライブラリ) の始動

## プロジェクト構造の定義
プロジェクトを「カーネル (Spinor)」と「標準ライブラリ (Twister)」という概念に分離します。
ユーザーの哲学は **「原理は単純に (Spinor)、構造は複雑に (Twister)」** です。

## 目標
1. `twister/` ディレクトリを作成し、Spinor 言語で書かれたライブラリコードを配置する。
2. Spinor (Haskell側) に、外部ファイルを読み込んで評価する `load` プリミティブを実装する。
3. 起動時に `twister/boot.spin` を読み込み、環境を初期化できるようにする。

## 実装詳細指示

### 1. `twister/` ディレクトリとファイル作成
プロジェクトルートに `twister` フォルダを作成し、以下の Spinor コード（S式）を作成してください。

* **`twister/core.spin`** (基本論理)
    * `(def not (fn (x) (if x #f #t)))`
    * `(def id (fn (x) x))`
* **`twister/list.spin`** (リスト操作)
    * `(def null? (fn (x) (empty? x)))`
    * `(def map (fn (f xs) (if (null? xs) (quote ()) (cons (f (car xs)) (map f (cdr xs))))))`
* **`twister/boot.spin`** (ブートローダー)
    * `(print "Loading Twister environment...")`
    * `(load "twister/core.spin")`
    * `(load "twister/list.spin")`
    * `(print "Twister loaded.")`

### 2. `src/Spinor/Primitive.hs` (拡張)
* `load` プリミティブの実装:
    * `readFile` でテキストを読み、`parseFile` でパースし、`eval` で順次実行する。
    * 戻り値は `#t` (成功)。
* `print` プリミティブの実装:
    * 文字列を表示し、その値をそのまま返す。

### 3. `src/Spinor/Syntax.hs` (拡張)
* `parseFile :: Text -> Either String [Expr]` を追加。
    * ファイル全体（複数のS式）をリストとしてパースする機能。

### 4. `app/Main.hs` (修正)
* `main` 関数で、起動時に `./twister/boot.spin` の存在を確認。
* 存在する場合、自動的に `load` を実行してから REPL に入るようにする。

## 出力要件
* `twister/` 配下の各ファイルの内容。
* Haskell 側の変更点 (`Primitive.hs`, `Main.hs`, `Syntax.hs`)。
* 実装後、REPL を起動すると "Twister loaded." と表示され、`map` や `not` がすぐに使える状態になることを目指してください。

# 実装内容

## 変更・新規ファイル

| ファイル | 操作 | 概要 |
|---|---|---|
| `src/Spinor/Syntax.hs` | 修正 | `EStr Text` コンストラクタ追加、`pStr` 文字列パーサー追加、`parseFile` (複数式パース) 追加、`pList` の閉じ括弧に `lexeme` 適用 |
| `src/Spinor/Val.hs` | 修正 | `VStr Text` コンストラクタ追加、`showVal` に VStr 表示追加 |
| `src/Spinor/Primitive.hs` | 修正 | `empty?` を `null?` のエイリアスとして追加 |
| `src/Spinor/Eval.hs` | 修正 | `EStr` eval 追加、`def` エイリアス追加、`load` 特殊形式 (ファイル読み込み+パース+逐次eval)、`print` 特殊形式、`exprToVal` に EStr 対応 |
| `app/Main.hs` | 修正 | 起動時 `twister/boot.spin` 自動読み込み (`loadBoot`)、REPL バナーを step5 に更新 |
| `spinor.cabal` | 修正 | executable に `directory` 依存追加 |
| `twister/core.spin` | 新規 | `not`, `id` 関数定義 |
| `twister/list.spin` | 新規 | `null?` (empty? ラッパー), `map` 関数定義 |
| `twister/boot.spin` | 新規 | ブートローダー (core.spin, list.spin を load) |

## 設計メモ

- **文字列リテラル**: `EStr Text` / `VStr Text` を新設。`pStr` は `manyTill L.charLiteral` でエスケープシーケンス対応
- **`load` / `print` は特殊形式**: IO とeval環境アクセスが必要なため、`VPrim` (純粋関数) ではなく `Eval.hs` 内の特殊形式として実装。引数は評価される (define のような非評価特殊形式とは異なる)
- **`def` エイリアス**: `define` の短縮形。`evalDefine` ヘルパーで共通化
- **`pList` の lexeme 修正**: 閉じ括弧 `)` の後に `lexeme` を適用し、後続の空白を消費。これにより `parseFile` での複数式パース時に `many parseExpr` が正しく終了する
- **`loadBoot` の設計**: `Main.hs` で `doesFileExist` チェック → `parseFile` → `runEval` で全式を順次評価。エラー時はメッセージ表示してプリミティブ環境のままREPLへ

## テスト結果

```
$ cabal run spinor
Spinor REPL (step5)
Loading Twister environment...
Twister loaded.
spinor> (not #t)        => #f
spinor> (not #f)        => #t
spinor> (id 42)         => 42
spinor> (map (fn (x) (* x x)) (list 1 2 3 4 5))  => (1 4 9 16 25)
spinor> (print "hello world")
hello world
                        => "hello world"
spinor> (map (fn (x) (+ x 1)) (list 10 20 30))    => (11 21 31)
```
