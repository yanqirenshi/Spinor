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

