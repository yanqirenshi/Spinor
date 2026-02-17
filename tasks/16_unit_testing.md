# タスク: ステップ16 - ユニットテスト基盤の整備

## 目標

Spinor プロジェクトの信頼性を担保するため、Haskell 側の単体テストと、Spinor コードのテスト環境を構築する。

## 実装詳細指示

### 1. `spinor.cabal` (修正)

* `test-suite` セクションを追加し、`hspec` を利用できるようにしてください。
    * 依存関係: `base`, `hspec`, `hspec-discover`, `spinor` (library)
    * `hs-source-dirs`: `test`
    * `main-is`: `Spec.hs`
    * `type`: `exitcode-stdio-1.0`

### 2. Haskell 側テストの実装 (`test/`)

以下のファイルを作成し、基本的な動作確認テストを記述してください。

* `test/Spec.hs`:
    * `{-# OPTIONS_GHC -F -pgmF hspec-discover #-}` (自動検知用)
* `test/Spinor/ParserSpec.hs`:
    * 数値、真偽値、リスト、クォート等のパース結果が正しいか検証。
* `test/Spinor/EvalSpec.hs`:
    * 基本的な演算 (`+`, `-`)、関数適用、`if` の動作を検証。
    * 例: `eval env "(+ 1 2)"` が `VInt 3` になること。

### 3. バッチ実行モードの実装 (`app/Main.hs`)

Twister のテストを実行しやすくするため、引数でファイルを指定して実行し、終了するモードを追加します。

* **変更点:**
    * 引数なし: 従来の REPL 起動。
    * 引数あり (`cabal run spinor <file>`): 指定されたファイルを読み込み、評価して終了する。

### 4. Spinor 側テストの実装 (`twister/test.spin`)

Spinor 自身で記述されたテストライブラリと、実際のテストケースを作成します。

* **テスト用マクロ (`assert-equal`):**
    * 期待値と実測値が等しければ `.` を表示し、異なればエラーメッセージを表示してプログラムを終了（またはエラー報告）するマクロ。
* **テストケース:**
    * `list.spin` の関数 (`map`, `filter`, `reverse`, `foldl`) の動作確認。
    * `math.spin` の関数 (`fact`, `fib`) の動作確認。

## 確認事項

以下のコマンドが成功することを確認してください。

1. **Haskell テスト:**
   ```bash
   cabal test

```

-> 全ての Haskell テストがパスすること。

2. **Spinor テスト:**
```bash
cabal run spinor -- twister/test.spin

```

-> 定義したアサーションが全て通り、エラーが出ないこと。

## 出力要件

* `spinor.cabal` の変更点。
* `test/` ディレクトリ配下の Haskell コード。
* `app/Main.hs` の変更点。
* `twister/test.spin` の内容。
* テスト実行ログ。

# 実装内容

## 1. `spinor.cabal` の変更

`test-suite spinor-test` セクションを追加:

```cabal
test-suite spinor-test
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  other-modules:
    Spinor.ParserSpec
    Spinor.EvalSpec
  build-depends:
    base           >= 4.14 && < 5,
    hspec          >= 2.7  && < 3,
    hspec-discover >= 2.7  && < 3,
    text           >= 1.2  && < 3,
    containers     >= 0.6  && < 1,
    spinor
  default-language: Haskell2010
  ghc-options:      -Wall
  build-tool-depends:
    hspec-discover:hspec-discover >= 2.7 && < 3
```

## 2. `src/Spinor/Val.hs` の変更

テスト用に `Eq Val` インスタンスを追加。`VPrim`, `VFunc`, `VMacro` は関数を含むため常に不等:

```haskell
instance Eq Val where
  VInt  a   == VInt  b   = a == b
  VBool a   == VBool b   = a == b
  VStr  a   == VStr  b   = a == b
  VSym  a   == VSym  b   = a == b
  VList as  == VList bs  = as == bs
  VNil      == VNil      = True
  _         == _         = False
```

## 3. Haskell テスト (`test/`)

### `test/Spec.hs`
hspec-discover 用の自動検知エントリポイント。

### `test/Spinor/ParserSpec.hs`
パーサーの網羅テスト (17テスト):
- 整数リテラル (正/負/ゼロ)
- 真偽値 (`#t`, `#f`)
- 文字列リテラル
- シンボル (英字, 記号含み, 演算子)
- リスト (空, 整数, 関数適用, ネスト)
- quote (略記, 正式形式)
- let 式 → `ELet` 変換
- define 式

### `test/Spinor/EvalSpec.hs`
評価器の網羅テスト (24テスト):
- 基本算術演算 (`+`, `-`, `*`, `%`, ネスト)
- 比較演算 (`=`, `<`, `>`)
- `if` 式 (真/偽/条件式)
- 関数定義と適用 (即時適用, 多引数)
- `let` 式 (基本, ネスト)
- リスト操作 (`cons`, `car`, `cdr`, `null?`)
- `quote` (数値, リスト, 略記)

ヘルパー `evalStr` で「パース → 展開 → 評価」を一括実行し、`shouldEvalTo` で期待値検証。

## 4. `app/Main.hs` の変更

バッチ実行モードを追加:

- **引数なし** → 従来の REPL 起動 (`replMode`)
- **引数あり** → ファイル読み込み＋評価＋終了 (`batchMode`)

```haskell
main = do
  args <- getArgs
  case args of
    []     -> replMode
    [file] -> batchMode file
    _      -> putStrLn "Usage: spinor [file]"
```

`batchMode` は各式を展開→評価し、エラーがあれば `exitFailure` で非ゼロ終了。

## 5. `twister/test.spin`

`assert-equal` 関数と 23個のセルフテスト:

- **math.spin**: `fact` (4テスト), `fib` (3テスト), `even?`/`odd?` (5テスト)
- **list.spin**: `foldl` (2テスト), `length` (2テスト), `map` (2テスト), `filter` (2テスト), `reverse` (2テスト), `append` (1テスト)

## 6. テスト実行ログ

### `cabal test` (Haskell テスト)

```
Spinor.Eval
  Spinor.Eval (Evaluator)
    基本的な算術演算
      (+ 1 2) → 3 [✔]
      (- 10 3) → 7 [✔]
      (* 4 5) → 20 [✔]
      (% 7 3) → 1 [✔]
      ネストした算術: (+ (* 2 3) (- 10 4)) [✔]
    比較演算
      (= 1 1) → #t [✔]
      (= 1 2) → #f [✔]
      (< 1 2) → #t [✔]
      (> 1 2) → #f [✔]
    if 式
      (if #t 1 2) → 1 [✔]
      (if #f 1 2) → 2 [✔]
      条件が式: (if (= 1 1) 10 20) → 10 [✔]
    関数定義と適用
      即時適用: ((fn (x) (+ x 1)) 10) [✔]
      多引数: ((fn (x y) (+ x y)) 3 4) [✔]
    let 式
      (let x 5 (+ x 3)) → 8 [✔]
      ネストした let [✔]
    リスト操作
      cons でリスト構築 [✔]
      car でリストの先頭取得 [✔]
      cdr でリストの残り取得 [✔]
      null? で空リスト判定 [✔]
      null? で非空リスト判定 [✔]
    quote
      数値の quote [✔]
      リストの quote [✔]
      quote 略記 [✔]
Spinor.Parser
  Spinor.Syntax (Parser)
    整数リテラル
      正の整数 [✔]
      負の整数 [✔]
      ゼロ [✔]
    真偽値
      #t → EBool True [✔]
      #f → EBool False [✔]
    文字列リテラル
      二重引用符で囲まれた文字列 [✔]
    シンボル
      英字シンボル [✔]
      記号を含むシンボル [✔]
      演算子シンボル [✔]
    リスト
      空リスト [✔]
      整数のリスト [✔]
      関数適用式 [✔]
      ネストしたリスト [✔]
    quote
      quote 略記 [✔]
      quote 正式形式 [✔]
    let 式
      (let x 1 x) → ELet [✔]
    define 式
      (define x 42) [✔]

Finished in 0.0020 seconds
41 examples, 0 failures
```

### `cabal run spinor -- twister/test.spin` (Spinor セルフテスト)

```
Loading Twister environment...
Twister loaded.
.......................

All tests passed.
```

全 41 Haskell テスト + 23 Spinor セルフテスト = **合計 64 テスト、全てパス**。
