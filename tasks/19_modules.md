# Task 19: モジュールシステムの実装

## 1. 概要 (Overview)

Spinor にファイルベースのモジュールシステムを導入し、コードの分割、再利用、名前空間管理を実現します。本タスクは、仕様書 `specs/19_modules.md` に基づき、パーサーの拡張、モジュールローダーの実装、そして評価器のリファクタリングを行います。

これにより、`twister` ライブラリのような大規模なコードベースを、複数のファイルに分割して管理できるようになります。

## 2. 実装ステップ (Implementation Steps)

### Step 1: 構文の追加 (`src/Spinor/Syntax.hs`)

まず、`module` と `import` を表現するための AST ノードを `Expr` 型に追加し、それらを解析するパーサーを実装します。

- **`Expr` データ型の拡張:**
  `EModule` と `EImport` を追加します。

  ```haskell
  -- src/Spinor/Syntax.hs

  -- ... (他のデータ型定義)
  data ImportOption
    = Only [Text]
    | Except [Text]
    | Prefix Text
    | Alias Text
    deriving (Show, Eq)

  data Expr
    = EInt  Integer
    | EBool Bool
    -- ...
    | EModule Text [Text]             -- ^ (module name (export ...))
    | EImport Text [ImportOption]     -- ^ (import module-spec options...)
    | EMatch Expr [(Pattern, Expr)]
    deriving (Show, Eq)
  ```

- **パーサーの実装:**
  `pList` を拡張し、 `(module ...)` と `(import ...)` という形式を special form として認識させ、新しい `EModule` と `EImport` ノードを生成するように `pModule` と `pImport` パーサーを実装してください。
  - `(module <name> (export <s1> <s2> ...))` をパースします。
  - `(import <module> <options>...)` をパースします。オプションがない場合も考慮してください。

  ```haskell
  -- (ヒント) pList を変更して、新しい特殊形式を試す
  pList :: Parser Expr
  pList = do
    xs <- between (lexeme (char '(')) (lexeme (char ')')) (many parseExpr)
    case xs of
      (ESym "module" : _) -> parseModule xs -- 新しいパーサーへ分岐
      (ESym "import" : _) -> parseImport xs -- 新しいパーサーへ分岐
      [ESym "let", ESym name, val, body] -> pure $ ELet name val body
      _ -> pure $ EList xs

  -- `parseExpr` は `pList` の変更に注意
  parseExpr :: Parser Expr
  parseExpr = sc *> (try pData <|> try pMatch <|> pList <|> ... )
  ```
  `parseModule` と `parseImport` の具体的な実装は仕様書を参照してください。

### Step 2: 評価器の準備 (`src/Spinor/Eval.hs`)

`module` と `import` は、単一の `eval` ループでは評価できません。これらは式の評価が始まる前に「ローダー」によって処理される必要があります。
誤って `eval` に渡された場合に備え、これらの新しい構文に対する評価ルールを追加し、未実装であることを示すエラーを投げるようにしてください。

- **`eval` 関数の拡張:**

  ```haskell
  -- src/Spinor/Eval.hs

  eval :: Expr -> Eval Val
  -- ...
  eval (EModule _ _) = throwError "module declaration is only allowed at the top-level and should be handled by the module loader"
  eval (EImport _ _) = throwError "import is only allowed at the top-level and should be handled by the module loader"
  -- ... (既存の eval ケース)
  ```

### Step 3: モジュールローダーとレジストリの実装

これが本タスクの核心です。モジュールを読み込み、依存関係を解決し、評価するための仕組みを構築します。このロジックは `Eval.hs` または新しいモジュール `Spinor/Loader.hs` に実装するのが良いでしょう。

- **`ModuleRegistry` の定義:**
  ロード済みのモジュールの公開環境 (`Env`) を保持するためのグローバルなレジストリを `IORef` を使って定義します。

  ```haskell
  -- 例: Spinor/Loader.hs
  import Data.IORef
  import qualified Data.Map.Strict as Map
  import Data.Text (Text)
  import Spinor.Val (Env)

  type ModuleRegistry = IORef (Map.Map Text Env)

  -- main 関数で一度だけ作成される
  newModuleRegistry :: IO ModuleRegistry
  newModuleRegistry = newIORef Map.empty
  ```

- **`loadModule` 関数の実装:**
  指定されたモジュール名 (ファイルパスに対応) を引数に取り、以下の処理を行う関数を実装します。
  1.  **循環参照の検出:** 現在ロード中のモジュールリストを管理し、循環 `import` があればエラーを返します。
  2.  **ファイル読み込み:** モジュール名からファイルパスを解決し (`.spin` を付加)、ファイルの内容を読み込みます。
  3.  **スキャン:** `parseFile` を使ってファイルをパースし、`EModule` と `EImport` 宣言を抽出します。`module` 宣言がなければエラーとします。
  4.  **依存関係の解決:** `EImport` 宣言を元に、依存するモジュールを再帰的に `loadModule` でロードします。
  5.  **モジュール評価:**
      - 新しい空の `Env` を作成します。
      - 依存モジュールの公開 `Env` を `ModuleRegistry` から取得し、`import` のルール (`only`, `prefix` 等) に従って現在のモジュール `Env` にシンボルを束縛します。
      - ファイル内の `EModule` と `EImport` を除いた残りの式を、この `Env` の下で `eval` します。
  6.  **レジストリ登録:** `EModule` の `export` リストに基づき、評価後の `Env` から公開するシンボルのみを抽出した `publicEnv` を作成し、`ModuleRegistry` に登録します。

### Step 4: エントリーポイントの修正 (`app/Main.hs`)

アプリケーションの起動時に、単一ファイルの評価ではなく、モジュールローダーを使うように変更します。

- `main` 関数で `newModuleRegistry` を呼び出し、`loadModule` 関数に渡します。
- ファイルが指定された場合は、そのファイルを起点として `loadModule` を呼び出します。
- REPL 起動時は、`user` という仮想的なモジュールのコンテキストで動作させ、REPL 内で `(import ...)` を評価できるように `eval` と連携する仕組みを検討します (これは後続タスクでも可)。

### Step 5: 標準ライブラリのモジュール化 (`twister/`)

`core.spin` と `list.spin` の先頭に、それぞれ `module` 宣言を追加します。

- `twister/core.spin`:
  ```lisp
  (module twister/core (export + - * / = > < ...))
  ; ...
  ```
- `twister/list.spin`:
  ```lisp
  (import twister/core)
  (module twister/list (export car cdr cons ...))
  ; ...
  ```

## 3. 確認手順 (Verification)

1.  `Syntax.hs` を変更後、`readExpr "(module my-mod (export a))"` や `readExpr "(import a (only b))"` のようなテストが成功することを確認します。
2.  以下の3つのファイルを作成します。
    - `lib.spin`:
      ```lisp
      (module lib (export my-val))
      (define my-val 100)
      ```
    - `main.spin`:
      ```lisp
      (module main (export))
      (import lib)
      (print my-val)
      ```
3.  `spin main.spin` を実行し、標準出力に `100` と表示されることを確認します。
4.  `prefix` や `only` などの `import` オプションが正しく機能することもテストしてください。

---

# 実装方針

## 設計判断

### 1. モジュールローダーの分離
`Spinor.Loader` モジュールを新規作成し、モジュールのロード・依存解決・評価を `Eval.hs` から分離しました。これにより:
- 単一責任の原則を維持
- 循環参照の検出ロジックを明確に分離
- `ModuleRegistry` の状態管理を `IORef` で実装

### 2. 後方互換性の維持
`loadBoot` (Twister ファイルのロード) は従来通り動作するようにしました:
- `processBootExpr` で `EModule` と `EImport` をスキップ
- これにより、既存の `twister/*.spin` ファイルにモジュール宣言を追加しても、REPLや従来のバッチモードで正常に動作

### 3. インポートオプションの設計
仕様書に基づき、4種類のインポートオプションを実装:
- `Only [Text]`: 指定シンボルのみインポート
- `Except [Text]`: 指定シンボルを除外
- `Prefix Text`: 全シンボルにプレフィックス付加
- `Alias Text`: `M:sym` 形式でアクセス

### 4. パーサー実装方針
`pList` 内で `module` と `import` を特殊形式として認識し、専用のパーサー関数 (`parseModuleForm`, `parseImportForm`) に分岐させました。これにより:
- 既存の `parseExpr` の変更を最小化
- Lisp らしい S式ベースの構文を維持

---

# 実装内容

## 変更ファイル一覧

### 1. `src/Spinor/Syntax.hs`
- `ImportOption` データ型を追加 (`Only`, `Except`, `Prefix`, `Alias`)
- `Expr` 型に `EModule Text [Text]` と `EImport Text [ImportOption]` を追加
- `pList` を拡張して `module` と `import` を認識
- `parseModuleForm`, `parseImportForm`, `parseImportOption` 関数を追加
- `extractModuleName`, `extractSymName` ヘルパー関数を追加
- エクスポートリストに `ImportOption(..)` を追加

### 2. `src/Spinor/Eval.hs`
- `EModule` と `EImport` の評価ルールを追加 (ローダーで処理されるべきエラーを投げる)
- `exprToVal` に `EModule` と `EImport` のケースを追加
- `ImportOption(..)` のインポートを追加

### 3. `src/Spinor/Expander.hs`
- `EModule` と `EImport` の展開ルールを追加 (そのまま返す)

### 4. `src/Spinor/Loader.hs` (新規作成)
- `ModuleRegistry` 型: `IORef (Map.Map Text Env)`
- `LoaderConfig` 型: baseDir と primitiveEnv を保持
- `newModuleRegistry`: 空のレジストリを作成
- `loadModule`: 循環参照を検出しながらモジュールをロード
- `loadDependencies`: 依存モジュールをロードしてインポート環境を構築
- `applyImportOptions`: インポートオプションを適用
- `extractExports`: エクスポートリストに基づいて公開シンボルを抽出
- `evalFileWithModules`: バッチモード用のファイル評価

### 5. `app/Main.hs`
- `Spinor.Loader` のインポートを追加
- `batchMode` を修正してモジュールローダーを使用
- `processBootExpr` を修正して `EModule` と `EImport` をスキップ
- 必要なインポート追加 (`getCurrentDirectory`, `takeDirectory`, `Expr(..)`)

### 6. `spinor.cabal`
- `exposed-modules` に `Spinor.Loader` を追加
- `build-depends` に `filepath` と `directory` を追加

### 7. `twister/core.spin`
- モジュール宣言を追加: `(module twister/core (export not id when cond))`

### 8. `twister/list.spin`
- モジュール宣言を追加: `(module twister/list (export nil null? map length append foldl foldr reverse filter))`

### 9. `twister/math.spin`
- モジュール宣言を追加: `(module twister/math (export even? odd? fact fib))`

### 10. `test/Spinor/ParserSpec.hs`
- `ImportOption(..)` のインポートを追加
- `module` 宣言のパーステストを追加 (3ケース)
- `import` 宣言のパーステストを追加 (6ケース)

### 11. `test-modules/lib.spin` (新規作成)
- テスト用モジュール: `my-val` と `double` をエクスポート

### 12. `test-modules/main.spin` (新規作成)
- テスト用メインモジュール: `lib` をインポートして使用

## 動作確認手順

```bash
# ビルド
cabal build

# パーサーテスト
cabal test

# モジュールシステムのテスト
cabal run spinor -- test-modules/main.spin
# 期待出力:
# 100
# 42

# 既存のテスト (後方互換性確認)
cabal run spinor -- twister/test.spin
```
