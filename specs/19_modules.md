# Specification 19: モジュールシステム (Module System)

## 1. 概要

Spinor にコードの分割、再利用、名前空間管理のためのモジュールシステムを導入する。これにより、大規模なプログラムを整理し、ライブラリの配布と利用を容易にする。

各ファイルは独立したモジュールとして扱われ、明示的にエクスポートされた定義のみが他のモジュールからアクセス可能になる。

## 2. 主要な構文

モジュールシステムは2つの新しい特殊形式 `module` と `import` によって構成される。

- `(module <module-name> (export <name1> ...))`: モジュールを定義し、公開するシンボルを指定する。
- `(import <module-spec> <options>...)`: 他のモジュールからシンボルをインポートする。

## 3. `module` フォーム

ファイルの先頭に配置し、そのファイルが提供するモジュールを宣言する。

### 3.1. 構文

```lisp
(module <module-name> <export-directive>)

<module-name> ::= <symbol>
<export-directive> ::= (export <symbol1> <symbol2> ...)
```

- `<module-name>`: モジュールの名前をシンボルで指定する。この名前はファイルパスから推論されるべきであり、将来的にはこの指定は省略可能になるかもしれない (例: `twister/core.spin` -> `twister/core`)。
- `(export ...)`: このモジュールから外部に公開されるトップレベルの `define` もしくは `data` の名前をリストで指定する。

### 3.2. 評価ルール

- `module` フォームは、1つのファイルに1度だけ、かつ全ての他の式の前に記述しなければならない。
- `export` リストに記載されていないトップレベル定義は、そのモジュールのプライベートなものとなる。
- `export` を省略した場合、何も公開されない。

### 3.3. 例

`twister/math.spin`:

```lisp
(module twister/math
  (export add subtract))

(define add (fn (x y) (+ x y)))
(define subtract (fn (x y) (- x y)))

; これはプライベート
(define helper-func (fn () 100))
```

## 4. `import` フォーム

他のモジュールでエクスポートされた定義を現在のスコープに導入する。

### 4.1. 構文

```lisp
(import <module-spec> <import-option>*)

<module-spec> ::= <symbol> | <string> ; 'twister/math or "twister/math"
```

- `<module-spec>`: インポートするモジュールをシンボルまたは文字列で指定する。これは、プロジェクトルートからの相対パスに対応する。

### 4.2. インポートオプション

オプションを組み合わせることで、インポートするシンボルを柔軟に制御できる。

#### 4.2.1. `(only <symbol> ...)`

指定したシンボルのみをインポートする。

```lisp
(import 'twister/math (only add))
; `add` のみが利用可能になる
```

#### 4.2.2. `(except <symbol> ...)`

指定したシンボルを除き、エクスポートされた全てのシンボルをインポートする。

```lisp
(import 'twister/math (except subtract))
; `add` は利用可能だが `subtract` は利用不可
```

#### 4.2.3. `(prefix <prefix-symbol>)`

インポートする全てのシンボルの先頭にプレフィックスを付けて束縛する。

```lisp
(import 'twister/math (prefix math-))
(math-add 1 2) ; => 3
```

#### 4.2.4. `(alias <alias-symbol>)`

モジュール自体に別名を与え、シンボルを修飾名でアクセスできるようにする。これは他のオプションとは併用できない。

```lisp
(import 'twister/math (alias M))
(M:add 1 2) ; => 3 (注意: `M:add` というシンボルとして扱う)
```

### 4.3. デフォルトの挙動

インポートオプションが指定されない場合、モジュールがエクスポートする全てのシンボルが現在の名前空間にインポートされる。

```lisp
(import 'twister/math)
(add 1 2)       ; => 3
(subtract 5 2)  ; => 3
```

## 5. カーネルへの影響

### 5.1. パーサー (`Syntax.hs`)

`Expr` 型に `EModule` と `EImport` を追加する。

```haskell
data Expr
  = ...
  | EModule Text [Text] -- (module name (export ...))
  | EImport Text [ImportOption]
  deriving (Show, Eq)

data ImportOption
  = Only [Text]
  | Except [Text]
  | Prefix Text
  | Alias Text
  deriving (Show, Eq)
```

パーサーは `(module ...)` と `(import ...)` をこれらの新しいASTノードとして解析するように拡張される必要がある。

### 5.2. ローダーと評価器 (`Eval.hs`)

評価プロセスを大幅に変更する必要がある。

1.  **Module Loader の導入**: ファイルを評価する前に、まず `module` と `import` 宣言をスキャンする。
2.  **依存関係解決**: `import` 宣言からモジュール間の依存関係グラフを構築する。
3.  **環境の分離**: 各モジュールは独立した環境 (`Env`) を持つ。
4.  **Module Registry**: エクスポートされたシンボルを管理するためのグローバルなレジストリを導入する。`Map ModuleName Env` のような形になる。
5.  **評価順序**: 依存グラフに基づき、依存先のモジュールから順に評価する。
6.  **`eval` の拡張**: `eval` は `EImport` を処理し、Module Registry からシンボルを取得して現在のモジュールの環境に束縛する。`EModule` は Loader によって処理されるため、`eval` が直接評価することはないかもしれない。

### 5.3. REPL

REPL は `user` という名前の特殊なモジュール内で動作する。REPL から `(import ...)` を実行することで、対話的にライブラリをロードして使用できる。

## 6. 実装タスクの分解

1.  **`Syntax.hs` の拡張**: `EModule`, `EImport` および関連するデータ型を定義し、パーサーを実装する。
2.  **Module Loader の骨格作成**: ファイルを読み込み、`EModule` と `EImport` のリストを抽出する `scanImports` のような関数を実装する。
3.  **Module Registry の実装**: `IORef` や `MVar` を使って、ロードされたモジュールのエクスポート内容を保持するグローバルな状態を管理する。
4.  **`Eval.hs` のリファクタリング**:
    - `runEval` を変更し、モジュールごとの `Env` を扱えるようにする。
    - `eval` 内で `import` を処理するロジックを追加する。
5.  **エントリーポイント (`app/Main.hs`) の変更**:
    - バッチ実行モードで、単一ファイルではなくプロジェクト（依存関係を含む）をロードして実行できるようにする。
    - REPL の起動時に `user` モジュールを初期化する。
6.  **`twister` ライブラリのモジュール化**: `core.spin`, `list.spin` 等を新しいモジュール構文に準拠させる。
