# Architecture & Internals

Spinor の内部アーキテクチャ、設計思想、およびトランスパイルパイプラインの技術的な解説です。言語開発者やコントリビューターを対象としています。

---

## 概要: Dual Implementation

Spinor は **Dual Implementation** アーキテクチャを採用しています。これは、同一の言語仕様を 2 つの異なるバックエンドで実行可能にする設計です。

```
                      Spinor Source (.spin)
                              |
                              v
    +-----------------------------------------------------+
    |              Frontend (Haskell Kernel)              |
    |                                                     |
    |   Parser --> Expander --> Type Checker --> AST      |
    +-----------------------------------------------------+
                              |
              +---------------+---------------+
              |                               |
              v                               v
    +---------------------+       +---------------------+
    |     Interpreter     |       |      Compiler       |
    |      (Haskell)      |       |     (C Codegen)     |
    +---------------------+       +---------------------+
    | - REPL              |       | - Native Binary     |
    | - LSP / SLY         |       | - WASM Target       |
    | - Dev / Debug       |       | - Production        |
    +---------------------+       +---------------------+
```

### Dual Implementation のメリット

| 特性 | Interpreter (Haskell) | Compiler (C) |
|------|----------------------|--------------|
| **起動速度** | 即座に実行開始 | コンパイル時間が必要 |
| **実行速度** | 中程度 | ネイティブ速度 |
| **デバッグ** | スタックトレース完全 | 限定的 |
| **配布** | Haskell ランタイム必要 | スタンドアロン |
| **ターゲット** | 開発時 | 本番デプロイ |

---

## Frontend: Haskell Kernel

Spinor の Frontend は Haskell で実装され、以下のコンポーネントで構成されています。

### Parser (`Syntax.hs`)

[Megaparsec](https://hackage.haskell.org/package/megaparsec) ライブラリを使用した S 式パーサーです。

```haskell
-- パーサーの型定義
type Parser = Parsec Void Text

-- S 式のパース例
readExpr :: Text -> Either SpinorError Expr
```

**特徴:**
- **位置情報追跡**: 各 AST ノードに `SourceSpan` (ファイル名、行、列) を付与
- **エラーメッセージ**: Megaparsec の詳細なエラー報告機能を活用
- **Unicode 対応**: 日本語を含む任意の Unicode シンボルをサポート

### AST (`Syntax.hs`)

```haskell
data Expr
  = EInt   SourceSpan Integer        -- 整数リテラル
  | EFloat SourceSpan Double         -- 浮動小数点リテラル
  | EBool  SourceSpan Bool           -- 真偽値
  | EStr   SourceSpan Text           -- 文字列
  | ESym   SourceSpan Text           -- シンボル
  | EList  SourceSpan [Expr]         -- リスト (関数呼び出し)
  | EQuote SourceSpan Expr           -- クォート
  | EData  SourceSpan Text [TypeExpr] [ConstructorDef]  -- ADT 定義
  | EMatch SourceSpan Expr [(Pattern, Expr)]            -- パターンマッチ
  ...
```

### Macro Expander (`Expander.hs`)

マクロは Frontend でのみ展開され、Backend には展開済み AST が渡されます。

```
(when c b)  --[expand]-->  (if c b nil)
```

### Type Inference (`Infer.hs`)

**Hindley-Milner 型推論**を実装しています。

```haskell
-- 型の定義
data Type
  = TInt                    -- 整数型
  | TBool                   -- 真偽型
  | TStr                    -- 文字列型
  | TVar Text               -- 型変数
  | TFun Type Type          -- 関数型 (a -> b)
  | TList Type              -- リスト型 [a]
  | TData Text [Type]       -- ユーザー定義型

-- 型スキーム (多相型)
data Scheme = Forall [Text] Type  -- ∀a. a -> a
```

**型推論のフロー:**

```
AST --> [ Infer ] --> Typed AST
          |
          +-- (1) Collect constraints
          +-- (2) Unification
          +-- (3) Generalization
```

1. **制約収集**: AST を走査し、型制約 (例: `a = Int`, `b = a -> c`) を収集
2. **単一化 (Unification)**: 制約を解いて型変数を具体化
3. **汎化 (Generalization)**: 未束縛の型変数を多相型変数に

### Package System (`Val.hs`, `Eval.hs`)

Common Lisp 風のパッケージシステムを実装しています。

```haskell
data Package = Package
  { pkgName        :: Text                -- パッケージ名
  , pkgBindings    :: Map Text Val        -- 定義されたシンボル
  , pkgExports     :: Set Text            -- 外部公開シンボル
  , pkgUsedPackages :: [Text]             -- use したパッケージ
  }

data Context = Context
  { ctxCurrentPackage :: Text             -- 現在のパッケージ
  , ctxPackages       :: Map Text Package -- 全パッケージ
  }
```

**シンボル解決順序:**
1. ローカル束縛 (let, fn 引数)
2. カレントパッケージ
3. use-package でインポートしたパッケージ
4. spinor コアパッケージ

---

## Evaluator: Interpreter Mode

`Eval.hs` は AST を直接解釈実行するインタプリタです。

### 値の表現 (`Val.hs`)

```haskell
data Val
  = VInt  Integer                     -- 整数
  | VFloat Double                     -- 浮動小数点
  | VBool Bool                        -- 真偽値
  | VStr  Text                        -- 文字列
  | VList [Val]                       -- リスト
  | VFunc [Text] Expr Env             -- クロージャ
  | VData Text [Val]                  -- ADT コンストラクタ
  | VMVar (MVar Val)                  -- 並行処理用 MVar
  | VMatrix Int Int (VS.Vector Double)  -- 行列
  | VCLContext (Ptr ()) (Ptr ())      -- OpenCL コンテキスト
  ...
```

### 評価モナド

```haskell
type Eval a = StateT EvalState (ExceptT SpinorError IO) a

data EvalState = EvalState
  { evalEnv     :: Env      -- ローカル環境
  , evalContext :: Context  -- パッケージコンテキスト
  }
```

- **StateT**: 環境とコンテキストの状態管理
- **ExceptT**: エラーハンドリング (`handler-case`, `ignore-errors`)
- **IO**: ファイル I/O、並行処理、FFI

---

## Backend: C Transpiler

`Compiler/Codegen.hs` は Spinor AST を C99 コードに変換します。

### トランスパイルパイプライン

```
+----------------------------------------------------------+
|  spinor build example.spin                               |
+----------------------------------------------------------+
                            |
                            v
+----------------------------------------------------------+
|  Step 1: Parse & Type Check                              |
|                                                          |
|    (defun factorial (n)                                  |
|      (if (<= n 1) 1 (* n (factorial (- n 1)))))          |
+----------------------------------------------------------+
                            |
                            v
+----------------------------------------------------------+
|  Step 2: C Code Generation (Codegen.hs)                  |
|                                                          |
|    SpObject* sp_factorial(SpObject* n) {                 |
|        while(1) {  // TCO: loop conversion               |
|            if (sp_lte(n, sp_make_int(1))->value.boolean) |
|                return sp_make_int(1);                    |
|            } else {                                      |
|                SpObject* _tmp0 = sp_sub(n, sp_make_int(1));
|                n = _tmp0;                                |
|                continue;                                 |
|            }                                             |
|        }                                                 |
|    }                                                     |
+----------------------------------------------------------+
                            |
                            v
+----------------------------------------------------------+
|  Step 3: GCC/Clang Compilation                           |
|                                                          |
|    gcc -O2 -o example example.c runtime/spinor.c         |
+----------------------------------------------------------+
                            |
                            v
+----------------------------------------------------------+
|  Output: Native Binary                                   |
|                                                          |
|    ./example                                             |
|    => 3628800  (factorial of 10)                         |
+----------------------------------------------------------+
```

### C Runtime (`runtime/spinor.h`)

C ランタイムは **Tagged Union** パターンで Spinor の動的型を表現します。

```c
typedef enum {
    SP_NIL, SP_BOOL, SP_INT, SP_STR, SP_PAIR, SP_CLOSURE
} SpType;

typedef struct SpObject {
    SpType  type;      // 型タグ
    SpValue value;     // 値 (union)
} SpObject;

// コンストラクタ
SpObject* sp_make_int(long value);
SpObject* sp_make_str(const char* s);

// プリミティブ演算
SpObject* sp_add(SpObject* a, SpObject* b);
SpObject* sp_eq(SpObject* a, SpObject* b);
```

### Tail Call Optimization (TCO)

Spinor のコンパイラは **末尾再帰**を検出し、C の `while(1)` + `continue` パターンに変換します。これによりスタックオーバーフローを防ぎます。

**変換前 (Spinor):**
```lisp
(defun sum-iter (n acc)
  (if (<= n 0)
      acc
      (sum-iter (- n 1) (+ acc n))))
```

**変換後 (C):**
```c
SpObject* sp_sum_iter(SpObject* n, SpObject* acc) {
    while(1) {
        if (sp_lte(n, sp_make_int(0))->value.boolean) {
            return acc;
        } else {
            SpObject* _tmp0 = sp_sub(n, sp_make_int(1));
            SpObject* _tmp1 = sp_add(acc, n);
            n = _tmp0;
            acc = _tmp1;
            continue;  // Back to loop start (no function call)
        }
    }
}
```

---

## WASM Target

Spinor は [Emscripten](https://emscripten.org/) を経由して WebAssembly にコンパイルできます。

### WASM ビルドフロー

```
spinor build --wasm example.spin
        |
        v
+------------------------------------------+
|  Step 1: Spinor --> C Code Generation    |
|  (Codegen.hs)                            |
+------------------------------------------+
        |
        v
+------------------------------------------+
|  Step 2: Emscripten Compilation          |
|                                          |
|  emcc -O2 -s WASM=1                      |
|       -s EXPORTED_FUNCTIONS='["_main"]'  |
|       example.c runtime/spinor.c         |
|       -o example.js                      |
+------------------------------------------+
        |
        v
+------------------------------------------+
|  Output: example.wasm + example.js       |
|                                          |
|  <script src="example.js"></script>      |
+------------------------------------------+
```

### Browser REPL

Spinor の公式サイトには、WASM ベースの [ブラウザ REPL](/repl) が統合されています。

- **xterm.js**: ターミナルエミュレーション
- **WASM Module**: Spinor の C ランタイムを WASM にコンパイル
- **入出力**: JavaScript と WASM 間のメッセージパッシング

---

## 拡張機能

### HPC & Science 統合

Spinor は科学技術計算のためのバックエンドを統合しています。

| モジュール | 機能 | バックエンド |
|-----------|------|-------------|
| `GPGPU.hs` | GPU 並列計算 | OpenCL |
| `GL.hs` | 3D 可視化 | OpenGL / GLFW |
| `Val.hs` | 行列演算 | hmatrix (BLAS/LAPACK) |

### LSP / SLY 統合

| プロトコル | 用途 | 実装 |
|-----------|------|------|
| LSP | VS Code, Emacs 等での補完・診断 | `Lsp/` |
| Swank | SLY (Emacs) での対話的開発 | `Server.hs` |

---

## ディレクトリ構造

```
Spinor/
├── src/
│   └── Spinor/
│       ├── Syntax.hs      # Parser (Megaparsec)
│       ├── Type.hs        # Type definitions
│       ├── Infer.hs       # Type inference (HM)
│       ├── Val.hs         # Value types, Packages
│       ├── Eval.hs        # Interpreter
│       ├── Expander.hs    # Macro expansion
│       ├── Primitive.hs   # Built-in functions
│       ├── Loader.hs      # File loader
│       ├── GPGPU.hs       # OpenCL integration
│       ├── GL.hs          # OpenGL integration
│       ├── Server.hs      # SLY/Swank server
│       ├── Compiler/
│       │   └── Codegen.hs # C code generation
│       └── Lsp/
│           ├── Server.hs  # LSP server
│           └── Docs.hs    # Documentation
├── runtime/
│   ├── spinor.h           # C runtime header
│   └── spinor.c           # C runtime impl
├── twister/
│   └── *.spin             # Standard library (Spinor)
└── manual/
    └── public/docs/       # Documentation (Markdown)
```

---

## 関連ドキュメント

- [Introduction](introduction) - インストールと入門ガイド
- [Build Guide](build) - ビルド環境の構築
- [API Reference](api-index) - 全プリミティブ一覧
- [Cookbook](cookbook) - 実践的なレシピ集
