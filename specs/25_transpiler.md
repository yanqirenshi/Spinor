# Step 25: トランスパイラ基盤 (C Code Generation) - 技術仕様

## 1. 概要

本仕様は、Spinor 言語の AST (Abstract Syntax Tree) を C99 準拠の C 言語コードに変換するトランスパイラ（コンパイラ）の初期実装に関する技術的な設計を定義する。

目標は、Spinor のサブセットを静的にコンパイルし、C コンパイラ (gcc/clang) を介してネイティブ実行可能ファイルを生成することである。

## 2. ランタイムモデル (`runtime/spinor.h`)

Spinor の動的な型システムを C の静的な型システム上で表現するため、**Tagged Union** アプローチを採用する。すべての Spinor オブジェクトは `SpObject` 構造体として表現される。

### 2.1. 型タグ `SpType`

オブジェクトの型を識別するための enum を定義する。

```c
typedef enum {
    SP_NIL,
    SP_BOOL,
    SP_INT,
    // 将来の拡張用
    SP_SYM,
    SP_PAIR,
    SP_FUN,
    SP_CLOSURE
} SpType;
```

### 2.2. 値ユニオン `SpValue`

`SpType` に応じた実際の値を格納する union を定義する。

```c
// forward declaration
struct SpObject;

typedef struct {
    struct SpObject* car;
    struct SpObject* cdr;
} SpPair;

typedef union {
    bool     boolean;
    long     integer;
    char*    symbol;
    SpPair*  pair;
    // ... 将来の型
} SpValue;
```

### 2.3. オブジェクト構造体 `SpObject`

型タグと値ユニオンをまとめた、Spinor の中心的なデータ構造を定義する。

```c
typedef struct SpObject {
    SpType  type;
    SpValue value;
} SpObject;
```

### 2.4. メモリ管理

初期実装では、メモリ管理を簡素化する。

-   すべての `SpObject` は `malloc` を使してヒープ領域に確保する。
-   **ガベージコレクション (GC) は実装しない。** 生成されたオブジェクトは現時点では解放されない。これは将来的な課題（Step 26 以降）とする。

### 2.5. コンストラクタ API

C コード側で `SpObject` を安全かつ容易に生成するため、以下のヘルパー関数（コンストラクタ）のプロトタイプを定義する。

```c
// Constructor prototypes
SpObject* sp_make_nil();
SpObject* sp_make_bool(bool value);
SpObject* sp_make_int(long value);

// Utility prototypes
void sp_print(SpObject* obj);
```

## 3. コード生成 (`src/Spinor/Compiler/Codegen.hs`)

Haskell で実装されるコードジェネレータは、Spinor の `Expr` を受け取り、C コードの文字列 (`Text`) を生成する。

### 3.1. トップレベル構造

-   生成される C コードは `main` 関数を持つ。
-   `#include "spinor.h"` と `#include <stdio.h>` を冒頭に含める。
-   Spinor のトップレベル式は、`main` 関数内の一連の C の文に変換される。
-   各文の結果は `sp_print` 関数で標準出力に表示される。

### 3.2. AST から C コードへのマッピング

`codegen :: Expr -> Text` は、以下のルールに従って式を変換する。

| Spinor `Expr`                         | 生成される C コード (抜粋)                                 | 説明                                                                     |
| ------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------------------------------ |
| `EInt n`                              | `sp_make_int(n)`                                           | 整数リテラルを `SpObject*` に変換する。                                  |
| `EBool b`                             | `sp_make_bool(b)`                                          | 真偽値リテラルを `SpObject*` に変換する。                                |
| `(if cond then else)`                 | `(codegen(cond)->value.boolean ? codegen(then) : codegen(else))` | C の三項演算子に変換する。`cond` は `SpObject*` であり、その真偽値を参照する。 |
| `(+ a b)`                             | `sp_add(codegen(a), codegen(b))`                           | プリミティブ演算をランタイム関数 `sp_add` の呼び出しに変換する。       |
| `(- a b)`                             | `sp_sub(codegen(a), codegen(b))`                           | 同上。                                                                   |
| その他のプリミティブ (`*`, `/`, `=`) | `sp_mul(...)`, `sp_div(...)`, `sp_eq(...)`                 | 同上。                                                                   |

### 3.3. プリミティブ演算のランタイム関数

`sp_add` などのプリミティブ演算に対応する関数は、ランタイム側 (`spinor.c`) に実装する必要がある。これらの関数は `SpObject*` を引数に取り、計算結果を新しい `SpObject*` として返す。

例: `sp_add`
```c
// spinor.h にプロトタイプを追加
SpObject* sp_add(SpObject* a, SpObject* b);

// spinor.c に実装
SpObject* sp_add(SpObject* a, SpObject* b) {
    // 型チェック (当面は省略しても良いが、将来的には必須)
    long result = a->value.integer + b->value.integer;
    return sp_make_int(result);
}
```
