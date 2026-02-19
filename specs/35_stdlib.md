# Step 35: Standard Library Expansion (String & I/O) - 技術仕様

## 1. 概要
Spinor を実用的なスクリプト言語にするため、基本的な文字列操作とファイル入出力機能を追加する。
インタプリタ (Haskell) とコンパイラ (C) の両方で動作することを目標とする。

## 2. 追加するプリミティブ関数

### 2.1. String Operations (文字列操作)

| 関数名 | シグネチャ | 説明 | 例 |
|--------|-----------|------|-----|
| `string-append` | `(String...) -> String` | 複数の文字列を連結する (可変長引数) | `(string-append "Hello" " " "World")` → `"Hello World"` |
| `string-length` | `(String) -> Int` | 文字列の長さ (文字数) を返す | `(string-length "hello")` → `5` |
| `substring` | `(String, Int, Int) -> String` | 部分文字列を取得 (start, end) | `(substring "hello" 1 3)` → `"el"` |
| `string=?` | `(String, String) -> Bool` | 文字列の等価判定 | `(string=? "abc" "abc")` → `#t` |
| `string->list` | `(String) -> List` | 文字列を1文字ずつのリストに変換 | `(string->list "ab")` → `("a" "b")` |
| `list->string` | `(List) -> String` | 文字列のリストを連結して1つの文字列に | `(list->string ("a" "b"))` → `"ab"` |

### 2.2. File I/O (ファイル入出力)

| 関数名 | シグネチャ | 説明 | 例 |
|--------|-----------|------|-----|
| `read-file` | `(String) -> String` | ファイルパスを受け取り、内容を文字列として返す | `(read-file "data.txt")` |
| `write-file` | `(String, String) -> Bool` | パスと内容を受け取り、ファイルに書き込む (上書き) | `(write-file "out.txt" "hello")` |
| `append-file` | `(String, String) -> Bool` | パスと内容を受け取り、ファイルに追記する | `(append-file "log.txt" "entry\n")` |
| `file-exists?` | `(String) -> Bool` | ファイルが存在するかを判定 | `(file-exists? "data.txt")` |

### 2.3. System (システム)

| 関数名 | シグネチャ | 説明 | 例 |
|--------|-----------|------|-----|
| `command-line-args` | `() -> List` | コマンドライン引数のリストを返す | `(command-line-args)` → `("arg1" "arg2")` |
| `getenv` | `(String) -> String` | 環境変数の値を取得 (未定義なら空文字列) | `(getenv "HOME")` |

## 3. Implementation Strategy

### 3.1. Interpreter (Haskell)

**対象ファイル:** `src/Spinor/Primitive.hs`

- **文字列操作:** `Data.Text` の関数 (`T.append`, `T.length`, `T.take`, `T.drop` 等) を使用
- **ファイル I/O:** `Data.Text.IO` の `readFile`, `writeFile` をラップ
  - I/O 操作は `VPrim` の関数型では実行できないため、`Eval.hs` に特殊形式として追加する
- **システム:** `System.Environment` の `getArgs`, `lookupEnv` を使用

#### I/O プリミティブの実装方針

`VPrim` は純粋関数 (`[Val] -> Either Text Val`) のため、I/O を伴う操作は以下のいずれかで実装する:

1. **特殊形式として Eval.hs に追加** (推奨)
   - `eval (EList [ESym "read-file", pathExpr]) = ...`
   - `liftIO` を使って I/O を実行可能

2. **VPrimIO コンストラクタを Val に追加**
   - より汎用的だが、Val.hs の変更が必要

本 Step では方針 1 を採用し、I/O 系は `Eval.hs` に特殊形式として追加する。
純粋な文字列操作は `Primitive.hs` に追加する。

### 3.2. Compiler / C Runtime

**対象ファイル:**
- `runtime/spinor.h` - 型定義と関数宣言
- `runtime/spinor.c` - 実装
- `src/Spinor/Compiler/Codegen.hs` - コード生成ルール

#### C ランタイムの拡張

**文字列型の追加:**
```c
typedef enum {
    SP_NIL,
    SP_BOOL,
    SP_INT,
    SP_STR,   // 新規追加
    ...
} SpType;

typedef union {
    bool     boolean;
    long     integer;
    char*    string;  // 新規追加 (heap-allocated)
    ...
} SpValue;
```

**追加する関数:**
```c
// コンストラクタ
SpObject* sp_make_str(const char* s);

// 文字列操作
SpObject* sp_str_append(SpObject* a, SpObject* b);
SpObject* sp_str_length(SpObject* s);
SpObject* sp_substring(SpObject* s, SpObject* start, SpObject* end);
SpObject* sp_str_eq(SpObject* a, SpObject* b);

// ファイル I/O
SpObject* sp_read_file(SpObject* path);
SpObject* sp_write_file(SpObject* path, SpObject* content);
SpObject* sp_append_file(SpObject* path, SpObject* content);
SpObject* sp_file_exists(SpObject* path);
```

#### Codegen.hs の拡張

```haskell
-- 文字列リテラル
compileExpr (EStr s) = "sp_make_str(\"" <> escapeC s <> "\")"

-- 文字列操作
compileExpr (EList [ESym "string-append", a, b]) =
    "sp_str_append(" <> compileExpr a <> ", " <> compileExpr b <> ")"
-- ...
```

## 4. エラーハンドリング

| 状況 | インタプリタ | コンパイラ (C) |
|------|-------------|---------------|
| 型エラー | `throwError` で明確なメッセージ | `fprintf(stderr, ...)` + 適切な戻り値 |
| ファイル読み込み失敗 | `throwError` | `NULL` を返す or エラー値 |
| 範囲外の substring | 空文字列を返す | 空文字列を返す |

## 5. 優先度

| 優先度 | 機能 |
|--------|------|
| P0 (必須) | `string-append`, `string-length`, `read-file`, `write-file` |
| P1 (重要) | `substring`, `string=?`, `append-file`, `file-exists?` |
| P2 (任意) | `string->list`, `list->string`, `command-line-args`, `getenv` |

## 6. 将来の拡張
- 正規表現サポート (`string-match`, `string-replace`)
- ストリーム I/O (`open-input-file`, `read-line`, `close-port`)
- バイナリファイル操作
