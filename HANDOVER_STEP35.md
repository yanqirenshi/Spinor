# Step 35 実装引き継ぎ (WSL2/Ubuntu 検証用)

## 概要

Step 35 (Standard Library Expansion) の実装が完了しました。WSL2 (Ubuntu) 環境での検証を行ってください。

## 実装済みの内容

### 1. Primitive.hs - 純粋な文字列操作

追加した関数:
- `string-append` - 複数文字列の連結 (可変長引数)
- `string-length` - 文字列の長さを返す
- `string-length` - 部分文字列を取得 (0-indexed, [start, end))
- `string=?` - 文字列の等価判定
- `string->list` - 文字列を1文字ずつのリストに変換
- `list->string` - 文字列リストを連結

### 2. Eval.hs - I/O 特殊形式

追加した関数:
- `read-file` - ファイルを読み込んで文字列として返す
- `write-file` - ファイルに書き込む (上書き)
- `append-file` - ファイルに追記
- `file-exists?` - ファイルの存在確認
- `command-line-args` - コマンドライン引数を取得
- `getenv` - 環境変数を取得

追加した言語拡張:
- `TypeApplications` (`try @IOException` のため)

追加したインポート:
- `Control.Exception (try, IOException)`
- `System.Directory (doesFileExist)`
- `System.Environment (getArgs, lookupEnv)`

### 3. C ランタイム (spinor.h / spinor.c)

- `SpType` enum に `SP_STR` を追加
- `SpValue` union に `char* string` フィールドを追加
- 文字列コンストラクタ: `sp_make_str()`
- 文字列操作: `sp_str_append()`, `sp_str_length()`, `sp_substring()`, `sp_str_eq()`
- ファイル I/O: `sp_read_file()`, `sp_write_file()`, `sp_append_file()`, `sp_file_exists()`
- `sp_print()` に `SP_STR` ケースを追加

### 4. Codegen.hs - コード生成ルール

- `EStr` リテラルを `sp_make_str("...")` に変換
- `escapeC` ヘルパー関数を追加 (C 文字列エスケープ)
- 文字列操作・I/O のコード生成ルールを追加
- `primitives` リストに新規関数名を追加

## 検証手順

### 1. ビルド

```bash
cd /path/to/Spinor
cabal build
```

### 2. インタプリタでのテスト

```bash
cabal run spinor -- twister/test-stdlib.spin
```

期待される出力:
```
Loading Twister environment...
Twister loaded.
=== String Tests ===
Hello, World!
5
0
el
hello
#t
#f
("a" "b" "c")
abc
=== File I/O Tests ===
Hello from Spinor!
Second line

#t
#f
=== All tests passed ===

All tests passed.
```

### 3. Haskell テストスイート

```bash
cabal test
```

### 4. コンパイラでのテスト (Optional)

```bash
cabal run spinor -- build twister/test-stdlib.spin -o test-stdlib
./test-stdlib
```

## Windows での問題点

Windows (GHC 9.6.7 + MSYS2/MinGW64) 環境で `network` パッケージのビルドに失敗:

```
include/HsNetDef.h:4:10: fatal error: 'HsNetworkConfig.h' file not found
```

この問題は `Spinor.Server` モジュール (Swank サーバー) にのみ影響します。
Step 35 の機能自体は正常に動作することを確認済みです。

WSL2/Ubuntu では `network` パッケージが正常にビルドできる可能性が高いため、
フルビルドの検証をお願いします。

## 変更されたファイル一覧

```
src/Spinor/Primitive.hs      - 文字列操作関数の追加
src/Spinor/Eval.hs           - I/O 特殊形式の追加
src/Spinor/Compiler/Codegen.hs - コード生成ルールの追加
runtime/spinor.h             - SP_STR 型と関数宣言
runtime/spinor.c             - C 実装
twister/test-stdlib.spin     - テストファイル (新規作成)
```

## 実装報告の記入

検証完了後、`tasks/35_stdlib.md` の「実装報告」セクションに結果を記入してください。
