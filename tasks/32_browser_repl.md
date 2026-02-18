# Step 32: Browser REPL UI (WASM Integration)

# 仕様

## 目標

xterm.js を使ったブラウザ上の対話型 REPL を実現する。
既存のワンショット WASM ビルド（`scripts/build-wasm.sh`）とは別に、対話的な REPL 専用のビルドパイプラインを新設する。

## 要件

- C 層に簡易 S 式評価器（Tokenizer → Parser → Evaluator）を実装し、`sp_eval_string()` を Emscripten 経由で JS に公開する
- xterm.js v5.3.0 でターミナル UI を構築し、行入力を `Module.ccall()` で C 関数に渡す
- 既存ファイル（`runtime/spinor.c`, `scripts/build-wasm.sh`, Haskell ソース）は変更しない

## 対応する構文

- 整数リテラル: `42`, `-7`
- 真偽値: `#t`, `#f`
- nil: `nil`, `()`
- 算術演算: `+`, `-`, `*`, `/`（可変長引数対応）
- 比較演算: `=`, `<`, `>`, `<=`, `>=`
- 条件式: `(if cond then else)`
- ネスト式: `(+ (* 3 4) (- 10 5))`

## アーキテクチャ

```
[xterm.js Terminal] → onData → [repl.js (line buffer)]
                                    ↓ Enter
                              Module.ccall('sp_eval_string', ...)
                                    ↓
                              [repl.c: tokenize → parse → eval]
                                    ↓
                              SpObject* → sp_format() → string
                                    ↓
                              [xterm.js Terminal] ← writeln
```

# 実装方針

## 概要

JS-driven REPL + C exported eval function 方式を採用する。
C 側は pure function（入力文字列 → 結果文字列）として設計し、状態管理は JS 側（xterm.js + 行バッファ）に任せる。

## 設計判断

### C 層の評価器を static pool ベースにした理由

WASM 環境では malloc/free のオーバーヘッドが大きいため、AST ノードとトークンは static 配列（ノードプール）で管理する。
`sp_eval_string()` 呼び出しごとにプールをリセットするため、メモリリークも発生しない。

### 既存 `spinor.c` のランタイム関数を再利用

`sp_add()`, `sp_sub()` 等の既存プリミティブ演算関数をそのまま呼び出す。
評価器は演算子名でディスパッチし、対応する `sp_*()` 関数に委譲する。

### `sp_format()` を `spinor.h` にも宣言した理由

`sp_format()` の実装は `repl.c` にあるが、将来的に他のモジュールからも利用可能にするため `spinor.h` に前方宣言を置いた。

## モジュール構成

| モジュール | 責務 |
|-----------|------|
| `runtime/repl.c` (Tokenizer) | 入力文字列をトークン列（`(`, `)`, INT, SYMBOL, BOOL, EOF）に分解 |
| `runtime/repl.c` (Parser) | トークン列を再帰下降で AST（`ASTNode`）に変換 |
| `runtime/repl.c` (Evaluator) | AST を走査し、`sp_*()` ランタイム関数にディスパッチ |
| `web/repl.js` | xterm.js の入力イベントを行バッファリングし、Enter で `sp_eval_string()` を呼び出す |
| `web/index.html` | xterm.js + FitAddon のロードとレイアウト |

# タスク

### Step A: C ランタイム拡張 ✅

- [x] `runtime/repl.h` — `sp_eval_string()`, `sp_format()` 宣言
- [x] `runtime/repl.c` — Tokenizer, Parser, Evaluator 実装
- [x] `runtime/spinor.h` — `sp_format()` 宣言追加

### Step B: Web アセット ✅

- [x] `web/index.html` — xterm.js v5.3.0 + FitAddon、カスタムテーマ
- [x] `web/repl.js` — xterm.js ↔ WASM ブリッジ（履歴、Backspace、Ctrl+C 対応）

### Step C: ビルドスクリプト ✅

- [x] `scripts/build-wasm-repl.sh` — REPL 用 WASM ビルド

### Step D: 動作確認

```sh
# 1. ビルド
./scripts/build-wasm-repl.sh

# 2. ローカルサーバー起動
cd web && python3 -m http.server 8000

# 3. ブラウザで http://localhost:8000 にアクセス
```

# 実装内容

## 変更ファイル一覧

| ファイル | 操作 | 概要 |
|---------|------|------|
| `runtime/repl.h` | 新規 | `sp_eval_string()`, `sp_format()` の関数宣言 |
| `runtime/repl.c` | 新規 | Tokenizer, Parser, Evaluator の3サブシステム + `sp_eval_string()` エントリポイント |
| `runtime/spinor.h` | 修正 | `sp_format()` の前方宣言を追加（1行） |
| `web/index.html` | 新規 | xterm.js v5.3.0 + FitAddon CDN 読み込み、Catppuccin テーマ適用 |
| `web/repl.js` | 新規 | xterm.js ↔ WASM ブリッジ（行バッファ、履歴、Ctrl+C） |
| `scripts/build-wasm-repl.sh` | 新規 | `emcc` で `repl.c` + `spinor.c` → `web/spinor-repl.js` + `.wasm` |

## 主要関数

| 関数 | ファイル | 説明 |
|------|---------|------|
| `sp_eval_string(const char*)` | `repl.c` | 文字列を受け取り、tokenize → parse → eval して結果文字列を返す |
| `sp_format(SpObject*)` | `repl.c` | `SpObject*` を表示用文字列にフォーマット |
| `tokenize(const char*)` | `repl.c` | 入力を `Token` 配列（static, 最大256）に分解 |
| `parse_expr()` | `repl.c` | 再帰下降パーサ。`ASTNode` プール（static, 最大256）を使用 |
| `eval_node(ASTNode*)` | `repl.c` | AST を走査し `sp_*()` 関数にディスパッチ。`if` 式は短絡評価 |
| `initREPL()` | `repl.js` | xterm.js Terminal 初期化、`onData` ハンドラ登録 |
| `evalAndPrint(input)` | `repl.js` | `Module.ccall('sp_eval_string', ...)` を呼び結果を表示 |

## テスト結果

gcc によるネイティブビルドで 21/21 テストケースが PASS:

```
PASS: 42 => 42
PASS: -7 => -7
PASS: #t => #t
PASS: #f => #f
PASS: nil => ()
PASS: () => ()
PASS: (+ 1 2) => 3
PASS: (- 10 3) => 7
PASS: (* 4 5) => 20
PASS: (/ 10 2) => 5
PASS: (= 1 1) => #t
PASS: (= 1 2) => #f
PASS: (< 1 2) => #t
PASS: (> 1 2) => #f
PASS: (<= 3 3) => #t
PASS: (>= 5 3) => #t
PASS: (if #t 42 0) => 42
PASS: (if #f 42 0) => 0
PASS: (if (> 10 5) 42 0) => 42
PASS: (+ (* 3 4) (- 10 5)) => 17
PASS: (+ 1 2 3 4 5) => 15

21/21 tests passed
```

## WASM ビルド結果

`~/emsdk` の `emcc 5.0.1` でビルド成功:

```
$ ./scripts/build-wasm-repl.sh
Building Spinor Browser REPL (WASM)...
Build successful!
  web/spinor-repl.js   (13K)
  web/spinor-repl.wasm (19K)
```

## 注意事項

- WASM ビルドには `emcc`（Emscripten）が必要。`~/emsdk` にインストール済み
- `repl.c` の `main()` は `#ifndef SPINOR_REPL_NO_MAIN` で囲んでおり、テスト時には `-DSPINOR_REPL_NO_MAIN` を付けて別の `main()` を使用可能
