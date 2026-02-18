# Step 28: WebAssembly (WASM) 対応 - 実装指示書

## 概要

このタスクでは、Spinor プログラムを WebAssembly にコンパイルし、ブラウザで実行するまでの手動ビルドフローを確立します。既存の C 言語トランスパイラと、外部ツールである Emscripten (`emcc`) を組み合わせます。

**注意:** このタスクは `spinor` コマンド自体の改修は含みません。一連のコマンドを手動で実行することで、WASM ビルドが可能であることを実証します。

## Prerequisites (事前準備)

-   **Emscripten SDK のインストール:**
    -   このタスクには `emcc` コマンドが必要です。
    -   [Emscripten の公式サイト](https://emscripten.org/docs/getting_started/downloads.html) の指示に従い、Emscripten SDK をインストールし、パスを設定しておいてください。

## Steps (実装手順)

### 1. テスト用の Spinor ソースを作成

WASM で実行するための簡単な Spinor プログラムを作成します。

1.  **ファイル名:** `test-wasm.spin`
2.  **内容:**
    -   現在のコンパイラは文字列リテラルをサポートしていないため、数値計算の結果が正しく表示されることを確認します。

    ```lisp
    ;; test-wasm.spin

    (defun fact (n)
      (if (<= n 1)
          1
          (* n (fact (- n 1)))))

    ;; この計算結果 (120) がブラウザに表示されるはず
    (fact 5)
    ```

### 2. C コードへのトランスパイル

既存の `compile` サブコマンドを使って、`test-wasm.spin` を `output.c` に変換します。

```sh
cabal run spinor -- compile test-wasm.spin
```
-   カレントディレクトリに `output.c` が生成されていることを確認してください。

### 3. WebAssembly へのコンパイル

`emcc` を使って、生成された C コードと Spinor ランタイムを WebAssembly にコンパイルします。

```sh
emcc output.c runtime/spinor.c -Iruntime -o index.html -O2
```

-   **`-Iruntime`:** `runtime/spinor.h` を見つけるために必要です。
-   **`-o index.html`:** WASM, JS, HTML の3点セットを生成するよう指示します。
-   **`-O2`:** 最適化オプション（推奨）。
-   コマンド実行後、`index.html`, `index.js`, `index.wasm` が生成されていることを確認してください。

### 4. 動作確認 (Verification)

生成された HTML ファイルをブラウザで開いて実行結果を確認します。

1.  **ローカルサーバーの起動:**
    -   ブラウザはセキュリティ上の理由から、ローカルファイル (`file://...`) からの WASM の実行を制限することがあります。`http://` 経由でアクセスするために、簡単なローカルウェブサーバーを起動してください。
    -   (Python 3 がある場合)
        ```sh
        python -m http.server
        ```
    -   (Node.js と `npx` がある場合)
        ```sh
        npx http-server
        ```

2.  **ブラウザで確認:**
    -   ウェブブラウザを開き、`http://localhost:8000/index.html` (サーバーの指示に従ってください) にアクセスします。
    -   ページ上のテキストエリアに、`fact 5` の計算結果である **`120`** が表示されることを確認してください。

### 5. (任意) 自動化スクリプトの作成

ここまでの手順 2 と 3 を自動化するため、簡単なシェルスクリプトを作成することを推奨します。

1.  **ファイル:** `scripts/build-wasm.sh` を作成します。
2.  **内容:**
    ```sh
    #!/bin/sh
    set -e # エラーが発生したらスクリプトを終了

    INPUT_FILE=$1

    if [ -z "$INPUT_FILE" ]; then
      echo "Usage: $0 <input.spin>"
      exit 1
    fi

    echo "Step 1: Transpiling $INPUT_FILE to C..."
    cabal run spinor -- compile "$INPUT_FILE"

    echo "Step 2: Compiling C to WebAssembly with emcc..."
    emcc output.c runtime/spinor.c -Iruntime -o index.html -O2

    echo "Build successful! Created index.html, index.js, and index.wasm."
    echo "Run a local web server and open index.html to see the result."
    ```
3.  **実行:**
    ```sh
    chmod +x scripts/build-wasm.sh
    ./scripts/build-wasm.sh test-wasm.spin
    ```

---

## 実装報告

**実装完了後、この Markdown ファイルを直接編集し、以下の2つのセクションを追記して実装内容を報告してください。**

#### 実装方針

- **Emscripten:** emsdk v5.0.1 (emcc 5.0.1) を `~/emsdk/` にインストールして使用。Docker は不使用。
- **動作確認方法:** Node.js (v24.13.0) で `index.js` を直接実行して簡易確認。WSL2 環境のためブラウザ確認は省略。
- **`<=` / `>=` 演算子の追加:** テストプログラムが `<=` を使用しているが、既存のコンパイラとランタイムは `<=` / `>=` をサポートしていなかったため、本タスクの一環として追加実装した。

#### 実装内容

**1. 新規ファイル:**

- `test-wasm.spin` — 階乗計算テスト (`(fact 5)` → `120`)
- `scripts/build-wasm.sh` — C トランスパイル → WASM ビルドの自動化スクリプト

**2. コンパイラ修正 (`src/Spinor/Compiler/Codegen.hs`):**

- `<=` → `sp_lte()`, `>=` → `sp_gte()` のコード生成パターンを追加。
- プリミティブ演算子リストに `"<="`, `">="` を追加し、ユーザー定義関数呼び出しと誤認されないようにした。

**3. ランタイム修正 (`runtime/spinor.h`, `runtime/spinor.c`):**

- `sp_lte()`, `sp_gte()` 関数を宣言・実装。

**4. 生成された `output.c`:**

```c
#include <stdio.h>
#include <stdbool.h>
#include "spinor.h"

SpObject* user_fact(SpObject* user_n) {
    return (sp_lte(user_n, sp_make_int(1))->value.boolean ? sp_make_int(1) : sp_mul(user_n, user_fact(sp_sub(user_n, sp_make_int(1)))));
}

int main(void) {
    sp_print(user_fact(sp_make_int(5)));
    return 0;
}
```

**5. WASM ビルド結果:**

```
$ emcc output.c runtime/spinor.c -Iruntime -o index.html -O2
```

生成ファイル:
- `index.html` (19,571 bytes)
- `index.js` (21,342 bytes)
- `index.wasm` (11,562 bytes)

**6. Node.js 実行結果:**

```
$ node index.js
120
```

`fact(5) = 120` が正しく出力された。
