# Step 28: WebAssembly (WASM) 対応 - 技術仕様

## 1. 概要

本仕様は、Spinor で書かれたプログラムを WebAssembly にコンパイルし、ウェブブラウザ上で実行可能にするためのビルドフローと技術的要件を定義する。

目標は、既存の C 言語へのトランスパイラ基盤を活用し、**Emscripten (`emcc`)** をツールチェーンに組み込むことで、WASM へのコンパイルパスを確立することである。

## 2. WASM ビルドフロー

### 2.1. ツールチェーン

1.  **Spinor Compiler (`spinorc`):** Spinor ソースコード (`.spin`) を C ソースコード (`.c`) にトランスパイルする。
2.  **Emscripten (`emcc`):** Spinor Compiler が生成した C コードと、Spinor の C ランタイム (`spinor.c`) を入力とし、WebAssembly (`.wasm`) と、それを実行するための JavaScript グルーコード (`.js`) および HTML (`.html`) を出力する。

### 2.2. コンパイルプロセス

1.  **入力:**
    -   `output.c`: `spinorc` によって生成された C コード。
    -   `runtime/spinor.c`: Spinor の C ランタイム実装。
    -   `runtime/spinor.h`: Spinor の C ランタイムヘッダ。

2.  **コンパイルコマンド:**
    -   `emcc` を使用して、C ソースを WASM ターゲットにコンパイルする。

    ```sh
    emcc output.c runtime/spinor.c -Iruntime -o index.html
    ```
    -   **`-Iruntime`:** `#include "spinor.h"` を解決するために、`runtime` ディレクトリをインクルードパスに追加する。
    -   **`-o index.html`:** Emscripten に対して、WASM バイナリ、JS グルーコード、および実行用の HTML ハーネスを含む完全なウェブアプリケーションセットを生成するよう指示する。

3.  **出力:**
    -   `index.wasm`: コンパイルされた WebAssembly モジュール。
    -   `index.js`: WASM モジュールをロードし、ブラウザ API との連携（`printf` から `console.log` へのリダイレクト等）を行うための JavaScript コード。
    -   `index.html`: `index.js` を読み込み、実行結果を表示するための HTML ファイル。

## 3. ランタイムと制約事項

### 3.1. C ランタイムの互換性

-   現在の `runtime/spinor.c` および `spinor.h` は、標準的な C99 機能（`stdio.h`, `stdlib.h`, `stdbool.h`）のみに依存しているため、Emscripten でのコンパイルに際して、**コードの変更は原則不要**である。

### 3.2. 標準出力 (I/O)

-   C コード内の `printf` による標準出力は、Emscripten が生成する JS グルーコードによって捕捉され、ブラウザの **JavaScript コンソール** (`console.log`) にリダイレクトされる。
-   また、`emcc` が生成する `index.html` には、デフォルトで標準出力を表示するための `<textarea>` 要素が含まれる。

### 3.3. 制約事項 (スコープ外)

-   **ファイルシステム:** WebAssembly はサンドボックス内で実行されるため、ホストマシンのファイルシステムへの直接アクセスはできない。ファイル I/O が必要なプログラムは、Emscripten の仮想ファイルシステム API を利用する必要があるが、これは今回のスコープ外とする。
-   **ネットワーク:** TCP/IP ソケットなどの低レベルなネットワーク機能はブラウザ環境では利用できないため、`Spinor.Server` などの機能は WASM ターゲットでは動作しない。
-   **高度な DOM 操作:** Spinor プログラムから直接 HTML の DOM を操作するには、JavaScript との連携を密にするための追加実装が必要であり、今回は対象外とする。
