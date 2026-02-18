# Step 32: Browser REPL UI (WASM Integration) - 技術仕様

## 1. 概要

本仕様は、Step 28 で確立した WebAssembly ビルドフローを基盤とし、ブラウザ上で動作するインタラクティブな Spinor REPL (Read-Eval-Print Loop) を構築するための技術仕様を定義する。

`xterm.js` を用いたターミナル UI をフロントエンドに据え、ユーザー入力と WASM 化された Spinor 実行環境の標準入出力を接続することで、リッチな REPL 体験の提供を目指す。

## 2. Web UI アーキテクチャ

### 2.1. フロントエンド技術スタック

-   **HTML/CSS:** モダンブラウザに対応した標準的な HTML5 および CSS。
-   **JavaScript:** フレームワークは使用せず、ES6 Modules 形式のプレーンな JavaScript で実装する。これにより、依存関係とビルドの複雑さを最小限に抑える。
-   **Terminal Emulator:** `xterm.js` ライブラリを採用し、リッチなコンソール UI を実現する。`xterm.js` 本体および、サイズ調整のための `xterm-addon-fit` は CDN 経由で読み込む。

### 2.2. WebAssembly 統合 (I/O パイプライン)

ブラウザの JavaScript 環境と、Emscripten によってコンパイルされた WASM 環境との間で、標準入出力 (stdin/stdout) を相互に接続する。

-   **出力 (WASM -> JS -> xterm.js):**
    -   Spinor の C ランタイム (`spinor.c`) 内の `printf` による出力は、Emscripten のランタイムによってフックされる。
    -   Emscripten の `Module` オブジェクトが提供する `print` コールバックを上書きする。
    -   このコールバック関数内で、WASM からの出力文字列を受け取り、`xterm.js` のターミナルインスタンスの `.write()` メソッドに渡す。
    -   **実装例 (JS側):**
        ```javascript
        const term = new Terminal();
        const Module = {
          print: (text) => {
            term.write(text + '
');
          },
          // ... other Emscripten settings
        };
        ```

-   **入力 (xterm.js -> JS -> WASM):**
    -   `xterm.js` の `.onData()` イベントリスナーを使用して、ユーザーのキー入力を一文字ずつキャプチャする。
    -   キャプチャした文字列を、Emscripten が仮想化した標準入力 (`stdin`) に書き込む。これは `FS.write()` のような Emscripten の仮想ファイルシステム API を通じて行う。

## 3. REPL Loop (Spinor C ランタイム側)

ブラウザからの継続的な入力を処理するため、C 側のエントリーポイントである `main` 関数を、ワンショット実行から無限ループに変更する。

-   **ループ構造:** `main` 関数内に `while(1)` ループを設ける。
-   **入力処理:**
    -   `printf("spinor> "); fflush(stdout);` でプロンプトを表示し、`fgets(buffer, sizeof(buffer), stdin)` を使用して標準入力から一行分のデータを読み込む。
    -   Emscripten 環境下では、`stdin` バッファが空の場合、`fgets` は `NULL` を返し、プログラムの実行は中断され、ブラウザのイベントループに制御が戻る。JS 側から `stdin` にデータが書き込まれると、再度 C 側のループが再開される。
-   **評価と出力:**
    -   `fgets` で読み取った文字列を、評価器 (`sp_eval_string`) に渡す。
    -   評価結果は `sp_print` を介して `printf` で出力され、上記の出力パイプラインを通じて `xterm.js` に表示される。

### 3.1. 評価 API の新設 (簡易実装)

-   C ランタイムは現在、Spinor の S式文字列を直接評価する機能を持たない。このため、`char* sp_eval_string(const char* input)` のような関数を `runtime/spinor.c` に新設する必要がある。
-   **課題:** C 言語で完全な S式パーサとエバリュエータを実装するのは、このステップの範囲を大きく超える。
-   **暫定仕様:** `sp_eval_string` は、ブラウザ REPL の I/O パイプラインを実証するため、`(+ 1 2)` のようなごく単純な数値加算の S式 のみを `sscanf` 等で解釈・評価する**ダミー実装**とする。これにより、エンドツーエンドのインタラクティブな I/O が機能することの証明を優先する。
