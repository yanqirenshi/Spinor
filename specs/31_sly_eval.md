# Step 31: SLY Advanced (Interactive Evaluation) - 技術仕様

## 1. 概要

本仕様は、Step 30 で確立した Swank プロトコルの基本実装を拡張し、SLY/Emacs からのより高度でインタラクティブな操作を可能にするための RPC (Remote Procedure Call) に関する技術仕様を定義する。

具体的には、REPL での評価、バッファからのコンパイル（評価）、および標準出力の転送（Output Redirection）を対象とする。

## 2. Swank RPC 拡張仕様

### 2.1. `swank:interactive-eval`

SLY の REPL バッファで入力された式を評価するための RPC。

-   **リクエスト形式:** `(:emacs-rex (swank:interactive-eval "code-string") ...)`
-   **動作:**
    1.  引数として受け取った `code-string` を Spinor の S式 としてパースする。
    2.  パースした式を評価する (`expand` -> `eval`)。
    3.  評価結果の `Val` を、人間が読める形式の文字列に変換する (`showVal`)。
-   **レスポンス形式:** `(:return (:ok "result-string") request-id)`
    -   `result-string` は、評価結果を文字列化したもの。SLY はこの文字列を REPL の出力として表示する。

### 2.2. `swank:compile-string-for-emacs`

Emacs のバッファで `C-c C-c` (関数コンパイル) や `C-c C-k` (ファイルコンパイル) が実行された際に呼び出される RPC。

-   **リクエスト形式:** `(:emacs-rex (swank:compile-string-for-emacs "code-string" buffer-name position-info ...) ...)`
-   **動作:**
    -   Spinor にはインタプリタとコンパイラ（C言語への）があるが、SLYとの連携においては、インタプリタで評価することで「コンパイル」と同等の効果（関数定義の環境への登録など）を得る。
    -   したがって、動作は `swank:interactive-eval` と同様に、受け取った `code-string` を評価する。
-   **レスポンス形式:** `(:return (:ok (notes-list)) request-id)`
    -   `notes-list` はコンパイル結果に関する情報を含むリスト。今回は、成功したことを示す固定のメッセージを返す。
    -   例: `("Compilation finished." "t")` (`t` は成功を示すシンボル)

### 2.3. Output Redirection (標準出力の転送)

Spinor の評価プロセス内で発生した標準出力（例: `(print "hello")`）を、REPL を実行しているクライアントに表示させるための仕組み。

-   **動作フロー:**
    1.  サーバー側で `(print "hello")` が評価される。
    2.  Spinor の `print` プリミティブは、通常 `stdout` に書き込む `IO` アクションを実行する。
    3.  この `IO` アクションの出力先を、サーバー実行時には `stdout` からクライアントへのソケットに動的に切り替える必要がある。
-   **Swank メッセージ仕様:**
    -   キャプチャした出力は、RPC レスポンスとは別に、非同期のイベントメッセージとしてクライアントに送信する。
    -   **形式:** `(:write-string "captured-output\n")`
-   **実装の方向性:**
    -   この機能は、評価エンジン (`Eval` モナド) と `IO` の結合が密になり、実装が複雑になる。
    -   `Eval` モナドの実行コンテキストに、現在の出力先 (ソケットの `Handle` など) を持たせ、`print` プリミティブがそのコンテキストを参照するように改修するアプローチが考えられる。
    -   このステップでは、この機能は**努力目標 (Optional)** とし、まずは `eval` の結果を返す RPC の実装を優先する。
