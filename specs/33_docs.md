# Step 33: Documentation & Polish - 技術仕様

## 1. 概要

本仕様は、Spinor プロジェクトの機能セットがほぼ完成したことを受け、ユーザーがプロジェクトを容易に理解し、利用を開始できるようにするためのドキュメント体系の整備と、コマンドラインインターフェース (CLI) の品質向上に関する仕様を定義する。

## 2. ドキュメント体系の仕様

プロジェクトのドキュメントを `README.md`（入り口）、`docs/reference.md`（言語仕様）、`docs/emacs.md`（開発環境）の3つに整理・拡充する。

### 2.1. README.md (トップページ)

プロジェクトの顔として、概要と最新の機能を魅力的に伝えるように全面的に刷新する。

-   **キャッチコピー:** プロジェクトの核心を示す `"Static Lisp with Haskell Semantics"` を強調する。
-   **Features (主要機能):** これまで実装した以下の機能を網羅し、アピールする。
    -   Static Typing (Hindley-Milner)
    -   Let Polymorphism
    -   Hygienic Macros
    -   Native Compilation (via C)
    -   Tail Call Optimization (Self-recursion)
    -   WebAssembly Support
    -   SLY/SLIME Integration (Swank Server)
-   **Quick Start:**
    -   Cabal を使ったインストールと REPL の起動方法。
    -   ネイティブバイナリをビルドする簡単な例 (`hello.spin` -> `spinor build` -> `./hello`) を追加する。
-   **Live Demo:** Step 32 で作成するブラウザ REPL へのリンクを設置する。（例: `[Live Demo on GitHub Pages](...)`）

### 2.2. `docs/reference.md` (言語リファレンス)

Spinor 言語のコア機能について、説明と用例を記載する。

-   **対象:**
    -   **特殊形式:** `defun`, `mac`, `if`, `let`, `match`, `data`, `quote`, `setq`, `begin`/`progn`.
    -   **主要な組込み関数:**
        -   **算術:** `+`, `-`, `*`, `/`
        -   **比較:** `=`, `>`, `<`, `>=`, `<=`
        -   **リスト:** `cons`, `car`, `cdr`, `list`
        -   **型述語:** `nil?`, `list?`, `int?` 等
        -   **その他:** `print`, `error`, `bound?`
-   **フォーマット:** 各項目について、「構文」「説明」「例」を簡潔に記述する。

### 2.3. `docs/emacs.md` (開発環境ガイド)

Emacs + SLY を使ったインタラクティブな開発体験を提供するための手順をまとめる。

-   **`spinor-mode.el` の設定:** `editors/emacs/spinor-mode.el` を Emacs の `load-path` に追加し、`.spin` ファイルでモードが有効になるように設定例 (`(add-to-list 'auto-mode-alist ...)` ) を記載する。
-   **SLY との接続手順:**
    1.  ターミナルで `spinor server` を起動する。
    2.  Emacs で `M-x sly-connect` を実行する。
    3.  ホスト `localhost` とポート `4005` を入力して接続する。
-   **基本的な使い方:** `C-x C-e` (式評価)、`C-c C-c` (関数コンパイル) などの基本的な SLY の操作を紹介する。

## 3. CLI UX (User Experience) の仕様

`app/Main.hs` のコマンドライン引数処理を刷新し、よりユーザーフレンドリーにする。

-   **`--help` / 引数なし:**
    -   `spinor --help` または引数なしで実行した場合、単なる Usage ではなく、各サブコマンドの概要を含む詳細なヘルプメッセージを表示する。
    -   **サブコマンド:** `repl` (default), `build`, `compile`, `server`
-   **`--version`:**
    -   `spinor --version` を実行すると、`spinor version x.y.z` のようにバージョン情報を表示する。
    -   バージョン番号は `package.yaml` に記載されたバージョンと連動させる。
-   **実装方針:** 引数パーシングライブラリ `optparse-applicative` を導入し、宣言的に CLI を構築する。
