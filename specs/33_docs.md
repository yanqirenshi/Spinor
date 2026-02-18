# Step 33: Documentation & Polish - 技術仕様

## 1. 概要
本仕様は、Spinor プロジェクトの集大成として、ユーザーおよび開発者向けのドキュメント体系を整備し、CLI の使い勝手（UX）を向上させるためのガイドラインを定義する。

## 2. ドキュメント体系の仕様

### 2.1. README.md (トップページ)
プロジェクトの顔として、現状の「型推論 Lisp」から「実用的なコンパイル・開発環境を備えた言語」へと説明をアップデートする。

- **キャッチコピー:** "Static Lisp with Haskell Semantics"
- **Features (主要機能):**
    - **Hindley-Milner Type System:** Let多相を備えた静的型推論。
    - **Native Compilation:** C言語経由でのネイティブバイナリ生成。
    - **Tail Call Optimization (TCO):** 自己再帰のループ変換によるスタック消費の抑制。
    - **WebAssembly Support:** ブラウザ上で動作する REPL。
    - **SLY/SLIME Integration:** Swank プロトコルによる Emacs インタラクティブ開発。
- **Quick Start:** 
    - `cabal run spinor` による REPL 起動。
    - `spinor build hello.spin` によるバイナリ作成。
- **WASM Demo Link:** Step 32 で構築した `web/index.html` への誘導。

### 2.2. docs/reference.md (リファレンス)
言語仕様の網羅的なドキュメント。
- **特殊形式:** 
    - `defun`: 関数定義
    - `if`: 条件分岐
    - `let`: 変数束縛 (Let多相)
    - `match`: パターンマッチング
    - `data`: 代数的データ型定義
    - `mac`: ハイジニックマクロ
- **プリミティブ関数:** 算術演算、リスト操作 (`cons`, `car`, `cdr`)、入出力 (`print`) 等。

### 2.3. docs/emacs.md (開発環境)
Emacs ユーザー向けの導入ガイド。
- `spinor-mode.el` の読み込み設定。
- `spinor server` によるバックエンド起動。
- `M-x sly-connect` による接続と、バッファ評価 (`C-M-x`) のデモ。

## 3. CLI UX の仕様

### 3.1. ヘルプメッセージの刷新
`spinor --help` または引数なし実行時に、以下のサブコマンドの説明を網羅した親切なメッセージを表示する。

- `repl`: インタプリタ REPL を起動 (デフォルト)
- `build <file>`: ネイティブバイナリを生成 (トランスパイル + GCC)
- `compile <file>`: Cソースコードのみを出力
- `server [--port <port>]`: SLY 接続用の Swank サーバーを起動

### 3.2. バージョン表示
- `spinor --version` で、`spinor.cabal` (または `package.yaml`) に定義されたバージョン情報を表示する。
