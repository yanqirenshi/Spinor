# Step 33: Documentation & Polish - 実装指示書

## 概要
最新の機能セット（TCO、WASM、SLY対応）を反映したドキュメントを整備し、CLI コマンドのインターフェースを洗練させてください。

## Steps

### 1. README.md のリファクタリング
- 現状の `README.md` を `specs/33_docs.md` の内容に基づいて全面的に書き換える。
- 特に "Features" セクションに TCO や WebAssembly、SLY 連携の項目を追加すること。
- 「ビルド方法」のセクションに、Cコンパイラ(GCC等)が必須であることを明記する。

### 2. ドキュメントの新規作成
- `docs/reference.md` を作成し、主要な特殊形式とプリミティブ関数の説明を記述する。
- `docs/emacs.md` を作成し、`spinor-mode.el` の設定方法と `sly` での接続手順を記述する。

### 3. CLI Polish (app/Main.hs)
- `main` 関数を修正し、引数が不足している場合や `--help` が指定された場合に、サブコマンド一覧を含むヘルプメッセージを表示するようにする。
- `optparse-applicative` ライブラリの導入も検討して良いが、依存を増やしたくない場合は単純なパターンマッチでも可。
- `--version` フラグに対応し、プログラムのバージョンを表示する。

### 4. 最終確認
- `cabal run spinor -- --help` でメッセージが表示されること。
- `cabal run spinor -- --version` でバージョンが表示されること。
- 全てのドキュメントのリンクが（相対パスとして）正しいこと。

---
## 実装報告

### 実装方針
- 依存ライブラリを追加せず、シンプルなパターンマッチで CLI オプションを実装
- ドキュメントは日英混在で、技術用語は英語、説明は日本語を基本とする
- README.md は仕様書のキャッチコピー "Static Lisp with Haskell Semantics" を採用

### 実装内容

#### 1. README.md のリファクタリング
- Features セクションに TCO、WASM、SLY 連携を追加
- Quick Start セクションを拡充（REPL 起動 + ネイティブビルド）
- Commands セクションを表形式で追加
- Documentation セクションに reference.md と emacs.md へのリンクを追加
- Folder Structure を最新の構成（Compiler/, Server.hs, runtime/, editors/）に更新
- Cコンパイラ（GCC等）が必須であることを明記

#### 2. docs/reference.md の作成
- Special Forms: defun, fn, if, let, match, data, mac, quote
- Primitive Functions: 算術演算、比較演算、リスト操作、入出力
- Twister Library: list, core, math の主要関数
- Types: 基本型、型構築子、型推論の説明

#### 3. docs/emacs.md の作成
- spinor-mode.el のインストール方法とキーバインド
- カスタマイズ変数（spinor-program, spinor-program-args）
- SLY 連携のセットアップ手順
- ワークフロー例とトラブルシューティング

#### 4. CLI Polish (app/Main.hs)
- `--help` / `-h`: サブコマンド一覧と使用例を含むヘルプメッセージを表示
- `--version` / `-v`: "spinor 0.1.0.0" 形式でバージョン情報を表示
- 不正な引数の場合もヘルプメッセージを表示するように変更

#### 変更ファイル一覧
- `README.md` — 全面改訂
- `docs/reference.md` — 新規作成
- `docs/emacs.md` — 新規作成
- `app/Main.hs` — helpMessage, version 追加、main 関数修正
