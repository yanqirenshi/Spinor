# Spinor Project Roadmap

並行して進められる開発領域ごとのタスクリスト。

## 🔧 Editor & Environment (開発環境)
まずは Emacs での最低限の開発環境を整える。

- [x] **Step 15: Emacs Integration (spinor-mode.el)**
    - [x] `.spin` ファイルのシンタックスハイライト。
    - [x] `inferior-spinor-mode` (REPL 連携 / comint)。
    - [x] 基本的な式評価 (`C-x C-e`, `C-c C-k`)。

## 🧬 Language Core (言語仕様と信頼性)
言語としての基礎体力を高め、品質を保証する。

- [x] **Step 16: ユニットテストの整備 (Kernel & Library)**
    - [x] **Kernel Test (Haskell):** Hspec を導入し、Parser, Eval 等の単体テストを作成。
    - [x] **Batch Execution:** スクリプトを一括評価して終了するバッチ実行モードの追加。
    - [x] **Library Test (Spinor):** セルフホストされたテストフレームワーク (`assert-equal`) と Twister ライブラリのテスト作成。
- [x] **Step 17: ユーザー定義データ型 (ADTs)**
    - [x] 構文: `(data Maybe (Just a) (Nothing))`
    - [x] カーネルでの型コンストラクタと型定義のサポート。
- [x] **Step 18: パターンマッチ**
    - [x] 構文: `(match x ((Just v) v) (Nothing 0))`
    - [x] 分解ロジックと網羅性チェック。
- [x] **Step 19: モジュールシステム**
    - [x] 名前空間と `import` / `export` の仕組み。
- [x] **Step 20: 並行処理 (Concurrency) - Haskell RTS Integration**
    - [x] **Green Threads:** `spawn` (forkIO), `sleep` (threadDelay) プリミティブの実装。
    - [x] **Communication:** `MVar` (同期変数) のサポート (`new-mvar`, `take-mvar`, `put-mvar`)。

## 🤝 Common Lisp Alignment (CL互換レイヤー)
Common Lisp ライクな挙動を取り込み、実用性を高める（Kernel/Library 並行作業）。

- [x] **Step 21: 基本機能の拡充 (Basics)**
    - [x] `let` の拡張: 複数変数の並列束縛 `(let ((x 1) (y 2)) ...)` への対応。
    - [x] `setq`: 既存変数の破壊的代入。
    - [x] `eq` / `equal`: ポインタ等価と構造等価の実装。
- [x] **Step 22: 制御構文マクロ (Control Flow)**
    - [x] `defun`: 関数定義マクロ。
    - [x] `when`, `unless`: 条件分岐マクロ。
    - [x] `cond`: 多分岐マクロ。
    - [x] `and`, `or`: 短絡評価マクロ。
- [ ] **Step 23: リスト操作ユーティリティ (List Utilities)** (In Progress)
    - [ ] `list`: 可変長引数リスト作成。
    - [ ] `append`: リストの連結 (多引数対応)。
    - [ ] `member`: リストのメンバシップ判定。
    - [ ] `nth`: N番目の要素取得。
    - [ ] `reverse`: リストの反転。
    - [ ] `length`: リストの長さ。
- [ ] **Step 24: その他のCL互換機能**
    - [ ] `defvar` / `defparameter`: グローバル変数定義。
    - [ ] `let*`: 順次束縛マクロ。
    - [ ] `dolist`, `dotimes`: 反復マクロ。
    - [ ] `progn`: (begin のエイリアスまたはマクロとして整備)。
    - [ ] `error`: エラー送出プリミティブ。

## 📚 DevExp & Documentation (普及のための環境整備)
ユーザーが迷わず使えるためのドキュメントとツール群。

- [ ] **Reference Manual (CLHS style)**
    - [ ] 全プリミティブ・標準ライブラリ関数を網羅したリファレンスの作成。
    - [ ] 型シグネチャ、使用例、エッジケースの挙動を記載。
    - [ ] Web上で閲覧可能な形式 (HTML/Markdown) への出力。
- [ ] **エラーメッセージの改善**
    - [ ] ソース位置情報の追跡 (ファイル名, 行, 列)。
    - [ ] 初学者に優しい型エラーメッセージ。
- [ ] **LSP Server (Language Server Protocol)**
    - [ ] `spinor-lsp` の実装 (ホバー、定義ジャンプ)。
- [ ] **SLY / SLIME Support (Emacsユーザーの最終目標)**
    - [ ] カーネルへの TCP ソケット通信の実装。
    - [ ] Swank Protocol (Slynk) のサブセット実装。

## 📢 Promotion & Website (広報・Web)
Spinor / Twister の魅力を伝えるための Web プレゼンス。

- [ ] **Landing Page Creation**
    - [ ] プロジェクトのコンセプト ("Static Lisp with Haskell Semantics") を伝えるトップページ。
    - [ ] インストール手順、クイックスタートガイド。
    - [ ] シンプルでモダンなデザイン (HTML/CSS)。
- [ ] **GitHub Pages Deployment**
    - [ ] `docs/` フォルダへのデプロイ設定。
    - [ ] ユーザーマニュアルとの統合。

## 🚀 Performance & Compilation (パフォーマンスと移植性)
- [ ] **コンパイラ バックエンド**
    - [ ] インタプリタ実行ではなく、**C言語** や **WebAssembly (WASM)** へのトランスパイル。
    - [ ] スタンドアロンバイナリの生成。
- [ ] **最適化**
    - [ ] 末尾呼び出し最適化 (TCO) のカーネル内実装。

## 🔬 HPC & Science (科学技術計算と可視化)
- [ ] **Matrix Operations (BLAS/LAPACK Integration)**
- [ ] **GPGPU Support (OpenCL)**
- [ ] **Visualization (OpenGL)**

## 🧪 Experimental Features (実験的機能)
- [ ] **Linear Types / Ownership (所有権システム)**
- [ ] **Region-based Memory Management**

## 📦 Standard Library Expansion (標準ライブラリ拡充)
- [ ] **文字列と IO** (ネイティブString, ファイルIO)
- [ ] **テストフレームワーク**
- [ ] **JSON Support**
