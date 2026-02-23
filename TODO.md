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
- [x] **Step 23: リスト操作ユーティリティ (List Utilities)** (In Progress)
    - [x] `list`: 可変長引数リスト作成。
    - [x] `append`: リストの連結 (多引数対応)。
    - [x] `member`: リストのメンバシップ判定。
    - [x] `nth`: N番目の要素取得。
    - [x] `reverse`: リストの反転。
    - [x] `length`: リストの長さ。
- [x] **Step 24: その他のCL互換機能**
    - [x] `defvar` / `defparameter`: グローバル変数定義。
    - [x] `let*`: 順次束縛マクロ。
    - [x] `dolist`, `dotimes`: 反復マクロ。
    - [x] `progn`: (begin のエイリアスまたはマクロとして整備)。
    - [x] `error`: エラー送出プリミティブ。

## 📚 DevExp & Documentation (普及のための環境整備)
ユーザーが迷わず使えるためのドキュメントとツール群。

- [ ] **Reference Manual (CLHS style)**
    - [ ] 全プリミティブ・標準ライブラリ関数を網羅したリファレンスの作成。
    - [ ] 型シグネチャ、使用例、エッジケースの挙動を記載。
    - [ ] Web上で閲覧可能な形式 (HTML/Markdown) への出力。
- [ ] **エラーメッセージの改善**
    - [ ] ソース位置情報の追跡 (ファイル名, 行, 列)。
    - [ ] 初学者に優しい型エラーメッセージ。
- [x] **Step 29: SLY / SLIME Support (TCP Socket Integration)**
    - [x] **Dependency:** `package.yaml` に `network` パッケージを追加。
    - [x] **Socket Server:** 指定ポートで待ち受け、接続を受け入れる `spinor server` コマンドの実装。
    - [x] **Message Loop:** 受信した文字列を S式 としてパースし、評価結果を返す簡易ループの実装。
- [x] **Step 30: SLY Integration (Swank Protocol Basics)**
    - [x] **Message Framing:** Swank プロトコル仕様のパケット（長さヘッダ + ペイロード）の送受信実装。
    - [x] **RPC Dispatcher:** `(:emacs-rex ...)` 形式のリクエストを解析するディスパッチャの実装。
    - [x] **Handshake:** `M-x sly-connect` 接続時の初期化シーケンス (`connection-info`) への応答。
- [x] **Step 31: SLY Advanced (Interactive Evaluation)**
    - [x] **Interactive Eval:** `swank:interactive-eval` (`C-x C-e` 等) の実装と結果返却。
    - [x] **Compilation:** `swank:compile-string-for-emacs` (`C-c C-c`) の実装。
    - [ ] **Output Redirection:** 標準出力 (`print` 等) を Emacs の REPL バッファに転送する仕組み。
- [x] **Step 37: LSP Server Foundation (Language Server Protocol)**
    - [x] **Setup:** `lsp` パッケージの導入と、`spinor lsp` コマンドの作成。
    - [x] **Lifecycle:** `initialize`, `shutdown` 等の基本ハンドシェイクの実装。
    - [x] **Diagnostics:** パースエラー発生時にエディタ上に赤波線を表示する機能の実装。
- [x] **Step 38: LSP Advanced Features (Hover & Completion)**
    - [x] **Hover:** カーソル下のシンボルの情報（ドキュメント等）を表示 (textDocument/hover)。
    - [x] **Completion:** 組み込み関数の入力補完 (textDocument/completion)。
- [x] **Step 39: Reference Generator (docgen)**
    - [x] **SSoT:** `Docs.hs` のデータを唯一の正解 (Single Source of Truth) とする。
    - [x] **Generator:** `spinor docgen` コマンドによる Markdown 自動生成の実装。
    - [x] **Indexing:** `docs/reference.md` の自動更新と個別ファイルへのリンク生成。

## 📢 Promotion & Website (広報・Web)
Spinor / Twister の魅力を伝えるための Web プレゼンス。

- [x] **Landing Page Creation**
    - [x] プロジェクトのコンセプト ("Static Lisp with Haskell Semantics") を伝えるトップページ。
    - [x] インストール手順、クイックスタートガイド。
    - [x] シンプルでモダンなデザイン (HTML/CSS)。
- [ ] **GitHub Pages Deployment**
    - [x] `docs/` フォルダへのデプロイ設定。
    - [ ] ユーザーマニュアルとの統合。
- [x] **Step 32: Browser REPL UI (WASM Integration)**
    - [x] **xterm.js Integration:** WASM 版 Spinor を xterm.js と接続し、ブラウザ上でターミナル動作を実現。
    - [x] **Repl Loop:** 入力を受け取り、WASM で評価し、結果を表示するループの構築。
    - [x] **Github Pages:** 動作する REPL をランディングページに埋め込みデプロイ。
- [x] **Step 33: Documentation & Polish**
    - [x] **README Refactoring:** 最新機能 (TCO, WASM, SLY) を反映し全面刷新。
    - [x] **Reference:** `docs/reference.md` の作成。
    - [x] **Emacs Guide:** `docs/emacs.md` の作成。
    - [x] **CLI Polish:** `spinor --help`, `--version` の改善。
    - [x] **Doc Viewer:** `docs/doc.html` による Markdown の動的レンダリング (GitHub Pages 対応)。
    - [x] **README Refactoring:** ...
    - [x] **Doc Viewer:** ...
    - [x] **UI Overhaul:** `github-markdown-css` と `highlight.js` によるデザイン刷新 (Step 33-C)。
- [x] **Step 33-D: Add Project Logo**
    - [x] 作成した `spinor.png` をリポジトリ（例: `assets/` や `docs/assets/`）に配置。
    - [x] `README.md` のトップにロゴ画像を追加。
    - [x] `docs/index.html` および `docs/doc.html` のヘッダー等にロゴを配置してブランディングを強化。

## 🚀 Performance & Compilation (パフォーマンスと移植性)

インタプリタ依存からの脱却と、Web (WASM) 動作の実現。

- [x] **Step 25: トランスパイラ基盤 (C Code Generation)**
    - [x] **AST to C:** 基本的なデータ型 (Int, Bool) と算術演算の C コード生成。
    - [x] **Runtime Integration:** C言語用の最小限のランタイム（メモリ管理/GC戦略）の策定と統合。
    - [x] **Control Flow:** `if`, `defun`, 関数呼び出しの C 言語へのマッピング。
- [x] **Step 26: スタンドアロン コンパイル (Native Binary)**
    - [x] `spinor build` コマンドの実装 (トランスパイル後の C コードを gcc/clang でビルド)。
    - [x] Hello World のバイナリ生成と実行確認。
- [x] **Step 27: 最適化 (Optimization)**
    - [x] **TCO (Tail Call Optimization):** 末尾再帰を C の `goto` ループまたはトランポリンコードに展開。
- [x] **Step 28: WebAssembly (WASM) 対応**
    - [x] Emscripten を用いた C 出力からの WASM ビルドフローの確立。
    - [x] ブラウザ上での REPL 動作 (LPへの埋め込み)。
- [ ] **Step 36: LLVM Backend Investigation (GCC依存からの脱却)**
    - [ ] **Research:** `llvm-hs` ライブラリを用いた LLVM IR 生成の調査。
    - [ ] **Prototype:** 小さな計算式 (`+`, `*`) を LLVM JIT でメモリ内コンパイル・実行するプロトタイプの作成。
    - [ ] **Comparison:** 現行の C言語トランスパイル方式とのパフォーマンス・ビルド時間の比較。

## 📦 Standard Library Expansion (標準ライブラリ拡充)
- [x] **Step 35: Standard Library Expansion (String & I/O)**
    - [x] **String Operations:** `string-append`, `string-length`, `substring`, `string=?` の実装。
    - [x] **File I/O:** `read-file`, `write-file`, `append-file` の実装。
    - [x] **System:** `command-line-args` の実装。
    - [x] **Dual Implementation:** インタプリタ (Haskell) とコンパイラ (C Runtime) の両対応。
- [ ] **テストフレームワーク**
- [ ] **JSON Support**

### 🔬 HPC & Science (科学技術計算と可視化)

Spinor の Dual Implementation を維持しつつ、科学技術計算の強力なバックエンド（BLAS/OpenCL/OpenGL）を統合する。

- [x] **Step 44: Matrix/Tensor Type Foundation (行列・テンソル型の基盤)**
    - [x] 行列や多次元配列を表す新しい内部データ型 (`Tensor` / `Matrix`) の定義。
    - [x] Lisp構文での行列リテラル表現の設計 (例: `#m((1 2) (3 4))` や `(matrix 2 2 '(1 2 3 4))`)。
    - [x] 行列の生成、要素アクセス (`mref`)、次元取得 (`mdim`) などの基本プリミティブの実装。
- [x] **Step 45: BLAS/LAPACK Integration (行列演算の高速化)**
    - [x] **Interpreter:** `hmatrix` ライブラリを用いた BLAS/LAPACK バインディングの実装。
    - [x] **Primitives:** `m+`, `m*`, `transpose`, `inverse` の追加。
    - [x] **Compiler:** (将来タスク) C トランスパイル時に OpenBLAS などの外部ライブラリをリンクするビルドオプション (`spinor build --blas`) の追加。
- [ ] **Step 46: OpenCL Foundation (GPGPU 基盤とカーネル構文)**
    - [ ] **Data Model:** `Val` への `VCLContext`, `VCLBuffer`, `VCLKernel` の追加。
    - [ ] **Primitives:** `cl-init`, `to-device`, `to-host`, `cl-compile` の実装。
    - [ ] **Interpreter:** `OpenCL` パッケージによるプラットフォーム統合。
- [ ] **Step 47: OpenCL Execution Pipeline (GPGPU 実行レイヤー)**
    - [ ] 定義したカーネルを GPU 側のワークグループにディスパッチして並列実行する関数 (`spawn-cl` 等) の実装。
    - [ ] **Compiler:** C トランスパイル時に、OpenCL の C API (`clEnqueueNDRangeKernel` 等) を呼び出してカーネルを起動する C コードの生成。
- [ ] **Step 48: OpenGL Visualization (ネイティブ可視化基盤)**
    - [ ] OpenGL と GLFW (または SDL2) を用いた、ウィンドウ生成とメイン描画ループの組み込み。
    - [ ] ポリゴン描画、シェーダーのコンパイル、バッファ操作を行うためのグラフィックス用プリミティブ関数の提供。
    - [ ] **Interpreter:** Haskell の `gl`, `GLFW-b` 等を利用したインタラクティブな描画。
    - [ ] **Compiler:** ネイティブビルド時の OpenGL/GLFW ライブラリリンク対応。
- [ ] **Step 49: WebGL Target for WASM (ブラウザでの可視化)**
    - [ ] Emscripten が提供する OpenGL ES / WebGL のエミュレーション層を活用した WASM ビルド設定の拡張。
    - [ ] Spinor で書いた物理シミュレーション等の可視化コードが、変更なしでブラウザの Canvas 上 (WASM) で動く仕組みの確立。

## 🧪 Experimental Features (実験的機能)
- [ ] **Linear Types / Ownership (所有権システム)**
- [ ] **Region-based Memory Management**

## 📦 Distribution & CI (配布・自動化)
- [x] **Step 34: Binary Distribution (GitHub Actions)**
    - [x] **CI Pipeline:** Push 時に Linux / Windows / macOS でビルドとテストを実行するワークフローの作成。
    - [x] **Release Automation:** タグ (`v0.1.0` 等) を打った際に、自動で `spinor-windows.exe`, `spinor-linux` 等をビルドし、GitHub Releases にアップロードする設定。
- [ ] **Windows Native Build Foundation (MSYS2 & C Libraries)**
    - [ ] MSYS2 (MinGW64) 経由での C/C++ ビルドツール (`base-devel`, `autoconf`, `make`, `pkgconf` 等) の導入と `network` パッケージのビルドエラー (`HsNetworkConfig.h`) 解消。
    - [ ] MSYS2 経由での OpenBLAS (`mingw-w64-x86_64-openblas`) のインストール。
    - [ ] `cabal.project.local` の作成と、`hmatrix` に対する `+openblas` フラグの設定。
    - [ ] Windowsローカル環境での `cabal build` および `cabal test` の成功確認。
    - [ ] CI (GitHub Actions) の Windows ランナーに MSYS2 と OpenBLAS のセットアップステップを組み込む。


## 📖 Reference Manual (React CLHS-style Docs)
React を用いて、CLHS ライクでモダンなリファレンスサイトを構築し、GitHub Actions で自動ビルド・デプロイする。

- [x] **Step 40: React Manual Site Setup**
    - [x] `manual/` ディレクトリに Vite + React (TS) の基盤を作成。
    - [x] 左サイドバー（ナビゲーション）と右メインコンテンツのレイアウト実装。
    - [x] `react-router-dom` によるルーティングの導入。
    - [x] Markdown を動的にフェッチしてレンダリングするコンポーネントの実装 (`react-markdown`)。
- [x] **Step 41: Content Architecture & DocGen Integration**
    - [x] Introduction, Syntax, Types, Control Flow などの静的ページ (MD) の枠組みを作成。
    - [x] `spinor docgen` の出力先を `manual/public/ref/` 等に連携させ、React アプリから動的に読み込める仕組みの構築。
    - [x] Master Index (A-Z, Category) の一覧表示画面の実装。
    - [x] LP (ランディングページ) と WASM REPL を新しいマニュアルサイトに統合。
- [x] **Step 42: GitHub Actions Integration**
    - [x] Push 時に `manual/` をビルドし、成果物をリポジトリの `docs/` ディレクトリに配置・コミットする CI ワークフローの作成。
- [ ] **Step 43: CLHS Format Documentation & Core Pages**
    - [ ] `Docs.hs` のデータモデルを拡張し、CLHS と同等のセクション (Syntax, Arguments, Description, Examples 等) を追加。
    - [ ] `DocGen.hs` のジェネレータを改修し、各 Operator のページを CLHS フォーマットで出力。
    - [ ] `docs/syntax.md` に Lisp 構文と Haskell セマンティクスの解説を執筆・反映。
    - [ ] `docs/introduction.md` にインストール、セットアップ、利用方法のガイドを執筆・反映。

