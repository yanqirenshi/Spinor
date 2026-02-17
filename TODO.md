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
- [ ] **Step 17: ユーザー定義データ型 (ADTs)**
    - [ ] 構文: `(data Maybe (Just a) (Nothing))`
    - [ ] カーネルでの型コンストラクタと型定義のサポート。
- [ ] **パターンマッチ**
    - [ ] 構文: `(match x ((Just v) v) (Nothing 0))`
    - [ ] 分解ロジックと網羅性チェック。
- [ ] **Concurrency & Parallelism (Haskell RTS Integration)**
    - [ ] **Green Threads:** `spawn` (forkIO), `sleep` (threadDelay) プリミティブの実装。
    - [ ] **Communication:** `MVar` (同期変数) のサポート (`new-mvar`, `take-mvar`, `put-mvar`)。
    - [ ] **STM (Software Transactional Memory):** `TVar` と `atomically` ブロックの実装 (ロックフリーな並行処理)。
- [ ] **モジュールシステム**
    - [ ] 名前空間と `import` / `export` の仕組み。

## 🤝 Common Lisp Alignment (CL互換レイヤー)
Common Lisp ライクな挙動を取り込み、実用性を高める（Kernel/Library 並行作業）。

### Spinor Kernel (Haskell側)
- [ ] **Control Flow Primitives**
    - [ ] `progn`: 式を順番に評価し、最後の値を返す。
    - [ ] `setq` (または `set!`): 既存変数の破壊的代入。
    - [ ] `error`: エラー送出プリミティブ。
- [ ] **Equality Predicates**
    - [ ] `eq`: ポインタ/アトムの同一性。
    - [ ] `equal`: 構造的な等価性。
- [ ] **Input / Output**
    - [ ] `princ`, `print`: 標準出力への表示。
    - [ ] `read`: 標準入力からのS式読み込み。

### Twister Library (Spinor側)
- [ ] **Definitions & Binding Macros**
    - [ ] `defun`: `(define name (fn args ...))` の構文糖衣。
    - [ ] `defvar` / `defparameter`: グローバル変数定義。
    - [ ] `let*`: 順次束縛マクロ。
- [ ] **Control Flow Macros**
    - [ ] `when`, `unless`: `if` の片側版。
    - [ ] `and`, `or`: 短絡評価マクロ。
    - [ ] `cond`: (CL準拠化)。
- [ ] **Iteration Macros**
    - [ ] `dolist`: リストに対する反復。
    - [ ] `dotimes`: 回数指定の反復。
- [ ] **List Utilities**
    - [ ] `list`: 可変長引数リスト作成。
    - [ ] `append`: リストの連結。
    - [ ] `member`: リストのメンバシップ判定。
    - [ ] `nth`: N番目の要素取得。

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

## 🚀 Performance & Compilation (パフォーマンスと移植性)
- [ ] **コンパイラ バックエンド**
    - [ ] インタプリタ実行ではなく、**C言語** や **WebAssembly (WASM)** へのトランスパイル。
    - [ ] スタンドアロンバイナリの生成。
- [ ] **最適化**
    - [ ] 末尾呼び出し最適化 (TCO) のカーネル内実装。

## 🔬 HPC & Science (科学技術計算と可視化)
データサイエンス、機械学習、可視化のための拡張。

- [ ] **Matrix Operations (BLAS/LAPACK Integration)**
    - [ ] `hmatrix` 等の Haskell ライブラリとのバインディング。
    - [ ] `Val` 型への `VMatrix` (行列型) の追加。
    - [ ] 線形代数プリミティブの実装 (`dot`, `inv`, `eig`, `svd`)。
- [ ] **GPGPU Support (OpenCL)**
    - [ ] OpenCL コンテキスト操作のプリミティブ化。
    - [ ] 行列演算等の GPU オフロード。
- [ ] **Visualization (OpenGL)**
    - [ ] ウィンドウ作成と描画ループの制御。
    - [ ] 基本的な 2D/3D 描画プリミティブ。
- [ ] **Machine Learning (OpenML Integration)**
    - [ ] BLAS/LAPACK を基盤とした機械学習アルゴリズムの実装 (Twister)。

## 🧪 Experimental Features (実験的機能)
Rust のような安全性とリソース管理を導入する実験場。

- [ ] **Linear Types / Ownership (所有権システム)**
    - [ ] `Type` 定義への Multiplicity (Many/One) の導入。
    - [ ] 線形型チェック (変数の使用回数カウント) の実装。
    - [ ] Twister での `unique` (所有権付き) 変数のサポート。
    - [ ] **Borrow Checker:** 参照 (`&`) とライフタイムの簡易実装。
- [ ] **Region-based Memory Management**
    - [ ] GC に頼らない、静的なメモリ解放の仕組み (Rust の Drop trait 相当)。

## 📦 Standard Library Expansion (標準ライブラリ拡充)
- [ ] **文字列と IO**
    - [ ] ネイティブ `String` 型のサポート。
    - [ ] 基本的な IO プリミティブ (`print`, `read-line`, `read-file`)。
- [ ] **テストフレームワーク:** `(test ...)` マクロの実装。
- [ ] **パーサーコンビネータ:** Spinor 上での Parsec 風ライブラリ。
- [ ] **JSON Support:** JSON パーサー/シリアライザ。
