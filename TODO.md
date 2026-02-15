# Spinor Project Roadmap
## Phase 1: Editor Integration (Basic) - Current Focus

まずは Emacs での最低限の開発環境を整える。

- [ ] **Step 15: Emacs Integration (spinor-mode.el)**
    - [ ] `.spin` ファイルのシンタックスハイライト。
    - [ ] `inferior-spinor-mode` (REPL 連携 / comint)。
    - [ ] 基本的な式評価 (`C-x C-e`, `C-c C-k`)。

## Phase 2: Reliability & Core Power-up (言語としての体力と信頼性)

機能拡充と並行して、言語実装自体の品質保証を行う。

- [ ] **ユニットテストの整備 (Kernel & Library)**
    - [ ] **Kernel Test (Haskell):** Hspec/Tasty を導入し、Parser, Eval, TypeCheck の単体テストを作成。
    - [ ] **Library Test (Spinor):** Twister ライブラリ (`list.spin` 等) の挙動を保証するテストスイートの作成。
- [ ] **Concurrency & Parallelism (Haskell RTS Integration)**
    - [ ] **Green Threads:** `spawn` (forkIO), `sleep` (threadDelay) プリミティブの実装。
    - [ ] **Communication:** `MVar` (同期変数) のサポート (`new-mvar`, `take-mvar`, `put-mvar`)。
    - [ ] **STM (Software Transactional Memory):** `TVar` と `atomically` ブロックの実装 (ロックフリーな並行処理)。
- [ ] **ユーザー定義データ型 (ADTs)**
    - [ ] 構文: `(data Maybe (Just a) (Nothing))`
    - [ ] カーネルでの型コンストラクタのサポート。
- [ ] **パターンマッチ**
    - [ ] 構文: `(match x ((Just v) v) (Nothing 0))`
    - [ ] 分解ロジックと網羅性チェック。
- [ ] **文字列と IO**
    - [ ] ネイティブ `String` 型のサポート。
    - [ ] 基本的な IO プリミティブ (`print`, `read-line`, `read-file`)。
- [ ] **モジュールシステム**
    - [ ] 名前空間と `import` / `export` の仕組み。

## Phase 3: Advanced DevExp & Documentation (普及のための環境整備)

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

## Phase 4: Performance & Compilation (パフォーマンスと移植性)

- [ ] **コンパイラ バックエンド**
    - [ ] インタプリタ実行ではなく、**C言語** や **WebAssembly (WASM)** へのトランスパイル。
    - [ ] スタンドアロンバイナリの生成。
- [ ] **最適化**
    - [ ] 末尾呼び出し最適化 (TCO) のカーネル内実装。

## Phase 5: HPC & Science (科学技術計算と可視化)

Spinor をデータサイエンス、機械学習、可視化のためのプラットフォームに進化させる。

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
    - [ ] データセットのロードや実験管理 (OpenML API 連携など)。

## Phase 6: Advanced Type System (Experimental)

Rust のような安全性とリソース管理を導入する実験的なフェーズ。

- [ ] **Linear Types / Ownership (所有権システム)**
    - [ ] `Type` 定義への Multiplicity (Many/One) の導入。
    - [ ] 線形型チェック (変数の使用回数カウント) の実装。
    - [ ] Twister での `unique` (所有権付き) 変数のサポート。
    - [ ] **Borrow Checker:** 参照 (`&`) とライフタイムの簡易実装。
- [ ] **Region-based Memory Management**
    - [ ] GC に頼らない、静的なメモリ解放の仕組み (Rust の Drop trait 相当)。

## Twister Library Expansion (標準ライブラリの拡充)

- [ ] **テストフレームワーク:** `(test ...)` マクロの実装。
- [ ] **パーサーコンビネータ:** Spinor 上での Parsec 風ライブラリ。
- [ ] **JSON Support:** JSON パーサー/シリアライザ。
