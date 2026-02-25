# Handover Note: Spinor Project (2026-02-23)

再起動後のセッション再開用。

## 📍 現在の状況
- **Role:** チーフアーキテクト (Architect)
- **フェーズ:** 大規模な機能拡張（HPC, 可視化, エラーレポート, JSON, ドキュメント刷新）の **設計およびタスク分解フェーズ**。
- **Git:** 全ての設計ドキュメント (specs/, tasks/) は `master` ブランチにコミット・プッシュ済み。

## 🛠 このセッションでの成果物 (作成済みの設計・指示書)

### 1. 開発基盤 & QoL
- **Step 50:** Windows Native Build Foundation (MSYS2 & C Libraries)
- **Step 59:** Error Reporting & Source Tracking (AST への位置情報埋め込み)
- **Step 60:** JSON Support (aeson 統合)

### 2. 数値計算 (HPC) & GPGPU
- **Step 44:** Matrix/Tensor Type Foundation (Storable Vector)
- **Step 45:** BLAS/LAPACK Integration (hmatrix)
- **Step 46:** OpenCL Foundation (GPGPU 基盤)
- **Step 47:** OpenCL Execution Pipeline (カーネル実行)

### 3. 可視化 (Visualization)
- **Step 48:** OpenGL Visualization (GLFW ネイティブ描画)
- **Step 49:** WebGL Target for WASM (Emscripten / SDL2)

### 4. ドキュメンテーション (Reference Manual)
- **Step 39:** Reference Generator (docgen 実装)
- **Step 43:** CLHS Format Documentation (フォーマット刷新)
- **Step 43-B:** Build & Environment Guide (環境構築手順)
- **Step 43-C:** Documentation Restructuring (階層化)
- **Step 43-G/H:** CLHS Chapters Expansion (章立ての拡充)

## 🚀 次のステップ
再起動後は、以下のいずれかのアプローチで進めることができます。

1. **実装の開始:** `tasks/` ディレクトリにある指示書を一つずつエンジニア (Claude Code 等) に渡し、実装フェーズに移行する。
2. **残りの設計:** `TODO.md` に残っている Step 51 以降（Lispマシンの調査、AI協調開発環境など）の設計を進める。

## 📝 備忘録
- `cabal.project.local` は Windows 環境で `hmatrix` をビルドするために必須。
- OpenCL は環境依存が強いため、実装後の手動検証が重要。
- エラーレポート (Step 59) は影響範囲が全コードに及ぶため、早期の着手が望ましい。
