# Spec 43b: Build & Environment Guide

## 概要
Spinor のビルドには Haskell ツールチェーンに加え、複数の C 言語ライブラリ（OpenBLAS, OpenCL, GLFW, OpenGL）のシステムへのインストールが必要です。本ドキュメントでは、主要な OS 環境ごとの詳細なセットアップ手順をユーザーに提供します。

## 対象ドキュメント
`manual/public/docs/build.md` を作成し、マニュアルサイトのナビゲーションに追加する。

## 構成案

### 1. 共通の前提条件
- **GHC 9.6.x 以上**: `ghcup` を推奨。
- **Cabal 3.10.x 以上**: `ghcup` を推奨。
- **Git**: ソースコードの取得用。

### 2. Linux (Ubuntu / WSL2)
必要なシステムパッケージのインストール手順。
- `sudo apt-get update`
- `build-essential`, `pkg-config`, `libnuma-dev`
- `libopenblas-dev`, `liblapack-dev` (hmatrix 用)
- `libglfw3-dev`, `libgl1-mesa-dev` (OpenGL 用)
- `pocl-opencl-icd`, `opencl-headers` (OpenCL 用)

### 3. Windows 11 (MSYS2 / MinGW64)
Windows では MSYS2 (MinGW64) 環境を強く推奨する。
- **MSYS2 の導入**: インストールと `pacman -Syu`。
- **パッケージインストール**:
  - `mingw-w64-x86_64-toolchain`, `mingw-w64-x86_64-pkg-config`
  - `mingw-w64-x86_64-openblas`
  - `mingw-w64-x86_64-glfw`
  - `mingw-w64-x86_64-opencl-icd`
- **環境変数の設定**: `C:\msys64\mingw64\bin` への PATH 通し。

### 4. Cabal の設定 (`cabal.project.local`)
Windows 環境で `hmatrix` をビルドするための必須設定を記載する。
```cabal
package hmatrix
  flags: +openblas
```

### 5. トラブルシューティング
- **DLL Not Found (Windows)**:
  - `cabal test` や `cabal run` 実行時に `libopenblas.dll` 等が見つからない問題の解決策。
- **OpenCL Platform Not Found**:
  - `(cl-init)` が失敗する場合の ICD Loader の確認方法。
