# Spec 50: Windows Native Build Foundation

## 概要
Windows (MSYS2/MinGW64) 環境における C ライブラリ依存関係 (`network`, `hmatrix`) のビルド問題を解決し、ローカル開発環境および GitHub Actions CI でのビルド安定性を確保する。

## 解決すべき課題
1. **network パッケージ:** `HsNetworkConfig.h` の生成に `autoconf` や `pkgconf` 等のツールが必要だが、標準の Windows 環境には存在しない。
2. **hmatrix パッケージ:** BLAS/LAPACK のバイナリが必要。MSYS2 の `openblas` を利用するようにフラグを設定する必要がある。

## アーキテクチャ

### 1. プロジェクト設定
プロジェクトルートに `cabal.project.local` を導入し、OS 固有のフラグを管理する。
特に Windows では `hmatrix` に対し `+openblas` フラグを強制する。

### 2. CI (GitHub Actions)
`windows-latest` ランナーにおいて、標準の PowerShell 環境ではなく MSYS2 環境をセットアップし、必要なツールとライブラリを `pacman` 経由でインストールする。

## 設定ファイル仕様

### cabal.project.local
```cabal
-- Windows 専用設定
package hmatrix
  flags: +openblas

-- 必要に応じてインクルードパスやライブラリパスを追加
```

## CI ワークフローの改修
`.github/workflows/release.yml` (および他のビルド用 YAML) の `jobs.build.strategy.matrix` に `windows-latest` が含まれている場合、以下のステップを挿入する。

1. **MSYS2 Setup:** `msys2/setup-msys2@v2` を使用。
2. **Dependency Install:** `pacman -S --noconfirm mingw-w64-x86_64-openblas mingw-w64-x86_64-pkg-config base-devel autoconf automake`

## 考慮事項
- **パスの解決:** `cabal` が MSYS2 側の `pkg-config` やライブラリを正しく見つけられるよう、`setup-msys2` の `path-type: inherit` や `msystem: MINGW64` 設定を適切に行う。
