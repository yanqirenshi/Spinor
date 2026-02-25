# Build Guide

Spinor のビルドには Haskell ツールチェーンに加え、複数の C 言語ライブラリが必要です。
本ガイドでは、主要な OS 環境ごとの詳細なセットアップ手順を解説します。

## 共通の前提条件

### Haskell ツールチェーン

[GHCup](https://www.haskell.org/ghcup/) を使用したインストールを推奨します。

```bash
# GHCup のインストール (Linux/macOS/WSL2)
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Windows (PowerShell)
Set-ExecutionPolicy Bypass -Scope Process -Force;
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```

必要なバージョン:

| ツール | 最小バージョン | 推奨バージョン |
|:-------|:---------------|:---------------|
| GHC | 9.6.0 | 9.6.7 |
| Cabal | 3.10.0 | 3.10.3 |
| Git | 2.0 | 最新版 |

バージョン確認:

```bash
ghc --version
cabal --version
git --version
```

---

## Linux (Ubuntu / Debian / WSL2)

### システムパッケージのインストール

```bash
# パッケージリストの更新
sudo apt-get update

# ビルドツール
sudo apt-get install -y build-essential pkg-config

# hmatrix (線形代数ライブラリ) の依存関係
sudo apt-get install -y libopenblas-dev liblapack-dev libnuma-dev

# OpenGL / GLFW の依存関係
sudo apt-get install -y libglfw3-dev libgl1-mesa-dev

# OpenCL の依存関係
sudo apt-get install -y pocl-opencl-icd opencl-headers ocl-icd-opencl-dev
```

### Spinor のビルド

```bash
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build

# テスト
cabal test

# REPL の起動
cabal run spinor
```

---

## Windows 11 (MSYS2 / MinGW64)

Windows では **MSYS2 (MinGW64)** 環境を強く推奨します。

### Step 1: MSYS2 のインストール

1. [MSYS2 公式サイト](https://www.msys2.org/) から最新のインストーラをダウンロード
2. `C:\msys64` にインストール (デフォルト推奨)
3. インストール完了後、**MSYS2 MinGW64** ターミナルを起動

### Step 2: パッケージのインストール

MSYS2 MinGW64 ターミナルで以下を実行:

```bash
# パッケージデータベースの更新
pacman -Syu

# ターミナルが閉じた場合は再度 MSYS2 MinGW64 を起動して続行
pacman -Su

# ビルドツール
pacman -S --noconfirm base-devel autoconf automake make pkgconf

# MinGW64 ツールチェーン
pacman -S --noconfirm mingw-w64-x86_64-toolchain mingw-w64-x86_64-pkg-config

# hmatrix (線形代数ライブラリ) の依存関係
pacman -S --noconfirm mingw-w64-x86_64-openblas

# OpenGL / GLFW の依存関係
pacman -S --noconfirm mingw-w64-x86_64-glfw

# OpenCL の依存関係
pacman -S --noconfirm mingw-w64-x86_64-opencl-icd mingw-w64-x86_64-opencl-headers
```

### Step 3: 環境変数の設定

MSYS2 のバイナリを Windows から利用できるように PATH を設定します。

1. Windows 設定 → システム → システムの詳細設定 → 環境変数
2. ユーザー環境変数の `Path` に以下を追加:
   ```
   C:\msys64\mingw64\bin
   ```
3. **PowerShell を再起動**して反映を確認:
   ```powershell
   echo $env:PATH | Select-String "msys64"
   pkg-config --version
   ```

### Step 4: cabal.project.local の作成

プロジェクトルートに `cabal.project.local` を作成し、MSYS2 のライブラリパスを指定します:

```cabal
-- Windows (MSYS2/MinGW64) 環境用の設定

package hmatrix
  flags: +openblas
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include

package OpenCL
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include

package bindings-GLFW
  extra-lib-dirs: C:\msys64\mingw64\lib
  extra-include-dirs: C:\msys64\mingw64\include
```

### Step 5: ビルド

PowerShell で実行:

```powershell
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build
```

---

## トラブルシューティング

### DLL Not Found (Windows)

**症状:** `cabal test` や `cabal run` 実行時に以下のようなエラーが発生:
```
exit code: -1073741515 (0xC0000135)
```
または
```
libopenblas.dll が見つかりません
```

**原因:** Windows の cabal は PATH 環境変数を子プロセスに正しく渡さないことがあります。

**解決策:** 必要な DLL を実行ファイルと同じディレクトリにコピーします。

```powershell
# テスト実行ディレクトリ (GHC バージョンに合わせて調整)
$testDir = "dist-newstyle\build\x86_64-windows\ghc-9.6.7\spinor-0.1.0.0\t\spinor-test\build\spinor-test"

# DLL をコピー
Copy-Item C:\msys64\mingw64\bin\libopenblas.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgfortran-5.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libquadmath-0.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libgcc_s_seh-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\libwinpthread-1.dll $testDir
Copy-Item C:\msys64\mingw64\bin\OpenCL.dll $testDir
```

**注意:** `cabal clean` を実行すると DLL も削除されるため、再度コピーが必要です。

### OpenCL Platform Not Found

**症状:** `(cl-init)` を実行すると "No OpenCL platform found" エラー。

**原因:** OpenCL ICD (Installable Client Driver) が正しくインストールされていません。

**解決策:**

Linux:
```bash
# ICD Loader のインストール
sudo apt-get install -y ocl-icd-libopencl1

# 利用可能なプラットフォームの確認
clinfo
```

Windows:
```powershell
# MSYS2 で OpenCL ICD をインストール
pacman -S --noconfirm mingw-w64-x86_64-opencl-icd

# GPU ドライバの OpenCL サポートを確認
# - NVIDIA: CUDA Toolkit に含まれる
# - AMD: AMD Software に含まれる
# - Intel: Intel OpenCL Runtime
```

### hmatrix ビルドエラー (openblas not found)

**症状:**
```
Setup: Missing dependency on a foreign library: * Missing (or bad) C library: openblas
```

**解決策:**

1. OpenBLAS がインストールされているか確認:
   - Linux: `dpkg -l | grep openblas`
   - Windows: `pacman -Qs openblas`

2. `cabal.project.local` で正しいパスが設定されているか確認

3. Windows の場合、`flags: +openblas` が設定されているか確認

### pkg-config が見つからない

**症状:**
```
pkg-config: command not found
```

**解決策:**

Linux:
```bash
sudo apt-get install -y pkg-config
```

Windows (MSYS2):
```bash
pacman -S --noconfirm mingw-w64-x86_64-pkg-config
```

また、`C:\msys64\mingw64\bin` が PATH に含まれているか確認してください。

---

## 次のステップ

- [Introduction](introduction) - クイックスタートガイド
- [Syntax](syntax) - 言語構文の解説
- [API Reference](api-index) - 組み込み関数リファレンス
