# Windows 11 (MSYS2 / UCRT64)

行列演算、OpenGL、OpenCL などのフル機能を使用する場合は **MSYS2 (UCRT64)** 環境が必要です。

> **Note:** UCRT64 は Windows 10/11 標準の Universal C Runtime (UCRT) を使用するモダンな環境です。
> 従来の MinGW64 (MSVCRT) よりも互換性が高く、推奨されます。

## Step 1: MSYS2 の確認

[共通の前提条件](prerequisites) で GHCup をインストールした際に、MSYS2 も一緒にインストールされています。

デフォルトのインストール先:
```
C:\ghcup\msys64
```

> **Note:** GHCup を使わず MSYS2 を単独でインストールした場合は `C:\msys64` になります。
> 以下の手順では GHCup 経由のパス (`C:\ghcup\msys64`) を使用します。

## Step 2: パッケージのインストール

スタートメニューから **MSYS2 UCRT64** ターミナルを起動し、以下を実行:

```bash
# パッケージデータベースの更新
pacman -Syu

# ターミナルが閉じた場合は再度 MSYS2 UCRT64 を起動して続行
pacman -Su

# ビルドツール
pacman -S --noconfirm base-devel autoconf automake make pkgconf

# UCRT64 ツールチェーン
pacman -S --noconfirm mingw-w64-ucrt-x86_64-toolchain mingw-w64-ucrt-x86_64-pkg-config

# hmatrix (線形代数ライブラリ) の依存関係
pacman -S --noconfirm mingw-w64-ucrt-x86_64-openblas

# OpenGL / GLFW の依存関係
pacman -S --noconfirm mingw-w64-ucrt-x86_64-glfw

# OpenCL の依存関係
pacman -S --noconfirm mingw-w64-ucrt-x86_64-opencl-icd mingw-w64-ucrt-x86_64-opencl-headers
```

## Step 3: 環境変数の設定

MSYS2 UCRT64 のバイナリを Windows から利用できるように PATH を設定します。

1. Windows 設定 → システム → システムの詳細設定 → 環境変数
2. ユーザー環境変数の `Path` に以下を追加:
   ```
   C:\ghcup\msys64\ucrt64\bin
   ```
3. **PowerShell を再起動**して反映を確認:
   ```powershell
   echo $env:PATH | Select-String "ucrt64"
   pkg-config --version
   ```

## Step 4: cabal.project.local の作成

GHCup の Cabal はデフォルトで古い MinGW64 環境を参照するため、UCRT64 を明示的に指定する必要があります。

プロジェクトルートに `cabal.project.local` を作成:

```cabal
-- Windows (MSYS2/UCRT64) 環境用の設定
-- GHCup のデフォルト (MinGW64) をオーバーライド

package hmatrix
  flags: +openblas
  extra-lib-dirs: C:\ghcup\msys64\ucrt64\lib
  extra-include-dirs: C:\ghcup\msys64\ucrt64\include

package OpenCL
  extra-lib-dirs: C:\ghcup\msys64\ucrt64\lib
  extra-include-dirs: C:\ghcup\msys64\ucrt64\include

package bindings-GLFW
  extra-lib-dirs: C:\ghcup\msys64\ucrt64\lib
  extra-include-dirs: C:\ghcup\msys64\ucrt64\include
```

> **重要:** このファイルがないと、Cabal が MinGW64 のライブラリを参照してしまい、
> ビルドエラーや実行時エラーの原因になります。

## Step 5: ビルド

PowerShell で実行:

```powershell
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build
```
