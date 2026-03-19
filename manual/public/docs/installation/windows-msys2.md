# Windows 11 (MSYS2 / MinGW64)

行列演算、OpenGL、OpenCL などのフル機能を使用する場合は **MSYS2 (MinGW64)** 環境が必要です。

## Step 1: MSYS2 のインストール

1. [MSYS2 公式サイト](https://www.msys2.org/) から最新のインストーラをダウンロード
2. `C:\msys64` にインストール (デフォルト推奨)
3. インストール完了後、**MSYS2 MinGW64** ターミナルを起動

## Step 2: パッケージのインストール

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

## Step 3: 環境変数の設定

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

## Step 4: cabal.project.local の作成

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

## Step 5: ビルド

PowerShell で実行:

```powershell
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build
```
