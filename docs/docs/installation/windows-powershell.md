# Windows 11 (PowerShell)

Windows 11 では、基本的な Spinor のビルドは PowerShell のみで可能です。

## 最小構成でのビルド

数値演算ライブラリ (hmatrix) や GPU 機能を使用しない最小構成の場合:

```powershell
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build

# REPL の起動
cabal run spinor
```

## フル機能を使用する場合

行列演算 (hmatrix/BLAS)、OpenGL、OpenCL を使用する場合は、[Windows (MSYS2)](windows-msys2) の手順に従ってください。
