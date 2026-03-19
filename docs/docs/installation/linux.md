# Linux (Ubuntu / Debian / WSL2)

## システムパッケージのインストール

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

## Spinor のビルド

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
