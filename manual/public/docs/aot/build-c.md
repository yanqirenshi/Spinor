# build コマンド (C 経由)

C 言語にトランスパイルし、GCC/Clang でネイティブバイナリを生成します。

## 使用方法

```bash
# hello.spin をネイティブバイナリにコンパイル
cabal run spinor -- build hello.spin

# 生成された実行ファイルを実行
./hello        # Linux/macOS
./hello.exe    # Windows
```

## 前提条件

GCC または Clang がシステムにインストールされている必要があります。

### Windows

```powershell
# MSYS2 UCRT64 環境で GCC をインストール
pacman -S mingw-w64-ucrt-x86_64-gcc
```

### Linux / WSL2

```bash
# Ubuntu / Debian
sudo apt update
sudo apt install build-essential
```

### macOS

```bash
# Xcode Command Line Tools
xcode-select --install
```
