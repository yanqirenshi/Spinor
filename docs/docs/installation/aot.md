# ネイティブコンパイル (AOT)

Spinor は `.spin` ファイルをネイティブ実行ファイルにコンパイルする AOT (Ahead-of-Time) コンパイル機能を提供しています。

## build コマンド (C 経由)

C 言語にトランスパイルし、GCC/Clang でネイティブバイナリを生成します。

```bash
# hello.spin をネイティブバイナリにコンパイル
cabal run spinor -- build hello.spin

# 生成された実行ファイルを実行
./hello        # Linux/macOS
./hello.exe    # Windows
```

**前提条件:** GCC または Clang がシステムにインストールされている必要があります。

## build-llvm コマンド (LLVM IR 経由)

LLVM IR を生成し、Clang でネイティブバイナリを生成します。
より高度な最適化が適用されます。

```bash
# LLVM IR 経由でコンパイル
cabal run spinor -- build-llvm fib.spin

# 生成された実行ファイルを実行
./fib
```

**前提条件:** システムに LLVM (Clang) がインストールされている必要があります。

### Windows での LLVM インストール

```powershell
# winget を使用してインストール (推奨)
winget install LLVM.LLVM

# インストール後、新しいターミナルを開いて確認
clang --version
```

または [LLVM 公式サイト](https://releases.llvm.org/) から Windows インストーラをダウンロードしてください。

### Linux / WSL2 での LLVM インストール

```bash
# Ubuntu / Debian
sudo apt update
sudo apt install clang

# インストール確認
clang --version
```

### macOS での LLVM インストール

```bash
# Xcode Command Line Tools (Clang を含む)
xcode-select --install

# または Homebrew で最新版をインストール
brew install llvm
```

## WASM ビルド (Emscripten 経由)

WebAssembly にコンパイルしてブラウザで実行できます。

```bash
# Emscripten SDK のセットアップが必要
cabal run spinor -- build --wasm app.spin

# 生成されたファイル
# - app.html
# - app.js
# - app.wasm
```
