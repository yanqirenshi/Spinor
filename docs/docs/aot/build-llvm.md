# build-llvm コマンド (LLVM IR 経由)

LLVM IR を生成し、Clang でネイティブバイナリを生成します。
より高度な最適化が適用されます。

## 使用方法

```bash
# LLVM IR 経由でコンパイル
cabal run spinor -- build-llvm fib.spin

# 生成された実行ファイルを実行
./fib
```

## 前提条件

システムに LLVM (Clang) がインストールされている必要があります。

---

## Windows での LLVM インストール

```powershell
# winget を使用してインストール (推奨)
winget install LLVM.LLVM

# インストール後、新しいターミナルを開いて確認
clang --version
```

または [LLVM 公式サイト](https://releases.llvm.org/) から Windows インストーラをダウンロードしてください。

---

## Linux / WSL2 での LLVM インストール

```bash
# Ubuntu / Debian
sudo apt update
sudo apt install clang

# インストール確認
clang --version
```

---

## macOS での LLVM インストール

```bash
# Xcode Command Line Tools (Clang を含む)
xcode-select --install

# または Homebrew で最新版をインストール
brew install llvm
```
