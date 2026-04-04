# build-llvm コマンド (LLVM IR 経由)

LLVM IR を生成し、Clang でネイティブバイナリを生成します。
より高度な最適化が適用されます。

## 使用方法

```bash
# LLVM IR 経由でコンパイル
cabal run spinor -- build-llvm fib-aot.spin

# 生成された実行ファイルを実行
./fib-aot        # Linux / macOS
.\fib-aot.exe    # Windows
```

## 前提条件

- LLVM (Clang) がインストールされていること
- Windows の場合: MSYS2 (MinGW) がインストールされていること（GHCup で自動インストールされます）

> **Windows の補足:** Spinor は Windows 上で clang を呼び出す際に `--target=x86_64-pc-windows-gnu` フラグを自動的に付与します。これにより MSYS2 の MinGW ヘッダーが使用されるため、Visual Studio の Developer Command Prompt は不要です。

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
