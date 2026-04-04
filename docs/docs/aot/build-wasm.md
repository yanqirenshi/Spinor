# WASM ビルド (Emscripten 経由)

WebAssembly にコンパイルしてブラウザで実行できます。

## 使用方法

```bash
# Emscripten SDK のセットアップが必要
cabal run spinor -- build --wasm app.spin

# 生成されたファイル
# - app.html
# - app.js
# - app.wasm
```

## 前提条件

Emscripten SDK がインストールされている必要があります。

---

## Windows でのインストール

```powershell
# 1. emsdk のクローン
git clone https://github.com/emscripten-core/emsdk.git C:\emsdk
cd C:\emsdk

# 2. Python でインストール・有効化を実行
#    (emsdk.bat は Python が PATH に必要なため、直接 emsdk.py を呼び出す)
python emsdk.py install latest
python emsdk.py activate latest

# 3. 環境変数の設定 (新しいターミナルごとに実行)
C:\emsdk\emsdk_env.ps1
```

> **Windows の補足:** Spinor は emcc.bat を呼び出す際に `EMSDK_PYTHON` 環境変数を自動的に設定します。そのため `emsdk_env.ps1` を実行しなくても、emsdk が `C:\emsdk` にインストールされていれば動作します。

---

## Linux / WSL2 でのインストール

```bash
# emsdk のクローン
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk

# 最新版のインストールと有効化
./emsdk install latest
./emsdk activate latest

# 環境変数の設定
source ./emsdk_env.sh
```

---

## macOS でのインストール

```bash
# emsdk のクローン
git clone https://github.com/emscripten-core/emsdk.git
cd emsdk

# 最新版のインストールと有効化
./emsdk install latest
./emsdk activate latest

# 環境変数の設定
source ./emsdk_env.sh
```

---

## 動作確認

```bash
emcc --version
```
