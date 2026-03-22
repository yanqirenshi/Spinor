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

### Emscripten のインストール

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

### 動作確認

```bash
emcc --version
```
