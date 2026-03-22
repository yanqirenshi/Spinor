# ネイティブコンパイル (AOT)

Spinor は `.spin` ファイルをネイティブ実行ファイルにコンパイルする AOT (Ahead-of-Time) コンパイル機能を提供しています。

## コンパイル方法

| コマンド | 説明 |
|:---------|:-----|
| [build (C 経由)](build-c) | C 言語にトランスパイルし、GCC/Clang でネイティブバイナリを生成 |
| [build-llvm (LLVM IR)](build-llvm) | LLVM IR を生成し、高度な最適化を適用してネイティブバイナリを生成 |
| [WASM ビルド (Emscripten)](build-wasm) | WebAssembly にコンパイルしてブラウザで実行可能に |
