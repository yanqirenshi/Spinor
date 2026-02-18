#!/bin/sh
# Spinor Browser REPL - WASM ビルドスクリプト
#
# Usage: ./scripts/build-wasm-repl.sh
# Output: web/spinor-repl.js, web/spinor-repl.wasm
#
# 前提: emcc (Emscripten) がインストール済みであること。

set -e

echo "Building Spinor Browser REPL (WASM)..."

emcc runtime/repl.c runtime/spinor.c \
  -Iruntime \
  -o web/spinor-repl.js \
  -s EXPORTED_FUNCTIONS="['_sp_eval_string','_main']" \
  -s EXPORTED_RUNTIME_METHODS="['ccall','cwrap']" \
  -s ALLOW_MEMORY_GROWTH=1 \
  -s MODULARIZE=0 \
  -O2

echo "Build successful!"
echo "  web/spinor-repl.js"
echo "  web/spinor-repl.wasm"
echo ""
echo "To run:"
echo "  cd web && python3 -m http.server 8000"
echo "  Open http://localhost:8000"
