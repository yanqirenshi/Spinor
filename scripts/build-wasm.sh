#!/bin/sh
set -e # エラーが発生したらスクリプトを終了

INPUT_FILE=$1

if [ -z "$INPUT_FILE" ]; then
  echo "Usage: $0 <input.spin>"
  exit 1
fi

echo "Step 1: Transpiling $INPUT_FILE to C..."
cabal run spinor -- compile "$INPUT_FILE"

echo "Step 2: Compiling C to WebAssembly with emcc..."
emcc output.c runtime/spinor.c -Iruntime -o index.html -O2

echo "Build successful! Created index.html, index.js, and index.wasm."
echo "Run a local web server and open index.html to see the result."
