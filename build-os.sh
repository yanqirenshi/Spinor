#!/usr/bin/env bash
#
# build-os.sh — Spinor OS (Phase 1 PoC) のソース準備スクリプト
#
# Issue #52 / The Ultimate Dream の Phase 1 として、Spinor の AOT C 出力を
# Unikraft でビルドするための前段ステップを自動化する。
#
# 実行フロー:
#   1. cabal run spinor -- build --emit-c examples/hello.spin
#      → hello.c をカレントディレクトリに生成
#   2. hello.c を os/ に移動
#   3. runtime/spinor.{c,h} を os/ にコピー
#   4. 次のステップ (kraft build / kraft run) を案内
#
# 関連:
#   - Issue #47 (--emit-c フラグの実装)
#   - Issue #50 (codegen の print プリミティブ修正)
#   - docs/vision/unikernel-architecture.md

set -euo pipefail

# ===========================================================================
# パス設定
# ===========================================================================
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

INPUT_SPIN="examples/hello.spin"
OUT_DIR="os"
RUNTIME_DIR="runtime"

# ===========================================================================
# 前提条件チェック
# ===========================================================================
if [[ ! -f "$INPUT_SPIN" ]]; then
  echo "error: $INPUT_SPIN が見つかりません。プロジェクトルートで実行してください。" >&2
  exit 1
fi

if [[ ! -d "$OUT_DIR" ]]; then
  echo "error: $OUT_DIR/ ディレクトリが見つかりません (Kraftfile / Makefile.uk が必要)。" >&2
  exit 1
fi

if [[ ! -f "$RUNTIME_DIR/spinor.c" ]] || [[ ! -f "$RUNTIME_DIR/spinor.h" ]]; then
  echo "error: $RUNTIME_DIR/spinor.{c,h} が見つかりません。" >&2
  exit 1
fi

# ===========================================================================
# Step 1: AOT で hello.c を生成
# ===========================================================================
echo "[1/3] Compiling $INPUT_SPIN with --emit-c..."
cabal run -v0 spinor -- build --emit-c "$INPUT_SPIN"

if [[ ! -f "hello.c" ]]; then
  echo "error: spinor --emit-c は完了しましたが、hello.c が生成されていません。" >&2
  exit 1
fi

# ===========================================================================
# Step 2: hello.c を os/ に移動
# ===========================================================================
echo "[2/3] Moving hello.c → $OUT_DIR/hello.c"
mv -f hello.c "$OUT_DIR/hello.c"

# ===========================================================================
# Step 3: runtime/spinor.{c,h} を os/ にコピー
# ===========================================================================
echo "[3/3] Copying $RUNTIME_DIR/spinor.{c,h} → $OUT_DIR/"
cp -f "$RUNTIME_DIR/spinor.c" "$OUT_DIR/spinor.c"
cp -f "$RUNTIME_DIR/spinor.h" "$OUT_DIR/spinor.h"

# ===========================================================================
# 完了メッセージ
# ===========================================================================
echo ""
echo "OS sources prepared in os/ directory! 🚀"
echo "To build the Unikernel, run: cd os && kraft build"
echo "To run the Unikernel, run: kraft run"
