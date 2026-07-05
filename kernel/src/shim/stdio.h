/* shim/stdio.h — ベアメタルビルド用スタブ (Issue #75)
 *
 * Spinor コンパイラが生成する C コードは `#include <stdio.h>` を含むが、
 * freestanding 環境に stdio は存在しない。カーネルビルドでは -I src/shim を
 * インクルードパスの先頭に置き、この空スタブで解決させる。
 * (printf 等は spinor_rt.c がシリアル出力版を提供する)
 */
#ifndef SPINOR_SHIM_STDIO_H
#define SPINOR_SHIM_STDIO_H

/* 意図的に空 */

#endif /* SPINOR_SHIM_STDIO_H */
