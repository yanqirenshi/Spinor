/* klib.h — フリースタンディング環境向けの最小ユーティリティ (Issue #74)
 *
 * 標準 C ライブラリ非依存。gcc が暗黙に生成しうる memset/memcpy も提供する。
 */
#ifndef SPINOR_KLIB_H
#define SPINOR_KLIB_H

#include <stddef.h>
#include <stdint.h>

void *memset(void *dst, int c, size_t n);
void *memcpy(void *dst, const void *src, size_t n);

/* 数値をシリアルへ出力するヘルパー (serial_write を利用) */
void serial_write_u64(uint64_t v);   /* 10 進 */
void serial_write_hex(uint64_t v);   /* 16 進 (0x プレフィックス付き) */

#endif /* SPINOR_KLIB_H */
