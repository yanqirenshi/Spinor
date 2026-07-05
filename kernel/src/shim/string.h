/* shim/string.h — ベアメタルビルド用スタブ (Issue #75)
 *
 * 文字列関数の実体は kernel/src/klib.c が提供する。
 */
#ifndef SPINOR_SHIM_STRING_H
#define SPINOR_SHIM_STRING_H

#include <stddef.h>

void  *memset(void *dst, int c, size_t n);
void  *memcpy(void *dst, const void *src, size_t n);
size_t strlen(const char *s);
char  *strcpy(char *dst, const char *src);
char  *strcat(char *dst, const char *src);
char  *strncpy(char *dst, const char *src, size_t n);
int    strcmp(const char *a, const char *b);

#endif /* SPINOR_SHIM_STRING_H */
