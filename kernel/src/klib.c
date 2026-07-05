/* klib.c — フリースタンディング環境向けの最小ユーティリティ (Issue #74) */
#include <stddef.h>
#include <stdint.h>

#include "klib.h"
#include "serial.h"

/* gcc は freestanding でも memset/memcpy 呼び出しを生成しうるため必ず提供する */

void *memset(void *dst, int c, size_t n)
{
    uint8_t *d = dst;
    while (n--) {
        *d++ = (uint8_t)c;
    }
    return dst;
}

void *memcpy(void *dst, const void *src, size_t n)
{
    uint8_t *d = dst;
    const uint8_t *s = src;
    while (n--) {
        *d++ = *s++;
    }
    return dst;
}

size_t strlen(const char *s)
{
    size_t n = 0;
    while (s[n] != '\0') {
        n++;
    }
    return n;
}

char *strcpy(char *dst, const char *src)
{
    char *d = dst;
    while ((*d++ = *src++) != '\0') {
        /* copy */
    }
    return dst;
}

char *strcat(char *dst, const char *src)
{
    strcpy(dst + strlen(dst), src);
    return dst;
}

char *strncpy(char *dst, const char *src, size_t n)
{
    size_t i = 0;
    for (; i < n && src[i] != '\0'; i++) {
        dst[i] = src[i];
    }
    for (; i < n; i++) {
        dst[i] = '\0';
    }
    return dst;
}

int strcmp(const char *a, const char *b)
{
    while (*a && (*a == *b)) {
        a++;
        b++;
    }
    return (unsigned char)*a - (unsigned char)*b;
}

void serial_write_u64(uint64_t v)
{
    char buf[21];
    int i = 20;
    buf[i] = '\0';
    if (v == 0) {
        serial_write("0");
        return;
    }
    while (v > 0 && i > 0) {
        buf[--i] = (char)('0' + (v % 10));
        v /= 10;
    }
    serial_write(&buf[i]);
}

void serial_write_hex(uint64_t v)
{
    static const char digits[] = "0123456789abcdef";
    char buf[19];
    int i = 18;
    buf[i] = '\0';
    if (v == 0) {
        serial_write("0x0");
        return;
    }
    while (v > 0 && i > 2) {
        buf[--i] = digits[v & 0xF];
        v >>= 4;
    }
    buf[--i] = 'x';
    buf[--i] = '0';
    serial_write(&buf[i]);
}
