/**
 * Spinor Runtime Header
 *
 * C99 準拠のランタイムヘッダ。
 * Tagged Union を用いて Spinor の動的型システムを C で表現する。
 */

#ifndef SPINOR_H
#define SPINOR_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

/* ========== 型タグ ========== */

typedef enum {
    SP_NIL,
    SP_BOOL,
    SP_INT,
    SP_STR,      /* 文字列型 */
    SP_SYM,      /* 将来の拡張用 */
    SP_PAIR,     /* 将来の拡張用 */
    SP_FUN,      /* 将来の拡張用 */
    SP_CLOSURE   /* 将来の拡張用 */
} SpType;

/* ========== 値ユニオンとオブジェクト構造体 ========== */

/* forward declaration for SpPair */
struct SpObject;

typedef struct SpPair {
    struct SpObject* car;
    struct SpObject* cdr;
} SpPair;

typedef union {
    bool     boolean;
    long     integer;
    char*    string;   /* 文字列型 (heap-allocated) */
    char*    symbol;
    SpPair*  pair;
} SpValue;

typedef struct SpObject {
    SpType  type;
    SpValue value;
} SpObject;

/* ========== コンストラクタ ========== */

SpObject* sp_make_nil(void);
SpObject* sp_make_bool(bool value);
SpObject* sp_make_int(long value);
SpObject* sp_make_str(const char* s);

/* ========== プリミティブ演算 ========== */

SpObject* sp_add(SpObject* a, SpObject* b);
SpObject* sp_sub(SpObject* a, SpObject* b);
SpObject* sp_mul(SpObject* a, SpObject* b);
SpObject* sp_div(SpObject* a, SpObject* b);
SpObject* sp_eq(SpObject* a, SpObject* b);
SpObject* sp_lt(SpObject* a, SpObject* b);
SpObject* sp_gt(SpObject* a, SpObject* b);
SpObject* sp_lte(SpObject* a, SpObject* b);
SpObject* sp_gte(SpObject* a, SpObject* b);

/* ========== 文字列操作 ========== */

SpObject* sp_str_append(SpObject* a, SpObject* b);
SpObject* sp_str_length(SpObject* s);
SpObject* sp_substring(SpObject* s, SpObject* start, SpObject* end);
SpObject* sp_str_eq(SpObject* a, SpObject* b);

/* ========== ファイル I/O ========== */

SpObject* sp_read_file(SpObject* path);
SpObject* sp_write_file(SpObject* path, SpObject* content);
SpObject* sp_append_file(SpObject* path, SpObject* content);
SpObject* sp_file_exists(SpObject* path);

/* ========== ユーティリティ ========== */

void sp_print(SpObject* obj);
const char* sp_format(SpObject* obj);

/* ========== 旧 API との互換性 (エイリアス) ========== */

#define sp_int(v)   sp_make_int(v)
#define sp_bool(v)  sp_make_bool(v)

#endif /* SPINOR_H */
