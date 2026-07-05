/* shim/spinor.h — Spinor ランタイムヘッダ (ベアメタル版, Issue #75)
 *
 * runtime/spinor.h と同一の API を宣言するが、stdio/stdlib には依存しない
 * (freestanding ヘッダ stdbool.h / stddef.h のみ)。
 * 実装はカーネル側の spinor_rt.c (kmalloc + シリアル出力ベース)。
 */
#ifndef SPINOR_H   /* runtime/spinor.h と同じガード名 (二重取込防止) */
#define SPINOR_H

#include <stdbool.h>
#include <stddef.h>

/* ========== 型タグ ========== */

typedef enum {
    SP_NIL,
    SP_BOOL,
    SP_INT,
    SP_STR,
    SP_SYM,
    SP_PAIR,
    SP_FUN,
    SP_CLOSURE
} SpType;

/* ========== 値ユニオンとオブジェクト構造体 ========== */

struct SpObject;

typedef struct SpPair {
    struct SpObject* car;
    struct SpObject* cdr;
} SpPair;

typedef union {
    bool     boolean;
    long     integer;
    char*    string;
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

/* ========== リスト操作 (cons セル) ========== */

SpObject* sp_cons(SpObject* car, SpObject* cdr);
SpObject* sp_car(SpObject* pair);
SpObject* sp_cdr(SpObject* pair);
SpObject* sp_is_nil(SpObject* obj);

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

/* ========== ファイル I/O (ベアメタルでは未サポート: パニックスタブ) ==== */

SpObject* sp_read_file(SpObject* path);
SpObject* sp_write_file(SpObject* path, SpObject* content);
SpObject* sp_append_file(SpObject* path, SpObject* content);
SpObject* sp_file_exists(SpObject* path);

/* ========== ユーティリティ ========== */

SpObject* sp_print(SpObject* obj);

/* ========== 旧 API との互換性 (エイリアス) ========== */

#define sp_int(v)   sp_make_int(v)
#define sp_bool(v)  sp_make_bool(v)

#endif /* SPINOR_H */
