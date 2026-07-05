/* spinor_rt.c — Spinor ランタイムのベアメタル実装 (Phase R1-3 / Issue #75)
 *
 * runtime/spinor.c (ホスト版) を freestanding 環境に移植したもの。
 * 置き換えの対応:
 *   malloc / free      → kmalloc / kfree     (Phase R1-2 のヒープ)
 *   printf / fprintf   → serial_write 系     (COM1 シリアル出力)
 *   exit(1)            → sp_panic            (メッセージ出力後 hlt 停止)
 *   ファイル I/O       → パニックスタブ       (ベアメタルに FS は無い)
 */
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#include "shim/spinor.h"
#include "memory.h"
#include "serial.h"
#include "klib.h"

/* ---- パニック -------------------------------------------------------- */

static void sp_panic(const char *msg)
{
    serial_write("Spinor PANIC: ");
    serial_write(msg);
    serial_write("\n");
    for (;;) {
        __asm__ volatile ("hlt");
    }
}

/* kmalloc の NULL チェック付きラッパ */
static void *sp_alloc(size_t size)
{
    void *p = kmalloc(size);
    if (!p) {
        sp_panic("out of memory");
    }
    return p;
}

/* ---- コンストラクタ --------------------------------------------------- */

SpObject* sp_make_nil(void)
{
    SpObject* obj = sp_alloc(sizeof(SpObject));
    obj->type = SP_NIL;
    return obj;
}

SpObject* sp_make_bool(bool value)
{
    SpObject* obj = sp_alloc(sizeof(SpObject));
    obj->type = SP_BOOL;
    obj->value.boolean = value;
    return obj;
}

SpObject* sp_make_int(long value)
{
    SpObject* obj = sp_alloc(sizeof(SpObject));
    obj->type = SP_INT;
    obj->value.integer = value;
    return obj;
}

SpObject* sp_make_str(const char* s)
{
    SpObject* obj = sp_alloc(sizeof(SpObject));
    size_t len = strlen(s);
    char* copy = sp_alloc(len + 1);
    memcpy(copy, s, len + 1);
    obj->type = SP_STR;
    obj->value.string = copy;
    return obj;
}

/* ---- リスト操作 (cons セル) ------------------------------------------- */

SpObject* sp_cons(SpObject* car, SpObject* cdr)
{
    SpPair* pair = sp_alloc(sizeof(SpPair));
    SpObject* obj = sp_alloc(sizeof(SpObject));
    pair->car = car;
    pair->cdr = cdr;
    obj->type = SP_PAIR;
    obj->value.pair = pair;
    return obj;
}

SpObject* sp_car(SpObject* pair)
{
    if (!pair || pair->type != SP_PAIR) {
        sp_panic("car of non-pair");
    }
    return pair->value.pair->car;
}

SpObject* sp_cdr(SpObject* pair)
{
    if (!pair || pair->type != SP_PAIR) {
        sp_panic("cdr of non-pair");
    }
    return pair->value.pair->cdr;
}

SpObject* sp_is_nil(SpObject* obj)
{
    return sp_make_bool(obj != NULL && obj->type == SP_NIL);
}

/* ---- プリミティブ演算 -------------------------------------------------- */

SpObject* sp_add(SpObject* a, SpObject* b)
{
    return sp_make_int(a->value.integer + b->value.integer);
}

SpObject* sp_sub(SpObject* a, SpObject* b)
{
    return sp_make_int(a->value.integer - b->value.integer);
}

SpObject* sp_mul(SpObject* a, SpObject* b)
{
    return sp_make_int(a->value.integer * b->value.integer);
}

SpObject* sp_div(SpObject* a, SpObject* b)
{
    if (b->value.integer == 0) {
        sp_panic("division by zero");
    }
    return sp_make_int(a->value.integer / b->value.integer);
}

SpObject* sp_eq(SpObject* a, SpObject* b)
{
    if (a->type != b->type) {
        return sp_make_bool(false);
    }
    switch (a->type) {
        case SP_NIL:  return sp_make_bool(true);
        case SP_BOOL: return sp_make_bool(a->value.boolean == b->value.boolean);
        case SP_INT:  return sp_make_bool(a->value.integer == b->value.integer);
        default:      return sp_make_bool(false);
    }
}

SpObject* sp_lt(SpObject* a, SpObject* b)
{
    return sp_make_bool(a->value.integer < b->value.integer);
}

SpObject* sp_gt(SpObject* a, SpObject* b)
{
    return sp_make_bool(a->value.integer > b->value.integer);
}

SpObject* sp_lte(SpObject* a, SpObject* b)
{
    return sp_make_bool(a->value.integer <= b->value.integer);
}

SpObject* sp_gte(SpObject* a, SpObject* b)
{
    return sp_make_bool(a->value.integer >= b->value.integer);
}

/* ---- 文字列操作 --------------------------------------------------------- */

SpObject* sp_str_append(SpObject* a, SpObject* b)
{
    size_t len_a = strlen(a->value.string);
    size_t len_b = strlen(b->value.string);
    char* result = sp_alloc(len_a + len_b + 1);
    memcpy(result, a->value.string, len_a);
    memcpy(result + len_a, b->value.string, len_b + 1);
    SpObject* obj = sp_alloc(sizeof(SpObject));
    obj->type = SP_STR;
    obj->value.string = result;
    return obj;
}

SpObject* sp_str_length(SpObject* s)
{
    return sp_make_int((long)strlen(s->value.string));
}

SpObject* sp_substring(SpObject* s, SpObject* start, SpObject* end)
{
    const char* str = s->value.string;
    long st  = start->value.integer;
    long en  = end->value.integer;
    long len = (long)strlen(str);

    if (st < 0)  st = 0;
    if (en > len) en = len;
    if (st >= en) return sp_make_str("");

    long sub_len = en - st;
    char* result = sp_alloc((size_t)sub_len + 1);
    memcpy(result, str + st, (size_t)sub_len);
    result[sub_len] = '\0';

    SpObject* obj = sp_alloc(sizeof(SpObject));
    obj->type = SP_STR;
    obj->value.string = result;
    return obj;
}

SpObject* sp_str_eq(SpObject* a, SpObject* b)
{
    return sp_make_bool(strcmp(a->value.string, b->value.string) == 0);
}

/* ---- ファイル I/O: ベアメタルでは未サポート (パニックスタブ) ---------- */

SpObject* sp_read_file(SpObject* path)
{
    (void)path;
    sp_panic("file I/O is not supported on bare metal (read-file)");
    return NULL;   /* not reached */
}

SpObject* sp_write_file(SpObject* path, SpObject* content)
{
    (void)path; (void)content;
    sp_panic("file I/O is not supported on bare metal (write-file)");
    return NULL;
}

SpObject* sp_append_file(SpObject* path, SpObject* content)
{
    (void)path; (void)content;
    sp_panic("file I/O is not supported on bare metal (append-file)");
    return NULL;
}

SpObject* sp_file_exists(SpObject* path)
{
    (void)path;
    sp_panic("file I/O is not supported on bare metal (file-exists?)");
    return NULL;
}

/* ---- 表示 (printf → シリアル出力) -------------------------------------- */

/* 値を改行なしでシリアルへ表示する内部ヘルパー */
static void sp_print_inner(SpObject* obj)
{
    if (!obj) {
        serial_write("NULL");
        return;
    }
    switch (obj->type) {
        case SP_NIL:
            serial_write("()");
            break;
        case SP_BOOL:
            serial_write(obj->value.boolean ? "#t" : "#f");
            break;
        case SP_INT: {
            long v = obj->value.integer;
            if (v < 0) {
                serial_write("-");
                v = -v;
            }
            serial_write_u64((uint64_t)v);
            break;
        }
        case SP_STR:
            serial_write(obj->value.string);
            break;
        case SP_PAIR: {
            /* proper list は (a b c)、improper list は (a . b) 形式 */
            serial_write("(");
            SpObject* cur = obj;
            while (cur && cur->type == SP_PAIR) {
                sp_print_inner(cur->value.pair->car);
                cur = cur->value.pair->cdr;
                if (cur && cur->type == SP_PAIR) {
                    serial_write(" ");
                }
            }
            if (cur && cur->type != SP_NIL) {
                serial_write(" . ");
                sp_print_inner(cur);
            }
            serial_write(")");
            break;
        }
        default:
            serial_write("<unknown>");
            break;
    }
}

SpObject* sp_print(SpObject* obj)
{
    sp_print_inner(obj);
    serial_write("\n");
    return obj;   /* Lisp 伝統: print は引数をそのまま返す */
}
