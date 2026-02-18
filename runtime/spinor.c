/**
 * Spinor Runtime Implementation
 *
 * spinor.h で宣言されたランタイム関数の実装。
 * メモリ管理は malloc のみ (GC は将来の課題)。
 */

#include "spinor.h"

/* ========== コンストラクタ ========== */

SpObject* sp_make_nil(void) {
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_NIL;
    return obj;
}

SpObject* sp_make_bool(bool value) {
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_BOOL;
    obj->value.boolean = value;
    return obj;
}

SpObject* sp_make_int(long value) {
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_INT;
    obj->value.integer = value;
    return obj;
}

/* ========== プリミティブ演算 ========== */

SpObject* sp_add(SpObject* a, SpObject* b) {
    /* 型チェック (将来的には必須だが、現時点では省略可能) */
    return sp_make_int(a->value.integer + b->value.integer);
}

SpObject* sp_sub(SpObject* a, SpObject* b) {
    return sp_make_int(a->value.integer - b->value.integer);
}

SpObject* sp_mul(SpObject* a, SpObject* b) {
    return sp_make_int(a->value.integer * b->value.integer);
}

SpObject* sp_div(SpObject* a, SpObject* b) {
    if (b->value.integer == 0) {
        fprintf(stderr, "Spinor: division by zero\n");
        exit(1);
    }
    return sp_make_int(a->value.integer / b->value.integer);
}

SpObject* sp_eq(SpObject* a, SpObject* b) {
    /* 同じ型のみ比較可能 */
    if (a->type != b->type) {
        return sp_make_bool(false);
    }
    switch (a->type) {
        case SP_NIL:
            return sp_make_bool(true);
        case SP_BOOL:
            return sp_make_bool(a->value.boolean == b->value.boolean);
        case SP_INT:
            return sp_make_bool(a->value.integer == b->value.integer);
        default:
            return sp_make_bool(false);
    }
}

SpObject* sp_lt(SpObject* a, SpObject* b) {
    return sp_make_bool(a->value.integer < b->value.integer);
}

SpObject* sp_gt(SpObject* a, SpObject* b) {
    return sp_make_bool(a->value.integer > b->value.integer);
}

/* ========== ユーティリティ ========== */

void sp_print(SpObject* obj) {
    if (!obj) {
        printf("NULL\n");
        return;
    }
    switch (obj->type) {
        case SP_NIL:
            printf("()\n");
            break;
        case SP_BOOL:
            printf("%s\n", obj->value.boolean ? "#t" : "#f");
            break;
        case SP_INT:
            printf("%ld\n", obj->value.integer);
            break;
        default:
            printf("<unknown>\n");
            break;
    }
}
