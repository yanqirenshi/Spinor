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

SpObject* sp_make_str(const char* s) {
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_STR;
    obj->value.string = strdup(s);  /* ヒープにコピー */
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

SpObject* sp_lte(SpObject* a, SpObject* b) {
    return sp_make_bool(a->value.integer <= b->value.integer);
}

SpObject* sp_gte(SpObject* a, SpObject* b) {
    return sp_make_bool(a->value.integer >= b->value.integer);
}

/* ========== 文字列操作 ========== */

SpObject* sp_str_append(SpObject* a, SpObject* b) {
    size_t len_a = strlen(a->value.string);
    size_t len_b = strlen(b->value.string);
    char* result = (char*)malloc(len_a + len_b + 1);
    if (!result) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    strcpy(result, a->value.string);
    strcat(result, b->value.string);
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_STR;
    obj->value.string = result;
    return obj;
}

SpObject* sp_str_length(SpObject* s) {
    return sp_make_int((long)strlen(s->value.string));
}

SpObject* sp_substring(SpObject* s, SpObject* start, SpObject* end) {
    const char* str = s->value.string;
    long st = start->value.integer;
    long en = end->value.integer;
    long len = (long)strlen(str);

    if (st < 0) st = 0;
    if (en > len) en = len;
    if (st >= en) return sp_make_str("");

    long sub_len = en - st;
    char* result = (char*)malloc(sub_len + 1);
    if (!result) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    strncpy(result, str + st, sub_len);
    result[sub_len] = '\0';

    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_STR;
    obj->value.string = result;
    return obj;
}

SpObject* sp_str_eq(SpObject* a, SpObject* b) {
    return sp_make_bool(strcmp(a->value.string, b->value.string) == 0);
}

/* ========== ファイル I/O ========== */

SpObject* sp_read_file(SpObject* path) {
    FILE* f = fopen(path->value.string, "r");
    if (!f) return sp_make_str("");  /* エラー時は空文字列 */

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char* content = (char*)malloc(size + 1);
    if (!content) {
        fclose(f);
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    fread(content, 1, size, f);
    content[size] = '\0';
    fclose(f);

    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    obj->type = SP_STR;
    obj->value.string = content;
    return obj;
}

SpObject* sp_write_file(SpObject* path, SpObject* content) {
    FILE* f = fopen(path->value.string, "w");
    if (!f) return sp_make_bool(false);
    fputs(content->value.string, f);
    fclose(f);
    return sp_make_bool(true);
}

SpObject* sp_append_file(SpObject* path, SpObject* content) {
    FILE* f = fopen(path->value.string, "a");
    if (!f) return sp_make_bool(false);
    fputs(content->value.string, f);
    fclose(f);
    return sp_make_bool(true);
}

SpObject* sp_file_exists(SpObject* path) {
    FILE* f = fopen(path->value.string, "r");
    if (f) {
        fclose(f);
        return sp_make_bool(true);
    }
    return sp_make_bool(false);
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
        case SP_STR:
            printf("%s\n", obj->value.string);
            break;
        default:
            printf("<unknown>\n");
            break;
    }
}
