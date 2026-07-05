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

/* ========== リスト操作 (cons セル) ========== */

SpObject* sp_cons(SpObject* car, SpObject* cdr) {
    SpPair* pair = (SpPair*)malloc(sizeof(SpPair));
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));
    if (!pair || !obj) {
        fprintf(stderr, "Spinor: out of memory\n");
        exit(1);
    }
    pair->car = car;
    pair->cdr = cdr;
    obj->type = SP_PAIR;
    obj->value.pair = pair;
    return obj;
}

SpObject* sp_car(SpObject* pair) {
    if (!pair || pair->type != SP_PAIR) {
        fprintf(stderr, "Spinor: car of non-pair\n");
        exit(1);
    }
    return pair->value.pair->car;
}

SpObject* sp_cdr(SpObject* pair) {
    if (!pair || pair->type != SP_PAIR) {
        fprintf(stderr, "Spinor: cdr of non-pair\n");
        exit(1);
    }
    return pair->value.pair->cdr;
}

SpObject* sp_is_nil(SpObject* obj) {
    return sp_make_bool(obj != NULL && obj->type == SP_NIL);
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

/* 値を改行なしで表示する内部ヘルパー (リストの再帰表示に使用) */
static void sp_print_inner(SpObject* obj) {
    if (!obj) {
        printf("NULL");
        return;
    }
    switch (obj->type) {
        case SP_NIL:
            printf("()");
            break;
        case SP_BOOL:
            printf("%s", obj->value.boolean ? "#t" : "#f");
            break;
        case SP_INT:
            printf("%ld", obj->value.integer);
            break;
        case SP_STR:
            printf("%s", obj->value.string);
            break;
        case SP_PAIR: {
            /* proper list は (a b c)、improper list は (a . b) 形式 */
            printf("(");
            SpObject* cur = obj;
            while (cur && cur->type == SP_PAIR) {
                sp_print_inner(cur->value.pair->car);
                cur = cur->value.pair->cdr;
                if (cur && cur->type == SP_PAIR) {
                    printf(" ");
                }
            }
            if (cur && cur->type != SP_NIL) {
                printf(" . ");
                sp_print_inner(cur);
            }
            printf(")");
            break;
        }
        default:
            printf("<unknown>");
            break;
    }
}

SpObject* sp_print(SpObject* obj) {
    sp_print_inner(obj);
    printf("\n");
    return obj;  /* Lisp 伝統: print は引数をそのまま返す */
}
