/**
 * Spinor REPL Evaluator
 *
 * ブラウザ REPL 用の簡易 S 式評価器。
 * Tokenizer → Parser → Evaluator のパイプラインで処理する。
 *
 * 対応する構文:
 *   - 整数リテラル: 42, -7
 *   - 真偽値リテラル: #t, #f
 *   - nil リテラル: nil, ()
 *   - 算術演算: +, -, *, /
 *   - 比較演算: =, <, >, <=, >=
 *   - 条件式: (if cond then else)
 *   - ネスト式: (+ (* 3 4) (- 10 5))
 */

#include "repl.h"
#include <string.h>
#include <ctype.h>

/* ========== 出力バッファ ========== */

#define FORMAT_BUF_SIZE 1024
static char format_buf[FORMAT_BUF_SIZE];

/* ========== Tokenizer ========== */

typedef enum {
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_INT,
    TOK_SYMBOL,
    TOK_BOOL_TRUE,
    TOK_BOOL_FALSE,
    TOK_EOF,
    TOK_ERROR
} TokenType;

typedef struct {
    TokenType type;
    long      int_val;
    char      sym[64];
} Token;

#define MAX_TOKENS 256
static Token tokens[MAX_TOKENS];
static int   token_count;
static int   token_pos;

static int tokenize(const char* input) {
    token_count = 0;
    token_pos = 0;
    int i = 0;
    int len = (int)strlen(input);

    while (i < len && token_count < MAX_TOKENS) {
        /* skip whitespace */
        while (i < len && isspace((unsigned char)input[i])) i++;
        if (i >= len) break;

        char c = input[i];

        if (c == '(') {
            tokens[token_count].type = TOK_LPAREN;
            token_count++;
            i++;
        } else if (c == ')') {
            tokens[token_count].type = TOK_RPAREN;
            token_count++;
            i++;
        } else if (c == '#') {
            /* #t or #f */
            if (i + 1 < len && input[i + 1] == 't') {
                tokens[token_count].type = TOK_BOOL_TRUE;
                token_count++;
                i += 2;
            } else if (i + 1 < len && input[i + 1] == 'f') {
                tokens[token_count].type = TOK_BOOL_FALSE;
                token_count++;
                i += 2;
            } else {
                tokens[token_count].type = TOK_ERROR;
                token_count++;
                return -1;
            }
        } else if (isdigit((unsigned char)c) ||
                   (c == '-' && i + 1 < len && isdigit((unsigned char)input[i + 1]))) {
            /* integer literal */
            long val = 0;
            int sign = 1;
            if (c == '-') {
                sign = -1;
                i++;
            }
            while (i < len && isdigit((unsigned char)input[i])) {
                val = val * 10 + (input[i] - '0');
                i++;
            }
            tokens[token_count].type = TOK_INT;
            tokens[token_count].int_val = sign * val;
            token_count++;
        } else {
            /* symbol */
            int j = 0;
            while (i < len && !isspace((unsigned char)input[i]) &&
                   input[i] != '(' && input[i] != ')' && j < 63) {
                tokens[token_count].sym[j++] = input[i++];
            }
            tokens[token_count].sym[j] = '\0';

            /* check for nil */
            if (strcmp(tokens[token_count].sym, "nil") == 0) {
                tokens[token_count].type = TOK_SYMBOL;
            } else {
                tokens[token_count].type = TOK_SYMBOL;
            }
            token_count++;
        }
    }

    tokens[token_count].type = TOK_EOF;
    token_count++;
    return 0;
}

static Token* peek_token(void) {
    if (token_pos < token_count)
        return &tokens[token_pos];
    return &tokens[token_count - 1]; /* EOF */
}

static Token* next_token(void) {
    Token* t = peek_token();
    if (token_pos < token_count - 1) /* don't advance past EOF */
        token_pos++;
    return t;
}

/* ========== Parser (AST) ========== */

typedef enum {
    NODE_INT,
    NODE_BOOL,
    NODE_NIL,
    NODE_SYM,
    NODE_LIST
} NodeType;

typedef struct ASTNode {
    NodeType type;
    long     int_val;
    bool     bool_val;
    char     sym[64];
    struct ASTNode* children[32];
    int      child_count;
} ASTNode;

#define NODE_POOL_SIZE 256
static ASTNode node_pool[NODE_POOL_SIZE];
static int     node_pool_pos;

static ASTNode* alloc_node(void) {
    if (node_pool_pos >= NODE_POOL_SIZE) return NULL;
    ASTNode* n = &node_pool[node_pool_pos++];
    n->child_count = 0;
    return n;
}

static ASTNode* parse_expr(void);

static ASTNode* parse_list(void) {
    /* '(' has been consumed */
    ASTNode* list = alloc_node();
    if (!list) return NULL;
    list->type = NODE_LIST;
    list->child_count = 0;

    while (peek_token()->type != TOK_RPAREN && peek_token()->type != TOK_EOF) {
        ASTNode* child = parse_expr();
        if (!child) return NULL;
        if (list->child_count < 32) {
            list->children[list->child_count++] = child;
        }
    }

    if (peek_token()->type == TOK_RPAREN) {
        next_token(); /* consume ')' */
    }

    /* empty list () → NIL */
    if (list->child_count == 0) {
        list->type = NODE_NIL;
    }

    return list;
}

static ASTNode* parse_expr(void) {
    Token* t = next_token();

    switch (t->type) {
        case TOK_INT: {
            ASTNode* n = alloc_node();
            if (!n) return NULL;
            n->type = NODE_INT;
            n->int_val = t->int_val;
            return n;
        }
        case TOK_BOOL_TRUE: {
            ASTNode* n = alloc_node();
            if (!n) return NULL;
            n->type = NODE_BOOL;
            n->bool_val = true;
            return n;
        }
        case TOK_BOOL_FALSE: {
            ASTNode* n = alloc_node();
            if (!n) return NULL;
            n->type = NODE_BOOL;
            n->bool_val = false;
            return n;
        }
        case TOK_SYMBOL: {
            if (strcmp(t->sym, "nil") == 0) {
                ASTNode* n = alloc_node();
                if (!n) return NULL;
                n->type = NODE_NIL;
                return n;
            }
            ASTNode* n = alloc_node();
            if (!n) return NULL;
            n->type = NODE_SYM;
            strncpy(n->sym, t->sym, 63);
            n->sym[63] = '\0';
            return n;
        }
        case TOK_LPAREN:
            return parse_list();
        default:
            return NULL;
    }
}

/* ========== Evaluator ========== */

static SpObject* eval_node(ASTNode* node);

static SpObject* eval_list(ASTNode* node) {
    if (node->child_count == 0) {
        return sp_make_nil();
    }

    ASTNode* head = node->children[0];

    /* if 式 */
    if (head->type == NODE_SYM && strcmp(head->sym, "if") == 0) {
        if (node->child_count < 4) {
            return NULL; /* (if cond then else) requires 4 elements */
        }
        SpObject* cond = eval_node(node->children[1]);
        if (!cond) return NULL;

        bool is_true;
        if (cond->type == SP_BOOL) {
            is_true = cond->value.boolean;
        } else if (cond->type == SP_NIL) {
            is_true = false;
        } else {
            is_true = true; /* non-nil, non-false is truthy */
        }

        if (is_true) {
            return eval_node(node->children[2]);
        } else {
            return eval_node(node->children[3]);
        }
    }

    /* operator call: evaluate all children */
    if (head->type != NODE_SYM) {
        return NULL;
    }

    const char* op = head->sym;

    /* evaluate arguments */
    int argc = node->child_count - 1;
    SpObject* args[32];
    for (int i = 0; i < argc && i < 32; i++) {
        args[i] = eval_node(node->children[i + 1]);
        if (!args[i]) return NULL;
    }

    /* dispatch to runtime functions */
    if (argc == 2) {
        if (strcmp(op, "+") == 0)  return sp_add(args[0], args[1]);
        if (strcmp(op, "-") == 0)  return sp_sub(args[0], args[1]);
        if (strcmp(op, "*") == 0)  return sp_mul(args[0], args[1]);
        if (strcmp(op, "/") == 0)  return sp_div(args[0], args[1]);
        if (strcmp(op, "=") == 0)  return sp_eq(args[0], args[1]);
        if (strcmp(op, "<") == 0)  return sp_lt(args[0], args[1]);
        if (strcmp(op, ">") == 0)  return sp_gt(args[0], args[1]);
        if (strcmp(op, "<=") == 0) return sp_lte(args[0], args[1]);
        if (strcmp(op, ">=") == 0) return sp_gte(args[0], args[1]);
    }

    /* variadic +, -, *, / with 1 or more args */
    if (argc >= 1 && (strcmp(op, "+") == 0 || strcmp(op, "-") == 0 ||
                      strcmp(op, "*") == 0 || strcmp(op, "/") == 0)) {
        SpObject* result = args[0];
        for (int i = 1; i < argc; i++) {
            if (strcmp(op, "+") == 0)      result = sp_add(result, args[i]);
            else if (strcmp(op, "-") == 0) result = sp_sub(result, args[i]);
            else if (strcmp(op, "*") == 0) result = sp_mul(result, args[i]);
            else if (strcmp(op, "/") == 0) result = sp_div(result, args[i]);
        }
        return result;
    }

    return NULL; /* unknown operator */
}

static SpObject* eval_node(ASTNode* node) {
    if (!node) return NULL;

    switch (node->type) {
        case NODE_INT:
            return sp_make_int(node->int_val);
        case NODE_BOOL:
            return sp_make_bool(node->bool_val);
        case NODE_NIL:
            return sp_make_nil();
        case NODE_SYM:
            /* bare symbol — not supported as variable reference yet */
            return NULL;
        case NODE_LIST:
            return eval_list(node);
        default:
            return NULL;
    }
}

/* ========== Public API ========== */

const char* sp_format(SpObject* obj) {
    if (!obj) {
        snprintf(format_buf, FORMAT_BUF_SIZE, "error: evaluation failed");
        return format_buf;
    }
    switch (obj->type) {
        case SP_NIL:
            snprintf(format_buf, FORMAT_BUF_SIZE, "()");
            break;
        case SP_BOOL:
            snprintf(format_buf, FORMAT_BUF_SIZE, "%s", obj->value.boolean ? "#t" : "#f");
            break;
        case SP_INT:
            snprintf(format_buf, FORMAT_BUF_SIZE, "%ld", obj->value.integer);
            break;
        default:
            snprintf(format_buf, FORMAT_BUF_SIZE, "<unknown>");
            break;
    }
    return format_buf;
}

const char* sp_eval_string(const char* input) {
    if (!input || strlen(input) == 0) {
        format_buf[0] = '\0';
        return format_buf;
    }

    /* reset node pool */
    node_pool_pos = 0;

    /* tokenize */
    if (tokenize(input) < 0) {
        snprintf(format_buf, FORMAT_BUF_SIZE, "error: tokenize failed");
        return format_buf;
    }

    /* parse */
    ASTNode* ast = parse_expr();
    if (!ast) {
        snprintf(format_buf, FORMAT_BUF_SIZE, "error: parse failed");
        return format_buf;
    }

    /* evaluate */
    SpObject* result = eval_node(ast);
    return sp_format(result);
}

/* ========== main (WASM entry point) ========== */

#ifndef SPINOR_REPL_NO_MAIN
int main(void) {
    /* WASM REPL mode: main does nothing, JS calls sp_eval_string() directly */
    return 0;
}
#endif
