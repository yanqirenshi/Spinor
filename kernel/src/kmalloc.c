/* kmalloc.c — 可変長ヒープアロケータ (Phase R1-2 / Issue #74)
 *
 * アルゴリズム: K&R (The C Programming Language 8.7) スタイルの
 * first-fit フリーリスト
 *   - ヒープは pmm_init が切り出した物理連続 4 MiB の固定アリーナ。
 *   - 空きブロックを「アドレス昇順の循環単方向リスト」で管理する。
 *   - kmalloc: リストを先頭から走査し、最初に収まるブロックを使う
 *     (first-fit)。大きすぎるブロックは末尾を切り出して分割する。
 *   - kfree: アドレス順の挿入位置を探し、前後のブロックと物理的に
 *     隣接していれば結合 (coalescing) してフラグメンテーションを抑える。
 *   - ヘッダは 16 バイト (union header) で、返却ポインタは 16 バイト境界。
 *
 * 制限 (Phase R1-2 時点):
 *   - アリーナは固定長。枯渇したら NULL を返す (拡張は将来フェーズで
 *     仮想メモリ管理と併せて対応)。
 */
#include <stddef.h>
#include <stdint.h>

#include "memory.h"

void kmalloc_init(void *heap_base, size_t heap_size);

/* ---- ブロックヘッダ -------------------------------------------------- */

typedef long double Align;   /* 16 バイトアラインを強制する */

union header {
    struct {
        union header *next;  /* 次の空きブロック (循環リスト) */
        size_t        units; /* このブロックのサイズ (ヘッダ単位、自身含む) */
    } s;
    Align _align;
};

typedef union header Header;

static Header  base_node;          /* リストの起点となる空ノード */
static Header *free_list = NULL;   /* 空きリストの探索開始点 */

/* ---- 初期化 ---------------------------------------------------------- */

void kmalloc_init(void *heap_base, size_t heap_size)
{
    /* アリーナ全体を 1 個の空きブロックとしてリストに登録する */
    Header *arena = (Header *)heap_base;
    arena->s.units = heap_size / sizeof(Header);

    base_node.s.next  = arena;
    base_node.s.units = 0;
    arena->s.next     = &base_node;

    free_list = &base_node;
}

/* ---- kmalloc --------------------------------------------------------- */

void *kmalloc(size_t size)
{
    if (size == 0 || free_list == NULL) {
        return NULL;
    }

    /* 要求バイト数 → ヘッダ単位 (ヘッダ 1 個分を足して切り上げ) */
    size_t nunits = (size + sizeof(Header) - 1) / sizeof(Header) + 1;

    Header *prev = free_list;
    for (Header *p = prev->s.next; ; prev = p, p = p->s.next) {
        if (p->s.units >= nunits) {
            if (p->s.units == nunits) {
                /* ちょうど収まる: リストから外す */
                prev->s.next = p->s.next;
            } else {
                /* 大きい: 末尾から nunits 切り出す (残りはリストに残す) */
                p->s.units -= nunits;
                p += p->s.units;
                p->s.units = nunits;
            }
            free_list = prev;
            return (void *)(p + 1);
        }
        if (p == free_list) {
            return NULL;   /* 一周した = 収まる空きが無い */
        }
    }
}

/* ---- kfree ----------------------------------------------------------- */

void kfree(void *ptr)
{
    if (ptr == NULL || free_list == NULL) {
        return;
    }

    Header *bp = (Header *)ptr - 1;   /* ブロックヘッダへ戻る */

    /* アドレス順で挿入位置を探す (循環リストの折り返しにも対応) */
    Header *p = free_list;
    for (; !(bp > p && bp < p->s.next); p = p->s.next) {
        if (p >= p->s.next && (bp > p || bp < p->s.next)) {
            break;   /* リスト末尾 ↔ 先頭の折り返し位置 */
        }
    }

    /* 後方ブロックと隣接していれば結合 */
    if (bp + bp->s.units == p->s.next) {
        bp->s.units += p->s.next->s.units;
        bp->s.next   = p->s.next->s.next;
    } else {
        bp->s.next = p->s.next;
    }

    /* 前方ブロックと隣接していれば結合 */
    if (p + p->s.units == bp) {
        p->s.units += bp->s.units;
        p->s.next   = bp->s.next;
    } else {
        p->s.next = bp;
    }

    free_list = p;
}
