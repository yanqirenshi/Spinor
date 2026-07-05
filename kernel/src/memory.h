/* memory.h — Spinor OS 物理メモリ管理 API (Phase R1-2 / Issue #74)
 *
 * 層構成:
 *   1. PMM (Page Frame Allocator) — Limine メモリマップ由来の 4KB ページ管理
 *   2. kmalloc / kfree            — PMM 上に構築した可変長ヒープ
 *
 * ポインタはすべて HHDM (Higher Half Direct Map) 上の仮想アドレスで
 * やり取りする (呼び出し側がそのまま読み書きできる)。
 */
#ifndef SPINOR_MEMORY_H
#define SPINOR_MEMORY_H

#include <stddef.h>
#include <stdint.h>

#define PAGE_SIZE 4096

/* ---- 物理ページアロケータ (PMM) ------------------------------------ */

/* Limine の memmap / hhdm レスポンスを解析して PMM とヒープを初期化する。
 * 成功なら 0、致命的失敗 (usable メモリ無し等) なら -1 を返す。 */
int pmm_init(void);

/* 4KB ページを 1 枚確保する (HHDM 仮想アドレス)。枯渇時は NULL。 */
void *pmm_alloc_page(void);

/* pmm_alloc_page で確保したページを返却する。 */
void pmm_free_page(void *page);

/* 統計: 管理下の総ページ数 / 現在の空きページ数 */
uint64_t pmm_total_pages(void);
uint64_t pmm_free_pages(void);

/* ---- 可変長ヒープ (kmalloc / kfree) --------------------------------- */

/* size バイトを確保する (16 バイト境界)。失敗時は NULL。 */
void *kmalloc(size_t size);

/* kmalloc で確保した領域を解放する (NULL は無視)。 */
void kfree(void *ptr);

#endif /* SPINOR_MEMORY_H */
