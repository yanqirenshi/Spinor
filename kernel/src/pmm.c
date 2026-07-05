/* pmm.c — 物理ページアロケータ (Phase R1-2 / Issue #74)
 *
 * アルゴリズム: intrusive free-list スタック方式
 *   - Limine メモリマップの USABLE 領域を 4KB フレームに分割し、
 *     「空きページ自身の先頭 8 バイト」に次の空きページへのポインタを
 *     格納して単方向リスト (スタック) を作る。
 *   - alloc = pop / free = push。どちらも O(1)、管理用メタデータの
 *     追加メモリは不要 (空きページ自体がリストノードを兼ねる)。
 *   - 物理アドレスへのアクセスは Limine の HHDM (Higher Half Direct
 *     Map) オフセットを介して行う。
 *
 * ヒープ切り出し:
 *   - kmalloc 用ヒープは「物理的に連続」である必要があるため、
 *     最大の USABLE 領域から先頭 KHEAP_SIZE バイトを free スタックに
 *     積む前に確保し、kmalloc_init に渡す。
 */
#include <stddef.h>
#include <stdint.h>

#include <limine.h>

#include "memory.h"
#include "serial.h"
#include "klib.h"

/* kmalloc.c 内部の初期化フック (公開 API ではないため memory.h に置かない) */
void kmalloc_init(void *heap_base, size_t heap_size);

/* ---- Limine リクエスト --------------------------------------------- */

__attribute__((used, section(".limine_requests")))
static volatile struct limine_memmap_request memmap_request = {
    .id = LIMINE_MEMMAP_REQUEST,
    .revision = 0,
};

__attribute__((used, section(".limine_requests")))
static volatile struct limine_hhdm_request hhdm_request = {
    .id = LIMINE_HHDM_REQUEST,
    .revision = 0,
};

/* ---- PMM 状態 ------------------------------------------------------- */

#define KHEAP_SIZE (4u * 1024u * 1024u)   /* kmalloc ヒープ: 4 MiB */

static uint64_t hhdm_offset;
static void    *free_head;      /* 空きページスタックの先頭 (HHDM 仮想) */
static uint64_t total_pages;
static uint64_t free_count;

static inline void *phys_to_virt(uint64_t phys)
{
    return (void *)(phys + hhdm_offset);
}

/* ---- API ------------------------------------------------------------ */

void pmm_free_page(void *page)
{
    if (page == NULL) {
        return;
    }
    /* ページ先頭に「次の空きページ」を書き込んで push */
    *(void **)page = free_head;
    free_head = page;
    free_count++;
}

void *pmm_alloc_page(void)
{
    if (free_head == NULL) {
        return NULL;
    }
    void *page = free_head;
    free_head = *(void **)page;   /* pop */
    free_count--;
    return page;
}

uint64_t pmm_total_pages(void) { return total_pages; }
uint64_t pmm_free_pages(void)  { return free_count; }

int pmm_init(void)
{
    if (memmap_request.response == NULL || hhdm_request.response == NULL) {
        serial_write("pmm: memmap/hhdm response missing\n");
        return -1;
    }

    hhdm_offset = hhdm_request.response->offset;

    struct limine_memmap_response *mm = memmap_request.response;

    /* 1. 最大の USABLE 領域を探す (kmalloc ヒープの切り出し元) */
    struct limine_memmap_entry *largest = NULL;
    for (uint64_t i = 0; i < mm->entry_count; i++) {
        struct limine_memmap_entry *e = mm->entries[i];
        if (e->type != LIMINE_MEMMAP_USABLE) {
            continue;
        }
        if (largest == NULL || e->length > largest->length) {
            largest = e;
        }
    }
    if (largest == NULL || largest->length < KHEAP_SIZE + PAGE_SIZE) {
        serial_write("pmm: no usable memory region large enough\n");
        return -1;
    }

    /* 2. ヒープを最大領域の先頭から切り出して kmalloc を初期化 */
    uint64_t heap_phys = largest->base;
    kmalloc_init(phys_to_virt(heap_phys), KHEAP_SIZE);

    /* 3. 残りの USABLE フレームをすべて free スタックへ push
     *    (Limine 仕様により USABLE エントリは 4KB アラインかつ互いに不重複) */
    free_head   = NULL;
    total_pages = 0;
    free_count  = 0;

    for (uint64_t i = 0; i < mm->entry_count; i++) {
        struct limine_memmap_entry *e = mm->entries[i];
        if (e->type != LIMINE_MEMMAP_USABLE) {
            continue;
        }
        uint64_t start = e->base;
        uint64_t end   = e->base + e->length;
        if (e == largest) {
            start += KHEAP_SIZE;   /* ヒープに割いた分はスキップ */
        }
        for (uint64_t p = start; p + PAGE_SIZE <= end; p += PAGE_SIZE) {
            pmm_free_page(phys_to_virt(p));
            total_pages++;
        }
    }

    /* free_count は push で加算済みだが total と一致させておく */
    free_count = total_pages;

    serial_write("pmm: hhdm offset       = ");
    serial_write_hex(hhdm_offset);
    serial_write("\npmm: usable pages      = ");
    serial_write_u64(total_pages);
    serial_write(" (");
    serial_write_u64(total_pages * PAGE_SIZE / (1024 * 1024));
    serial_write(" MiB)\npmm: kmalloc heap size = ");
    serial_write_u64(KHEAP_SIZE / (1024 * 1024));
    serial_write(" MiB\n");

    return 0;
}
