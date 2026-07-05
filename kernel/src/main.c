/* main.c — Spinor OS カーネルエントリーポイント
 *
 * Phase R1-1 (Issue #73): Limine ブート + シリアル出力
 * Phase R1-2 (Issue #74): 物理メモリ管理 (PMM + kmalloc) の初期化と検証
 *
 * 標準 C ライブラリ非依存 (freestanding)。
 */
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include <limine.h>

#include "serial.h"
#include "memory.h"
#include "klib.h"

/* ---- Limine リクエスト --------------------------------------------- */
/* (memmap / hhdm リクエストは pmm.c 側で定義)                          */

/* ブートローダに要求するプロトコルのリビジョン (revision 3) */
__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(3);

/* フレームバッファ要求 (画面出力のおまけ用。無くても動作する) */
__attribute__((used, section(".limine_requests")))
static volatile struct limine_framebuffer_request framebuffer_request = {
    .id = LIMINE_FRAMEBUFFER_REQUEST,
    .revision = 0,
};

/* リクエスト領域の開始/終了マーカ (linker.ld の .limine_requests に対応) */
__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;

__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

/* ---- 停止 ----------------------------------------------------------- */

static void hcf(void)
{
    for (;;) {
        __asm__ volatile ("hlt");
    }
}

/* ---- Spinor AOT エントリーポイント ----------------------------------- */
/* hello.spin から --emit-c で生成された C コード (hello.c) の main 関数。
 * freestanding ビルドでは `main` に特別な意味はなく、通常の関数として
 * kmain から呼び出す (Issue #75)。 */
extern int main(void);

/* ---- テストヘルパー -------------------------------------------------- */

static int test_failures = 0;

static void check(int cond, const char *name)
{
    serial_write(cond ? "  [OK]   " : "  [FAIL] ");
    serial_write(name);
    serial_write("\n");
    if (!cond) {
        test_failures++;
    }
}

/* ---- PMM テスト ------------------------------------------------------ */

static void test_pmm(void)
{
    serial_write("test: page frame allocator\n");

    uint64_t free_before = pmm_free_pages();

    /* 1. 2 ページ確保してパターンを書き込み、読み戻す */
    uint8_t *p1 = pmm_alloc_page();
    uint8_t *p2 = pmm_alloc_page();
    check(p1 != NULL && p2 != NULL, "alloc two pages");
    check(p1 != p2, "pages are distinct");

    memset(p1, 0xA5, PAGE_SIZE);
    memset(p2, 0x5A, PAGE_SIZE);
    int ok1 = 1, ok2 = 1;
    for (size_t i = 0; i < PAGE_SIZE; i++) {
        if (p1[i] != 0xA5) { ok1 = 0; break; }
    }
    for (size_t i = 0; i < PAGE_SIZE; i++) {
        if (p2[i] != 0x5A) { ok2 = 0; break; }
    }
    check(ok1 && ok2, "page write/read pattern");

    /* 2. 解放して再確保 → スタック (LIFO) なので直前のページが再利用される */
    pmm_free_page(p2);
    uint8_t *p3 = pmm_alloc_page();
    check(p3 == p2, "freed page is reused (LIFO)");

    pmm_free_page(p3);
    pmm_free_page(p1);
    check(pmm_free_pages() == free_before, "free page count restored");
}

/* ---- kmalloc テスト --------------------------------------------------- */

static void test_kmalloc(void)
{
    serial_write("test: kmalloc / kfree\n");

    /* 1. 大小さまざまなサイズを確保して読み書き */
    uint8_t  *a = kmalloc(8);
    uint8_t  *b = kmalloc(64);
    uint8_t  *c = kmalloc(1000);
    uint32_t *d = kmalloc(100 * 1024);   /* 100 KiB */
    check(a && b && c && d, "kmalloc various sizes (8B..100KiB)");

    /* 16 バイトアラインの確認 */
    check(((uintptr_t)a % 16 == 0) && ((uintptr_t)b % 16 == 0) &&
          ((uintptr_t)c % 16 == 0) && ((uintptr_t)d % 16 == 0),
          "returned pointers are 16-byte aligned");

    memset(a, 0x11, 8);
    memset(b, 0x22, 64);
    memset(c, 0x33, 1000);
    for (size_t i = 0; i < (100 * 1024) / 4; i++) {
        d[i] = (uint32_t)i * 2654435761u;   /* Knuth hash パターン */
    }

    int ok = 1;
    for (size_t i = 0; i < 8; i++)    { if (a[i] != 0x11) { ok = 0; } }
    for (size_t i = 0; i < 64; i++)   { if (b[i] != 0x22) { ok = 0; } }
    for (size_t i = 0; i < 1000; i++) { if (c[i] != 0x33) { ok = 0; } }
    for (size_t i = 0; i < (100 * 1024) / 4; i++) {
        if (d[i] != (uint32_t)i * 2654435761u) { ok = 0; break; }
    }
    check(ok, "write/read patterns survive across blocks");

    /* 2. 解放 → 再確保でメモリが再利用される */
    kfree(b);
    void *b2 = kmalloc(64);
    check(b2 != NULL, "realloc after free succeeds");

    /* 3. 全て解放し、結合 (coalescing) 後に大きな連続確保が通る */
    kfree(a);
    kfree(b2);
    kfree(c);
    kfree(d);
    void *big = kmalloc(3 * 1024 * 1024);   /* 3 MiB (4 MiB ヒープの大半) */
    check(big != NULL, "3 MiB alloc after coalescing");
    kfree(big);

    /* 4. ヒープ超過の要求は NULL */
    void *toobig = kmalloc(64 * 1024 * 1024);   /* 64 MiB > 4 MiB heap */
    check(toobig == NULL, "oversized alloc returns NULL");
}

/* ---- エントリーポイント --------------------------------------------- */

void kmain(void)
{
    /* ブートローダが要求リビジョンを満たさない場合は即停止 */
    if (LIMINE_BASE_REVISION_SUPPORTED == false) {
        hcf();
    }

    serial_init();
    serial_write("Hello, Linear Spinor\n");

    /* Phase R1-2: 物理メモリ管理の初期化 */
    if (pmm_init() != 0) {
        serial_write("FATAL: memory init failed\n");
        hcf();
    }

    /* 検証テスト */
    test_pmm();
    test_kmalloc();

    if (test_failures == 0) {
        serial_write("Memory allocator initialized and tested\n");
    } else {
        serial_write("MEMORY TESTS FAILED: ");
        serial_write_u64((uint64_t)test_failures);
        serial_write(" failure(s)\n");
        hcf();   /* メモリが壊れている状態で Spinor を走らせない */
    }

    /* Phase R1-3 (Issue #75): Spinor AOT コンパイル済みプログラムの実行 */
    serial_write("--- Spinor AOT program (hello.spin) ---\n");
    main();
    serial_write("--- Spinor AOT program finished ---\n");

    /* フレームバッファが提供されていれば白い対角線を描画 (生存確認) */
    if (framebuffer_request.response != NULL
        && framebuffer_request.response->framebuffer_count >= 1) {
        struct limine_framebuffer *fb =
            framebuffer_request.response->framebuffers[0];
        volatile uint32_t *pixels = fb->address;
        for (size_t i = 0; i < 100; i++) {
            pixels[i * (fb->pitch / 4) + i] = 0x00FFFFFF;
        }
    }

    hcf();
}
