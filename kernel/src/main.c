/* main.c — Spinor OS カーネルエントリーポイント (Phase R1-1 / Issue #73)
 *
 * Limine ブートプロトコル (base revision 3) に準拠した最小スタブカーネル。
 * 標準 C ライブラリ非依存 (freestanding)。
 *
 *   1. Limine リクエストヘッダ (.limine_requests セクション) を配置
 *   2. エントリーポイント kmain: シリアル (COM1) へ挨拶を出力
 *   3. おまけ: フレームバッファが貰えていれば対角線を描画 (GUI 起動時の生存確認)
 *   4. hlt 無限ループで停止
 */
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#include <limine.h>

#include "serial.h"

/* ---- Limine リクエスト --------------------------------------------- */

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

/* ---- エントリーポイント --------------------------------------------- */

void kmain(void)
{
    /* ブートローダが要求リビジョンを満たさない場合は即停止 */
    if (LIMINE_BASE_REVISION_SUPPORTED == false) {
        hcf();
    }

    serial_init();
    serial_write("Hello, Linear Spinor\n");

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
