/* serial.c — COM1 (16550 UART) minimal serial driver for Spinor OS.
 *
 * 標準 C ライブラリ非依存。ポート I/O (in/out 命令) のみで実装する。
 */
#include <stdint.h>

#include "serial.h"

#define COM1_PORT 0x3F8

/* ---- ポート I/O プリミティブ -------------------------------------- */

static inline void outb(uint16_t port, uint8_t val)
{
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline uint8_t inb(uint16_t port)
{
    uint8_t ret;
    __asm__ volatile ("inb %1, %0" : "=a"(ret) : "Nd"(port));
    return ret;
}

/* ---- 16550 UART 初期化 -------------------------------------------- */

void serial_init(void)
{
    outb(COM1_PORT + 1, 0x00);  /* 割り込み無効化                       */
    outb(COM1_PORT + 3, 0x80);  /* DLAB セット (分周比レジスタへ切替)   */
    outb(COM1_PORT + 0, 0x01);  /* 分周比 lo = 1 (115200 baud)          */
    outb(COM1_PORT + 1, 0x00);  /* 分周比 hi = 0                        */
    outb(COM1_PORT + 3, 0x03);  /* 8bit / パリティなし / stop 1, DLAB 解除 */
    outb(COM1_PORT + 2, 0xC7);  /* FIFO 有効化・クリア (14 byte 閾値)   */
    outb(COM1_PORT + 4, 0x0B);  /* DTR + RTS + OUT2                     */
}

/* ---- 送信 ----------------------------------------------------------- */

static int transmit_empty(void)
{
    return inb(COM1_PORT + 5) & 0x20;   /* LSR bit5 = THR empty */
}

void serial_putc(char c)
{
    while (!transmit_empty()) {
        /* busy wait */
    }
    outb(COM1_PORT, (uint8_t)c);
}

void serial_write(const char *s)
{
    while (*s) {
        if (*s == '\n') {
            serial_putc('\r');
        }
        serial_putc(*s++);
    }
}
