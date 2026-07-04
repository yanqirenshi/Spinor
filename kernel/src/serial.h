/* serial.h — COM1 (16550 UART) minimal serial driver for Spinor OS.
 *
 * Phase R1-1 (Issue #73): freestanding, no libc.
 */
#ifndef SPINOR_SERIAL_H
#define SPINOR_SERIAL_H

/* COM1 を 115200bps / 8N1 で初期化する */
void serial_init(void);

/* 1 文字送信 (送信バッファ空きを busy-wait) */
void serial_putc(char c);

/* NUL 終端文字列を送信 ('\n' は CRLF に変換) */
void serial_write(const char *s);

#endif /* SPINOR_SERIAL_H */
