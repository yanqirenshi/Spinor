/* linker.ld — Spinor OS x86-64 ELF カーネル用リンカスクリプト (Issue #73)
 *
 * Limine C テンプレート準拠。higher half (0xffffffff80000000) に配置する。
 */
OUTPUT_FORMAT(elf64-x86-64)

ENTRY(kmain)

PHDRS
{
    limine_requests PT_LOAD;
    text            PT_LOAD;
    rodata          PT_LOAD;
    data            PT_LOAD;
}

SECTIONS
{
    /* higher half の先頭 2GiB (-mcmodel=kernel と対応) */
    . = 0xffffffff80000000;

    /* Limine リクエスト領域 (マーカで囲む) */
    .limine_requests : {
        KEEP(*(.limine_requests_start))
        KEEP(*(.limine_requests))
        KEEP(*(.limine_requests_end))
    } :limine_requests

    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .text : {
        *(.text .text.*)
    } :text

    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .rodata : {
        *(.rodata .rodata.*)
    } :rodata

    . = ALIGN(CONSTANT(MAXPAGESIZE));

    .data : {
        *(.data .data.*)
    } :data

    .bss : {
        *(.bss .bss.*)
        *(COMMON)
    } :data

    /DISCARD/ : {
        *(.eh_frame*)
        *(.note .note.*)
    }
}
