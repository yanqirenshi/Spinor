# Makefile — Spinor OS (Phase R1-1 / Issue #73)
#
# ターゲット:
#   make          … カーネルをビルドし、ブート可能 ISO (spinor-os.iso) を生成
#   make run      … QEMU をヘッドレス起動 (シリアル出力を端末に表示)
#   make run-gui  … QEMU をウィンドウ表示で起動 (WSLg / Linux デスクトップ)
#   make clean    … ビルド成果物を削除
#   make distclean… clean + 取得済み Limine も削除
#
# 前提 (WSL Ubuntu / Linux):
#   gcc make git xorriso qemu-system-x86_64

KERNEL := bin/spinor-kernel
ISO    := spinor-os.iso

CC := cc
LD := ld

# Limine 公式バイナリブランチ (v8 系。limine.conf 構文)
LIMINE_BRANCH := v8.x-binary
LIMINE_REPO   := https://github.com/limine-bootloader/limine.git
LIMINE_DIR    := limine

# Spinor コンパイラ (AOT C 出力用)。Windows ホストの cabal ビルド成果物を
# WSL の interop で直接叩く。存在しない場合はコミット済み hello.c を使う。
SPINOR ?= ../dist-newstyle/build/x86_64-windows/ghc-9.6.7/spinor-0.1.0.0/x/spinor/build/spinor/spinor.exe

# フリースタンディング + カーネル向けコード生成フラグ (Limine テンプレート準拠)
# -I src/shim を先頭に置き、生成コードの <stdio.h> / "spinor.h" を
# ベアメタル用スタブ・ヘッダに解決させる (Issue #75)
CFLAGS := -Wall -Wextra -std=gnu11 \
          -ffreestanding -fno-stack-protector -fno-stack-check -fno-lto -fno-PIC \
          -m64 -march=x86-64 -mno-80387 -mno-mmx -mno-sse -mno-sse2 -mno-red-zone \
          -mcmodel=kernel \
          -I src/shim -I src -I $(LIMINE_DIR)

LDFLAGS := -m elf_x86_64 -nostdlib -static -z max-page-size=0x1000 -T linker.ld

SRCS := $(wildcard src/*.c)
OBJS := $(patsubst src/%.c,obj/%.o,$(SRCS)) obj/hello.o

.PHONY: all iso run run-gui clean distclean

all: $(ISO)

iso: $(ISO)

# ---- Limine の取得とホストツールのビルド --------------------------------

$(LIMINE_DIR)/limine.h:
	git clone $(LIMINE_REPO) --branch=$(LIMINE_BRANCH) --depth=1 $(LIMINE_DIR)

$(LIMINE_DIR)/limine: $(LIMINE_DIR)/limine.h
	$(MAKE) -C $(LIMINE_DIR)

# ---- Spinor AOT C コードの生成 (Issue #75) --------------------------------
# spinor コンパイラがあれば hello.spin から再生成する。
# 無い環境 (CI 等) ではコミット済みの hello.c をそのまま使う。

hello.c: hello.spin
	@if [ -x "$(SPINOR)" ]; then \
	    echo "SPINOR  build hello.spin --emit-c"; \
	    "$(SPINOR)" build hello.spin --emit-c; \
	else \
	    echo "warning: spinor compiler not found ($(SPINOR)); using committed hello.c"; \
	    touch hello.c; \
	fi

obj/hello.o: hello.c $(LIMINE_DIR)/limine.h
	@mkdir -p obj
	$(CC) $(CFLAGS) -c $< -o $@

# ---- カーネルのビルド -----------------------------------------------------

obj/%.o: src/%.c $(LIMINE_DIR)/limine.h
	@mkdir -p obj
	$(CC) $(CFLAGS) -c $< -o $@

$(KERNEL): $(OBJS) linker.ld
	@mkdir -p bin
	$(LD) $(OBJS) $(LDFLAGS) -o $@

# ---- ISO イメージ生成 -----------------------------------------------------

$(ISO): $(KERNEL) $(LIMINE_DIR)/limine limine.conf
	rm -rf iso_root
	mkdir -p iso_root/boot/limine iso_root/EFI/BOOT
	cp $(KERNEL) iso_root/boot/
	cp limine.conf \
	   $(LIMINE_DIR)/limine-bios.sys \
	   $(LIMINE_DIR)/limine-bios-cd.bin \
	   $(LIMINE_DIR)/limine-uefi-cd.bin \
	   iso_root/boot/limine/
	cp $(LIMINE_DIR)/BOOTX64.EFI $(LIMINE_DIR)/BOOTIA32.EFI iso_root/EFI/BOOT/
	xorriso -as mkisofs \
	    -b boot/limine/limine-bios-cd.bin -no-emul-boot \
	    -boot-load-size 4 -boot-info-table \
	    --efi-boot boot/limine/limine-uefi-cd.bin -efi-boot-part \
	    --efi-boot-image --protective-msdos-label \
	    iso_root -o $(ISO)
	$(LIMINE_DIR)/limine bios-install $(ISO)

# ---- 実行 -----------------------------------------------------------------

run: $(ISO)
	qemu-system-x86_64 -M q35 -m 512M -cdrom $(ISO) -display none -serial stdio

run-gui: $(ISO)
	qemu-system-x86_64 -M q35 -m 512M -cdrom $(ISO) -serial stdio

# ---- 掃除 -----------------------------------------------------------------

clean:
	rm -rf obj bin iso_root $(ISO)

distclean: clean
	rm -rf $(LIMINE_DIR)
