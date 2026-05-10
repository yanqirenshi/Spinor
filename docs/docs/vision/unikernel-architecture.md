# Unikernel Architecture: AOT C 出力をベアメタルで動かす戦略

[Issue #44](https://github.com/yanqirenshi/Spinor/issues/44) の調査ログ。
[The Ultimate Dream](./the-ultimate-dream.md) で掲げた「OS全体がひとつの巨大な REPL」を実現するための、最初の足場となる技術調査。

## 目的とアプローチ

Spinor の長期目標は、Lisp で書いた処理系がベアメタルで直接起動する Lisp マシンを再構築することにある。フルスタック (Hindley-Milner 型推論器、評価器、GHC ランタイム) をいきなり Unikernel 化することは現実的ではないため、**段階的な PoC** を以下の順序で構築する戦略を採る。

| Phase | スコープ | 入力 | 出力 |
|---|---|---|---|
| **Phase 1 (本書のスコープ)** | AOT C 出力を Unikernel 化 | `.spin` → `cabal run spinor -- build` で生成される `output.c` + `runtime/spinor.c` | Unikraft でビルドされた KVM/Xen ゲストイメージ |
| Phase 2 | LLVM IR 経由ルートも対応 | `cabal run spinor -- build-llvm` の出力 | 同上 (Clang ターゲット切替) |
| Phase 3 | REPL 本体 (GHC ランタイム) を Unikernel 化 | `cabal run spinor` バイナリ | HaLVM 系 / GHC-on-Unikraft の調査が必要 |

ホスト OS 不要のパスを最小手数で開通させることが最優先で、Phase 1 では「AOT 出力されたゲストイメージが QEMU/KVM 上で `Hello, World!` を表示する」ところまでをマイルストーンとする。

### なぜ Unikraft か

選定候補と判断:

| 候補 | 採用可否 | 理由 |
|---|---|---|
| **Unikraft** | ◎ 採用 | musl ベースの POSIX libc が利用可能。`stdio.h`/`stdlib.h`/`string.h` への依存を改造なしで吸収できる。ビルドシステム (`kraft`) が成熟。KVM/Xen/Firecracker に対応。 |
| MirageOS | △ | OCaml 前提。C コードのリンクは可能だが Spinor の AOT パスとの親和性が低い。 |
| IncludeOS | △ | C++ 中心。メンテナンスがやや停滞気味。 |
| HermitCore / Hermit | ○ 第二候補 | Rust ベース。C アプリのリンクも可能。Unikraft で詰まった場合の代替。 |
| 自作 (bare metal + multiboot) | × (将来) | Phase 3 以降の楽しみとして温存。 |

## 現状の C 生成コードの依存関係分析

### libc 依存リスト

`src/Spinor/Compiler/Codegen.hs` および `runtime/spinor.h` / `runtime/spinor.c` を実調査して列挙したシンボル一覧。

| ヘッダ | 使用シンボル | 用途 | Unikraft 対応 |
|---|---|---|---|
| `<stdio.h>` | `printf`, `fprintf`, `fopen`, `fclose`, `fread`, `fputs`, `fseek`, `ftell` | プリミティブ `print`、ファイル I/O | ✅ musl で完全提供。`stdout` は Unikraft の console driver にリダイレクトされる |
| `<stdlib.h>` | `malloc`, `free`, `exit` | オブジェクト確保、エラー終了 | ✅ Unikraft の `ukalloc` (デフォルト: bump allocator もしくは TLSF) が backend |
| `<stdbool.h>` | `bool`, `true`, `false` | 真偽値 | ✅ ヘッダのみ |
| `<string.h>` | `strlen`, `strcpy`, `strcat`, `strncpy`, `strdup`, `strcmp`, `memcpy` | 文字列プリミティブ | ✅ musl で完全提供 |
| `"spinor.h"` | (Spinor 独自) | Tagged Union / コンストラクタ | ✅ `runtime/spinor.c` を一緒にビルドするだけ |

**結論:** 現状の AOT 出力は **POSIX libc のごく薄いサブセット** にしか依存しておらず、Unikraft の musl libc レイヤで完全に充足できる。GLibc 固有の拡張も使っていない。

### メモリ管理 (GC 不在問題)

`runtime/spinor.c` は **GC を持たない**。すべてのコンストラクタ (`sp_make_int` など) が `malloc()` のみを呼び、`free()` を呼ばない。

```c
SpObject* sp_make_int(long value) {
    SpObject* obj = (SpObject*)malloc(sizeof(SpObject));  // ← leak
    if (!obj) { fprintf(stderr, "Spinor: out of memory\n"); exit(1); }
    obj->type = SP_INT;
    obj->value.integer = value;
    return obj;
}
```

実験的な代替パスとして `Spinor.BorrowCheck` (所有権ベース drop) と `Spinor.EscapeAnalysis` + Arena アロケータ (`compileProgramWithRegions`) が存在するが、デフォルトの `compileProgram` は素の malloc-leak である。

#### Unikernel 観点でのリスク

- **短時間の PoC (Hello World OS) では問題にならない:** プロセスが秒〜分単位で終わるため、leak はメモリ枯渇までいかない。
- **長期稼働するサービス化は不可:** Unikraft デフォルトのヒープ (`CONFIG_LIBUKALLOC_IFMALLOC` + bump allocator) は `free` されない領域を再利用できず、ヒープを使い果たすとアロケーション失敗で `exit(1)` に到達する。
- **対策の優先順位:**
  1. Phase 1 PoC では現状維持 (leak 容認)。Unikraft のヒープサイズ (`CONFIG_LIBUKBOOT_HEAPSZ`) を多めに割り当てて短時間動作を担保。
  2. Phase 2 で `compileProgramWithRegions` をデフォルト経路化し、`with-region` ブロックで一括解放する arena 方式に移行する。Issue #?? として別途起票。
  3. Phase 3 で Boehm GC ライクな保守的 GC を `runtime/` に組み込むか、線形型ベースの正確な GC を導入する。

### main 関数のシグネチャ

`compileProgram` (Codegen.hs:65) が生成する main は:

```c
int main(void) {
    /* mainStmts */
    return 0;
}
```

これは Unikraft の `LIBUKBOOT_MAIN_FN` (デフォルトで `main`) と完全に互換。`__libc_start_main` を Unikraft が用意するためエントリ調整は不要。

## PoC: "Hello World OS" 構築フロー

「Spinor で書いた `(print "Hello, World!")` を Unikraft でビルドし、KVM ゲストとして起動して console に表示する」までの手順。

### 0. ホスト前提

- Linux ホスト (Ubuntu 22.04+ / WSL2 でも可)
- `qemu-system-x86_64`, `kvm`, `make`, `gcc`, `clang`, `flex`, `bison`, `libelf-dev`, `libncurses-dev`
- Unikraft toolchain: `kraftkit` (`kraft` CLI)

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get.kraftkit.sh | sh
```

### 1. Spinor で AOT C コードを生成

```bash
echo '(print "Hello, World!")' > hello.spin
cabal run spinor -- build hello.spin
```

ただしこのコマンドは **直接バイナリを作りに行く**。Unikernel 化のためには C ソースだけ取り出したいので、ビルドロジックを分割する必要がある。

**改修ポイント (別 Issue 化が望ましい):** `app/Main.hs:430` 付近の `removeFile cFile` を `--keep-c` フラグでスキップできるようにする。または新サブコマンド `cabal run spinor -- emit-c hello.spin` を追加する。

暫定回避策として、`app/Main.hs` の該当行をローカルで一時的にコメントアウトすればよい。

成果物:
- `hello.c` — AOT 生成コード
- `runtime/spinor.h`, `runtime/spinor.c` — ランタイム (リポジトリ既存)

### 2. Unikraft アプリケーションのスケルトン作成

```bash
mkdir -p ~/spinor-unikraft/app-hello
cd ~/spinor-unikraft/app-hello
kraft init --plat kvm --arch x86_64
```

ディレクトリ構造:

```
app-hello/
├── Kraftfile          # アプリ定義 (YAML)
├── Makefile.uk        # ビルドルール
├── Config.uk          # Kconfig 設定
└── src/
    ├── hello.c        # ↑で生成
    ├── spinor.c       # runtime/spinor.c をコピー
    └── spinor.h       # runtime/spinor.h をコピー
```

### 3. `Makefile.uk` の記述

```makefile
$(eval $(call addlib_s,apphello,$(CONFIG_APPHELLO)))

APPHELLO_SRCS-y += $(APPHELLO_BASE)/src/hello.c
APPHELLO_SRCS-y += $(APPHELLO_BASE)/src/spinor.c

APPHELLO_CINCLUDES-y += -I$(APPHELLO_BASE)/src
APPHELLO_CFLAGS-y    += -Wno-unused-parameter
```

### 4. `Kraftfile` の記述

```yaml
specification: v0.6
name: spinor-hello
unikraft: stable
libraries:
  musl: stable
targets:
  - architecture: x86_64
    platform: kvm
```

`musl` を依存に入れることで `string.h`/`stdio.h`/`stdlib.h` 系が解決される。

### 5. ビルドと起動

```bash
kraft build --target spinor-hello-kvm-x86_64
kraft run --plat kvm --arch x86_64 spinor-hello
```

期待される出力:

```
SeaBIOS (version ...)
Booting from ROM...
Powered by Unikraft
Hello, World!
```

### 6. 検証チェックリスト

- [ ] `kraft build` が成功する (`undefined reference` が出ない = libc 依存が満たされている)
- [ ] `kraft run` でゲスト起動 → console に `Hello, World!` が表示される
- [ ] `exit(0)` 後にゲストがハングしない (Unikraft は `main` から戻ると halt するのが正常)
- [ ] イメージサイズを記録 (理論上 数 MB 程度に収まるはず)

## 将来の展望: REPL 本体 (GHC ランタイム) の Unikernel 化

Phase 3 の前提整理。**現時点では実装スコープ外**だが、設計判断のために課題を列挙しておく。

### 技術的課題

1. **GHC ランタイムの巨大さ**
   - Spinor 本体は Haskell (GHC 9.6+) で実装されており、stripped でも数十 MB のバイナリになる。
   - GHC RTS は POSIX threads, signals, mmap, pthread_key_create など、AOT 出力よりはるかに広い OS インターフェースに依存する。

2. **HaLVM の現状**
   - かつて Galois が開発した [HaLVM](https://github.com/GaloisInc/HaLVM) は Haskell プログラムを Xen 上の Unikernel にする処理系だったが、メンテナンスが停滞 (最終リリース 2017 年頃)。
   - 復活させるかどうかは、「GHC を fork してメンテナンスする覚悟があるか」が判断軸。

3. **GHC-on-Unikraft の選択肢**
   - Unikraft が musl + pthreads を提供するため、**理論上は標準 GHC バイナリをそのまま動かせる可能性**がある。ただし実例は確認できておらず、PoC が必要。
   - GHC のスタティックリンクモード (`-static`) で生成した ELF を Unikraft にかぶせる戦略。

4. **動的コード生成 (REPL の真髄)**
   - REPL は実行時に Haskell コードを評価する必要があるため、JIT もしくはバイトコードインタプリタが必要。
   - GHCi のバイトコードインタプリタを Unikernel に持ち込めるか? → 動的リンカ (`ghc-iserv`) の依存解決が課題。

5. **GC の協調**
   - GHC RTS の GC とゲスト OS のメモリ管理 (Unikraft の `ukalloc`) は別物。GHC の GC は自前のヒープを `mmap` で確保するため、Unikraft の `mmap` 実装の完成度に依存する。

### 段階的アプローチの提案

```
Phase 3a: GHC binary を Unikraft で動かす PoC (Hello World)
   └── 動かなければ HaLVM 路線へ pivot
Phase 3b: spinor バイナリ (REPL) を Unikraft で起動
   └── 標準入力からの読み込みは virtio-console 経由
Phase 3c: ファイルシステム連携 (twister/ ライブラリのロード)
   └── Unikraft の 9pfs / initrd を活用
Phase 3d: ネットワーク (TCP/IP REPL)
   └── lwIP 統合
Phase 3e: Self-hosting に向けた Spinor → ネイティブコンパイラの本格化
   └── Phase 1/2 が成熟して REPL を Spinor 自身で書き直せるようになれば、
       GHC 依存からの脱却が現実視野に入る
```

### 代替戦略: Spinor 自身による Self-Hosting

最も Lisp マシンの哲学に忠実な道は、**GHC 依存を捨てて Spinor 自身で REPL を書き直す** ことである。これは [The Ultimate Dream](./the-ultimate-dream.md) の核心でもある。

その前提として:
- Spinor の AOT コンパイラが「自分自身をコンパイルできる」レベルまで成熟する
- メモリ管理 (GC) が本格化する
- マクロ系 / 評価器 / 型推論器を Spinor で再実装する

これらが揃った時点で初めて、純粋な Lisp マシンとしての Spinor OS が立ち上がる。Phase 1 はその長い旅の **最初の一歩** に過ぎない。

## 次のアクション

- [ ] 本書のレビュー (Architect: Gemini CLI)
- [ ] `spinor build --emit-c` フラグ or `spinor emit-c` サブコマンド追加 Issue を起票
- [ ] Unikraft ホスト環境セットアップ手順を `docs/installation/` 配下に新設するか検討
- [ ] Phase 1 PoC リポジトリ (`spinor-unikraft-hello`) のひな型作成 Issue を起票

## 参考資料

- [Unikraft Documentation](https://unikraft.org/docs/)
- [HaLVM (archived)](https://github.com/GaloisInc/HaLVM)
- [The Ultimate Dream](./the-ultimate-dream.md)
- 内部資料: `src/Spinor/Compiler/Codegen.hs`, `runtime/spinor.{h,c}`, `app/Main.hs:403`
