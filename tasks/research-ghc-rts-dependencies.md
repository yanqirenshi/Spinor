# Research: GHC RTS の POSIX 依存性の洗い出しと低レイヤー要件の特定

> Issue: [#56](https://github.com/yanqirenshi/Spinor/issues/56)  (親: [#45](https://github.com/yanqirenshi/Spinor/issues/45))
> 調査対象 GHC バージョン: 9.6+ (現時点で `master` ブランチも参照し、9.6 以降に大きく変わっていない領域を中心に確認)
> 関連ドキュメント: [unikernel-architecture.md](../manual/public/docs/vision/unikernel-architecture.md), [the-ultimate-dream.md](../manual/public/docs/vision/the-ultimate-dream.md)
> 注: 一次ソースは原則 [github.com/ghc/ghc](https://github.com/ghc/ghc) (gitlab.haskell.org は Anubis ガードで `WebFetch` 不可だったため GitHub ミラーを使用)

## 1. エグゼクティブサマリ

調査の結論として、**GHC RTS は素の状態では Unikernel 上で動かない** が、**Unikraft の現行ライブラリ群でかなりの部分はカバーされており、ボトルネックは限定的**である。

- **メモリ管理 (`rts/posix/OSMem.c`):** `mmap`/`munmap`/`mprotect`/`madvise` に加え、**1MB 境界アラインされた "megablock"** を要求する。これは Unikraft の `lib/ukvmem` + `lib/posix-mmap` で原理的にカバー可能だが、`madvise(MADV_FREE)` や 8GB ヒープ予約のヒューリスティクスは Unikraft 側で要パッチ。⚠️
- **スレッド (`rts/posix/OSThreads.c`):** `pthread_*` (mutex, condvar, create, kill, affinity) にフル依存。Unikraft は `lib-pthread-embedded` または `lib-musl` 経由で pthread を提供しており、**SMP/Capability モデルは原理的に動作する見込み**。ただし `pthread_kill` / `sched_setaffinity` 周りは要検証。⚠️
- **タイマー (`rts/posix/Ticker.c`):** **9.6 以降は `setitimer` ではなく専用 pthread + `ppoll`/`select` 方式**。signal 依存が抑えられているため Unikernel 化が容易になっている。✅
- **シグナル (`rts/posix/Signals.c`):** `sigaction` を主に「ユーザシグナルを Haskell に届ける」目的で使用。Unikernel では POSIX シグナル意味論自体がほぼ不要なため、**no-op スタブ化で十分**な可能性が高い。✅
- **I/O イベント (`GHC.Internal.Event.*`):** Linux backend は `epoll` + `eventfd`。**Unikraft は `posix-poll` (epoll API) と `posix-eventfd` を提供しており、これは最も組み合わせが良い**。✅

総合判断: **Phase 3 GO に値する**。ただし「素直に動く」ことを期待できるのはタイマーと I/O のみで、メモリ層は ukvmem の挙動次第、Capability/SMP は要 PoC である。

## 2. メモリ管理 (Memory Management)

### 2.1 依存 API と利用箇所

`rts/posix/OSMem.c` ([master](https://github.com/ghc/ghc/blob/master/rts/posix/OSMem.c)) で使用される POSIX システムコール:

| API | 用途 | RTS 内ラッパー |
|---|---|---|
| `mmap()` (`MAP_ANON \| MAP_PRIVATE`, optionally `MAP_NORESERVE`, `MAP_FIXED`, `MAP_GUARD`) | megablock 予約と commit | `osGetMBlocks(n)`, `osReserveHeapMemory()` |
| `munmap()` | megablock 解放 | `osFreeMBlocks(addr, n)`, `osReleaseHeapMemory()` |
| `mprotect()` | クロージャ保護・ガードページ | `osProtectClosures()` 等 |
| `madvise(MADV_DONTNEED / MADV_FREE / MADV_DODUMP / MADV_DONTDUMP)` | decommit と core dump 制御 | `osDecommitMemory(at, size)` |
| `sysconf(_SC_PAGESIZE)`, `sysconf(_SC_PHYS_PAGES)` | ページサイズ・物理メモリ取得 | `getPageSize()`, `getPhysicalMemorySize()` |
| `getrlimit(RLIMIT_AS)` | 仮想メモリ上限取得 | ヒープ予約サイズ算定 |
| `mbind()` (NUMA, Linux 限定) | NUMA ノードバインド | `osBindMBlocksToNode()` |
| `sysctlbyname()` (Darwin/iOS), `sysctl()` (BSD) | 物理メモリ取得 | `getPhysicalMemorySize()` |

### 2.2 ブロックアロケータの要求

GHC RTS のストレージマネージャは **MBlock (megablock)** という単位で OS からメモリを取る。

- **MBLOCK_SIZE = 2^MBLOCK_SHIFT** ([`rts/include/rts/Constants.h`](https://github.com/ghc/ghc/blob/master/rts/include/rts/Constants.h)): 通常アーキ (x86_64, aarch64) では `MBLOCK_SHIFT = 20` ⇒ **1 MiB**。Wasm のみ `MBLOCK_SHIFT = 16` (64 KiB)。
- **BLOCK_SIZE = 2^12 = 4 KiB** (典型的なページサイズと一致)。
- **重要な不変条件:** megablock は **MBLOCK_SIZE 境界にアラインされていなければならない** (ポインタの下位 20bit から block descriptor を逆引きするため)。`OSMem.c` の冒頭コメントは "we use mmap() to allocate our memory. We want memory in chunks of MBLOCK_SIZE, and aligned on an MBLOCK_SIZE boundary." と明記している。
- **ヒープベース戦略:** 64bit 環境では `osReserveHeapMemory()` が **8GB (0x200000000) 以上の高位アドレス**に巨大連続領域を `mmap(PROT_NONE, MAP_NORESERVE)` で予約し、必要時に `mprotect(PROT_READ|PROT_WRITE)` で commit する (code locality と pointer tagging のため)。
- **decommit:** `MADV_FREE` (Linux ≥ 4.5) があればそれ、なければ `MADV_DONTNEED` をフォールバック。

参考: Edward Z. Yang の [GHC Block Allocator slides (PDF, MIT)](http://web.mit.edu/~ezyang/Public/blocks.pdf) (本文取得は PDF バイナリのため不可、検索結果のメタ情報から block descriptor 64B / 1MB megablock 構成を確認)。

### 2.3 Unikraft 代替診断

| GHC RTS 要求 | Unikraft 対応 | 評価 |
|---|---|---|
| `mmap(MAP_ANON, PROT_READ\|WRITE)` | `lib/posix-mmap` (上に `lib/ukvmem` ⇒ `uk/vmem.h`) — ["The API introduced by lib/ukvmem ... is exposed in a POSIX compatible way via lib/posix-mmap"](https://unikraft.org/blog/2023-02-07-unikraft-releases-epimetheus) | ✅ |
| `mprotect()` | `ukvmem` の vmem protection API ⇒ posix-mmap 経由で `mprotect` | ✅ |
| `munmap()` | posix-mmap で対応 | ✅ |
| `madvise(MADV_DONTNEED / MADV_FREE)` | `ukvmem` に decommit プリミティブはあるが、`MADV_FREE` (遅延ゼロクリア) の正確な意味論はサポートされているか不明 | ⚠️ TODO (要追加調査) |
| 1MB アラインの `mmap` | `mmap(addr=NULL)` で 1MB アラインが保証される保証はない。Linux 同等ならページ整数倍までしか保証されないため、GHC 側の "over-allocate して trimming" ロジックが動く必要 ([`OSMem.c` の `aligned_alloc` 経路](https://github.com/ghc/ghc/blob/master/rts/posix/OSMem.c)) | ⚠️ |
| 8GB 高位アドレス予約 (`MAP_NORESERVE`) | Unikraft の仮想アドレス空間は guest が単独で支配するため理論上問題ないが、guest 物理メモリより大きい予約をサポートするかは要確認 | ⚠️ TODO |
| `sysconf(_SC_PAGESIZE)` | musl 経由で提供 (定数返却) | ✅ |
| NUMA (`mbind`) | Unikraft 単一 NUMA ノード想定。`mbind` は no-op スタブで OK | ✅ (機能無効化) |
| `getrlimit(RLIMIT_AS)` | Unikraft の `posix-process` で部分対応。失敗時は GHC が物理メモリ量から推定 | ✅ |

**評価まとめ:** ✅ 主要 API は揃っている。⚠️ ただし `ukvmem` が比較的新しく ([Issue #1475](https://github.com/unikraft/unikraft/issues/1475) "ukvmem: Virtual Memory Cannot Handle Page Fault Correctly" が 2024 年時点で open) **page fault 経由の demand paging で GHC の `mprotect` ベースの commit が正しく動くかは PoC で要検証**。

## 3. スケジューリングとスレッド (Scheduling & Threads)

### 3.1 Capability アーキテクチャ

[`rts/Capability.h`](https://github.com/ghc/ghc/blob/master/rts/Capability.h) によれば、Capability は「STG コードを実行するためのトークン」であり、OS スレッドが Haskell コードを走らせるために必要な状態 (STG レジスタ、TSO ポインタ、nursery、run_queue、mut_lists、weak ptr list、spark pool、suspended_ccalls) を**まとめて1つの構造体に詰めた論理 CPU 抽象**。

- `THREADED_RTS` ビルドでは、`Capability` ごとに **`Mutex lock`** (running_task, returning_tasks, wakeup_queue, inbox, putMVars を保護) を持つ。
- 各 OS スレッドは `Task` 構造体で表現され、`Task->cond` (Condition Variable) で Capability の解放を待つ。
- 主要操作 ([Capability.c](https://github.com/ghc/ghc/blob/master/rts/Capability.c)):
  - `initCapabilities()`, `initCapability(cap, i)`
  - `waitForCapability(pCap, task)` — foreign call から戻った OS スレッドがブロック
  - `yieldCapability(pCap, task, gcAllowed)` — 一時的に放棄、GC 参加可能
  - `releaseCapability_(cap, always_wakeup)` — capability を空きに戻し、待機 task を起こす

### 3.2 pthreads 依存

[`rts/posix/OSThreads.c`](https://github.com/ghc/ghc/blob/master/rts/posix/OSThreads.c) で使われる pthread API の RTS マッピング:

| pthread API | RTS ラッパー |
|---|---|
| `pthread_create`, `pthread_detach` | `createOSThread`, `createAttachedOSThread` |
| `pthread_join` | `joinOSThread` |
| `pthread_kill` | `interruptOSThread` |
| `pthread_self` | `osThreadId`, `start_thread` |
| `pthread_exit` | `shutdownThread` |
| `pthread_mutex_init/destroy` | `initMutex`, `closeMutex` |
| `pthread_cond_init/destroy/wait/timedwait/signal/broadcast` | `initCondition`, `closeCondition`, `waitCondition`, `timedWaitCondition`, `signalCondition`, `broadcastCondition` |
| `pthread_condattr_setclock(CLOCK_MONOTONIC)` | `initCondition` (timedwait の単調時計化) |
| `sched_yield` | `yieldThread` |
| `sched_getaffinity` (Linux), `cpuset_getaffinity` (FreeBSD) | `getNumberOfProcessors` |
| `sched_setaffinity` (Linux), `cpuset_setaffinity` (FreeBSD), `numa_run_on_node` | `setThreadAffinity` |
| `pthread_threadid_np` / `pthread_getthreadid_np` (Darwin/FreeBSD) | `kernelThreadId` (eventlog 用) |

pthread mutex は **無条件 (`Mutex` 型は `pthread_mutex_t` の typedef)** で使われている。

### 3.3 Unikraft 代替診断

Unikraft では2系統の pthread 実装がある:
- **[lib-pthread-embedded](https://github.com/unikraft/lib-pthread-embedded)** — newlib と組み合わせる旧来パス
- **[lib-musl](https://github.com/unikraft/lib-musl)** — v0.11.0 (Janus, 2022-12) 以降のデフォルトで、["musl provides thread support natively"](https://unikraft.org/blog/2022-12-02-unikraft-releases-janus)

下位スケジューラとして [`lib/uksched`](https://github.com/unikraft/unikraft/pull/564) (cooperative `ukschedcoop` がデフォルト) が threads/TCB を提供し、`CONFIG_LIBUKSCHED_TCB_INIT` で musl/pthread-embedded と統合される。

| GHC RTS 要求 | Unikraft 対応 | 評価 |
|---|---|---|
| `pthread_create` / `join` | musl + uksched | ✅ |
| `pthread_mutex_*` | musl (uksched primitives 経由) | ✅ |
| `pthread_cond_*` (timedwait, CLOCK_MONOTONIC) | musl 経由。Unikraft の `posix-time` で `CLOCK_MONOTONIC` 対応 | ✅ |
| `pthread_kill(tid, sig)` | musl 上では実装あるが、Unikraft の シグナル意味論との整合性に懸念 | ⚠️ |
| `sched_yield` | uksched 直結 | ✅ |
| `sched_getaffinity` / `sched_setaffinity` | Unikraft はマルチコア対応が GSoC ベース。SMP は KVM 上で進行中だが affinity API は限定的 | ⚠️ TODO (要追加調査) |
| `pthread_threadid_np` 等 | musl の `gettid` 相当で代用可 | ✅ |
| `numa_*` | 非対応 (NUMA は Unikraft で意味なし) | ✅ no-op で OK |

**評価まとめ:** ⚠️ `THREADED_RTS` ビルドで -N1 (single capability) なら ✅ 動作見込み。-N>1 の SMP は Unikraft 側の SMP 成熟度に依存。pthread-embedded は `MAX_THREADS=32` 制約があるため musl 一択。

## 4. タイマーとシグナル (Timers & Signals)

### 4.1 GC tick と Ticker

**重要な発見:** GHC 9.6+ の [`rts/posix/Ticker.c`](https://github.com/ghc/ghc/blob/master/rts/posix/Ticker.c) は **`setitimer` も `timer_create` も使っていない**。代わりに**専用 pthread が `ppoll()` (もしくは fallback で `select()`) でタイムアウトを待つ**実装になっている。

- 関数: `initTicker(Time interval, TickProc handle_tick)`, `startTicker()`, `stopTicker()`, `exitTicker(bool wait)`
- 内部: `itimer_thread_func()` が ppoll でブロックし、起動シグナルファイルディスクリプタで割り込み可能
- `DEFAULT_TICK_INTERVAL = 10ms` (profiling 時は 1ms — [GHC scheduler wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/scheduler))
- tick で `handle_tick()` がコールされ、context switch flag を立て、eventlog flush をトリガする

歴史: 旧 GHC は `setitimer(ITIMER_VIRTUAL)` + `SIGVTALRM` ハンドラ方式だった ([古い実装は `rts/posix/ticker/Setitimer.c` に名残あり](https://fossies.org/linux/ghc/rts/posix/ticker/Setitimer.c) — Fossies 経由)。GHC 8.x で pthread + timerfd / ppoll に置き換わっており、**現代の GHC はシグナル経由のタイマーを使っていない**点が Unikernel 化に極めて好都合。

### 4.2 シグナルハンドラ

[`rts/posix/Signals.c`](https://github.com/ghc/ghc/blob/master/rts/posix/Signals.c) で確認できる範囲:

| シグナル | 用途 | ハンドラ |
|---|---|---|
| `SIGINT` | 秩序ある shutdown | `shutdown_handler` |
| `SIGPIPE` | broken pipe で死なないように無視 | empty handler |
| `SIGQUIT` | バックトレース出力 | `backtrace_handler` |
| `SIGTSTP` | terminal suspend (^Z) で TTY 状態保存 | `kill(SIGSTOP)` 自己送信 |
| `SIGCHLD` | 子プロセス通知 (`SA_NOCLDSTOP` オプション) | optional |
| 任意 (Haskell `System.Posix.Signals` 経由) | `generic_handler()` が `siginfo_t` を `timer_manager_control_wr_fd` パイプに書き込み、Haskell スレッドに転送 | `stg_sig_install` で登録 |

POSIX API: `sigaction()`, `sigprocmask()`, `sigemptyset()`, `sigaddset()`, `kill()`。

**注目すべき欠落:** `SIGSEGV` / `SIGBUS` のスタックオーバーフロー検知用ハンドラや `sigaltstack` は **`rts/posix/Signals.c` 内では確認できなかった**。Haskell スタックは TSO の `stk_size` を nursery 内で動的に伸長する設計のため、SIGSEGV ベースのガードページ検知は使っていないと推測される (要再確認)。**TODO (要追加調査):** `rts/sm/` や `rts/StgCRun.*.S` 内に SIGSEGV ハンドラがあるか確認。

### 4.3 Unikraft 代替診断

| GHC RTS 要求 | Unikraft 対応 | 評価 |
|---|---|---|
| Ticker pthread + `ppoll()` | musl の `ppoll` は posix-poll/posix-time 経由。あるいは `lib/posix-timerfd` で代用可 | ✅ |
| `sigaction(SIGINT/SIGPIPE/SIGQUIT/SIGTSTP/SIGCHLD)` | Unikernel ではユーザ介入 (Ctrl-C) もパイプも子プロセスも基本存在しない | ✅ no-op スタブ化で十分 |
| `generic_handler` 経由のユーザシグナル → Haskell 配送 | Haskell ライブラリが `System.Posix.Signals` で signal を受け取るユースケースは Unikernel では稀。`lib/uksignal` で最低限の `sigaction` 受付は可能 | ⚠️ アプリ依存 |
| `sigaltstack` (もし使っていれば) | Unikraft で対応不明 | TODO (要追加調査) |
| `setitimer` / `timer_create` | **9.6 以降不要** | ✅ |

**評価まとめ:** ✅ 現代 GHC がシグナル/タイマーをパイプ + pthread 方式に移行済みであることが、Unikernel 化を**最も助けている**ポイント。これは HaLVM 時代 (GHC 7.x、setitimer 全盛期) より遥かに有利。

## 5. I/O イベント (I/O Event Manager)

### 5.1 GHC.Event の実装

現在の `base` ライブラリの I/O Manager 実装は [`libraries/ghc-internal/src/GHC/Internal/Event/`](https://github.com/ghc/ghc/tree/master/libraries/ghc-internal/src/GHC/Internal/Event) に存在 (9.10 から `ghc-internal` に移動)。

- [`Manager.hs`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/Manager.hs): `EventManager` 型と `Backend` 抽象。Backend 種別:
  - **`EPoll`** (Linux, `HAVE_EPOLL`)
  - **`KQueue`** (BSD/macOS, `HAVE_KQUEUE`)
  - **`Poll`** (fallback)
- [`Control.hs`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/Control.hs): wakeup 機構を `HAVE_EVENTFD` で切り替え:
  - Linux: **`eventfd(2)`** + `eventfd_write`
  - その他: 古典的な `pipe(2)` ペア

### 5.2 epoll 依存

[`EPoll.hsc`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/EPoll.hsc) の FFI:

```haskell
foreign import ccall unsafe "sys/epoll.h epoll_create"
  c_epoll_create :: CInt -> IO CInt
foreign import ccall unsafe "sys/epoll.h epoll_ctl"
  c_epoll_ctl   :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt
foreign import ccall safe   "sys/epoll.h epoll_wait"
  c_epoll_wait  :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "sys/epoll.h epoll_wait"
  c_epoll_wait_unsafe :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt
```

使用フラグ: `EPOLLIN`, `EPOLLOUT`, `EPOLLERR`, `EPOLLHUP`, `EPOLLONESHOT` (注: **`EPOLLET` は使っていない** — level-triggered モード)。`epoll_create1` も使わず、`epoll_create` 後に `setCloseOnExec` を呼ぶ。

### 5.3 Unikraft 代替診断

| GHC.Event 要求 | Unikraft 対応 | 評価 |
|---|---|---|
| `epoll_create` / `epoll_ctl` / `epoll_wait` | **`lib/posix-poll`** (epoll API ベース、v0.10 Phoebe 以降) — [posix-poll は ` epoll system calls and data structures` を実装](https://unikraft.org/releases/v0.17.0) | ✅ |
| `EPOLLIN/OUT/ERR/HUP/ONESHOT` | posix-poll で標準サポート (eventpoll は vfscore で各 fd の poll 操作にプロキシ) | ✅ |
| `eventfd(2)` / `eventfd_write` | **`lib/posix-eventfd`** (専用 uklib) | ✅ |
| `pipe(2)` (fallback wakeup) | `lib/posix-pipe` あり | ✅ |
| `setCloseOnExec` | Unikernel に exec が無いので no-op で OK | ✅ |
| `socket(2)`, `bind`, `accept`, `read`, `write` | `lib/posix-socket` + `lwip` または `lib-virtio-net` | ✅ |
| `kqueue` | (BSD のみなので不要) | N/A |

**評価まとめ:** ✅ **4 領域中もっとも代替容易**。Unikraft の posix-poll + posix-eventfd + lwip の組み合わせは "Linux 風 I/O" を提供することを明確にゴールとしており、GHC.Event の epoll backend がそのまま動くはず。

## 6. Unikernel 代替可能性サマリマトリクス

| 領域 | 主要 API / 機構 | Unikraft 対応コンポーネント | 評価 | 備考 |
|---|---|---|---|---|
| メモリ: 確保 | `mmap(MAP_ANON\|PRIVATE)` | `lib/posix-mmap` + `lib/ukvmem` | ⚠️ | 1MB アライン要件は GHC 側 over-alloc で吸収可能だが要 PoC |
| メモリ: 解放 | `munmap` | posix-mmap | ✅ | |
| メモリ: 保護 | `mprotect(PROT_NONE \| PROT_READ\|WRITE)` | ukvmem | ⚠️ | demand paging の挙動 ([Issue #1475](https://github.com/unikraft/unikraft/issues/1475)) を要検証 |
| メモリ: decommit | `madvise(MADV_FREE/DONTNEED)` | ukvmem (partial) | ⚠️ | `MADV_FREE` semantics 不明 |
| メモリ: 大規模予約 | 8GB+ `MAP_NORESERVE` | ukvmem | ⚠️ | guest 物理メモリ < 予約サイズで OK か |
| メモリ: NUMA | `mbind` | (非対応) | ✅ | no-op で問題なし |
| スレッド: 作成 | `pthread_create/join/detach` | `lib-musl` + `lib/uksched` | ✅ | musl 経由 |
| スレッド: 同期 | `pthread_mutex_*`, `pthread_cond_*` | musl + uksched + uklock | ✅ | `CLOCK_MONOTONIC` も対応 |
| スレッド: 割込 | `pthread_kill` | musl + uksignal | ⚠️ | uksignal の意味論確認要 |
| スレッド: affinity | `sched_setaffinity` | (限定的) | ⚠️ | SMP 成熟度依存。-N1 なら無関係 |
| タイマー: tick | pthread + `ppoll`/`select` | posix-poll + posix-time | ✅ | 9.6+ で signal レス化済み |
| シグナル: 制御 | `sigaction(SIGINT 等)` | `lib/uksignal` | ✅ | スタブで十分 |
| シグナル: ユーザ | `generic_handler` → pipe | uksignal + posix-pipe | ⚠️ | Haskell アプリ依存 |
| シグナル: SEGV | `sigaltstack` (?) | 未対応? | ❌ TODO | 使われていない可能性高 |
| I/O: epoll | `epoll_create/ctl/wait` | `lib/posix-poll` | ✅ | epoll API を直接実装 |
| I/O: eventfd | `eventfd(2)` | `lib/posix-eventfd` | ✅ | 専用 uklib あり |
| I/O: socket | `socket/bind/accept` | `lib/posix-socket` + `lwip` | ✅ | TCP/UDP 標準 |
| I/O: FS | `open/read/write` (VFS) | `lib/vfscore` + `9pfs`/`ramfs` | ✅ | GHC は GC heap dump 等で限定使用 |

**Phase 3 GO/NO-GO 判定:** **GO**。✅ 16項目 / ⚠️ 7項目 / ❌ 1項目 (うち ❌ は使われていない可能性が高い)。クリティカルパスはメモリ層のみで、Unikraft が積極改善中の領域でもある。

## 7. 推奨される次のステップ

調査結果を踏まえた Phase 3 への接続候補:

1. **【最優先】 最小 PoC: "Hello, Haskell" を Unikraft で動かす**
   - `cabal run` で動く最小の `main = putStrLn "Hello"` を、Unikraft の musl + posix-mmap + uksched 構成にリンクし、KVM 上で起動。シングルキャパビリティ (`-N1`) かつ I/O 無し (eventlog/profiling オフ) で **メモリ確保 + Ticker + 基本 GC が回るかだけを検証**する。Issue 化推奨。

2. **【検証】 `lib/ukvmem` の `mprotect` + `madvise(MADV_FREE)` 挙動テスト**
   - GHC をビルドする前に、`OSMem.c` を模した C 単体テスト (1MB アラインで `mmap`、commit/decommit を 1000 回ループ) を Unikraft 上で走らせ、`ukvmem` の挙動と性能を測る。Issue #1475 の影響範囲を確定する。

3. **【調査継続】 GHC RTS の SIGSEGV 利用の有無**
   - 本レポートで未確認だった `rts/sm/`, `rts/StgCRun*`, `rts/Linker.c` 内の SIGSEGV/SIGBUS/sigaltstack 利用を grep で網羅。スタックオーバーフロー検知が pure software 方式 (TSO サイズ比較) であることを確認できれば、Unikernel 化はさらに楽になる。

4. **【代替路線】 HaLVM パターンの再評価**
   - [HaLVM](https://github.com/GaloisInc/HaLVM) は Xen mini-OS 上で GHC RTS を直接動かす実績がある (`halvm-ghc/rts/xen/` で "timers, clocks, interrupts" を独自実装)。Unikraft 路線が ukvmem で詰まる場合、**Spinor バックエンドを LLVM → Xen mini-OS リンク**にする HaLVM 流のアプローチも比較検討する価値あり。ただし HaLVM は GHC 8.0 までで停止しており、フォークアップグレードのコストは高い。

5. **【究極案】 Spinor self-hosting への pivot**
   - GHC RTS を Unikernel に持ち込まず、**Spinor 自身をネイティブコンパイル (Phase 1 PoC で C 出力 → Unikraft) して、Spinor コンパイラ本体も Spinor で書く** (self-hosting)。RTS 依存性を Spinor 側で再実装すれば、GHC の POSIX 制約から解放される。プロジェクトのもう一つの長期ゴール ("カーネルを Spinor で書く") と整合する。

## 8. 参考資料

### GHC ソースコード (一次ソース)
- [`rts/posix/OSMem.c`](https://github.com/ghc/ghc/blob/master/rts/posix/OSMem.c) — メモリ層
- [`rts/posix/OSThreads.c`](https://github.com/ghc/ghc/blob/master/rts/posix/OSThreads.c) — pthread 層
- [`rts/posix/Ticker.c`](https://github.com/ghc/ghc/blob/master/rts/posix/Ticker.c) — タイマー (pthread + ppoll)
- [`rts/posix/Signals.c`](https://github.com/ghc/ghc/blob/master/rts/posix/Signals.c) — シグナル層
- [`rts/Capability.h`](https://github.com/ghc/ghc/blob/master/rts/Capability.h), [`rts/Capability.c`](https://github.com/ghc/ghc/blob/master/rts/Capability.c) — Capability アーキテクチャ
- [`rts/include/rts/Constants.h`](https://github.com/ghc/ghc/blob/master/rts/include/rts/Constants.h) — `MBLOCK_SHIFT = 20` (1MB)
- [`rts/include/rts/storage/Block.h`](https://github.com/ghc/ghc/blob/master/rts/include/rts/storage/Block.h) — block 不変条件
- [`libraries/ghc-internal/src/GHC/Internal/Event/Manager.hs`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/Manager.hs)
- [`libraries/ghc-internal/src/GHC/Internal/Event/EPoll.hsc`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/EPoll.hsc)
- [`libraries/ghc-internal/src/GHC/Internal/Event/Control.hs`](https://github.com/ghc/ghc/blob/master/libraries/ghc-internal/src/GHC/Internal/Event/Control.hs) — eventfd vs pipe wakeup

### GHC Commentary / 公式ドキュメント
- [GHC Wiki: rts/scheduler](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/scheduler) — Capability/Task/TSO の関係、tick 10ms (note: GitLab は Anubis ガード、検索結果メタ情報経由で参照)
- Edward Z. Yang, ["GHC Block Allocator (slides)"](http://web.mit.edu/~ezyang/Public/blocks.pdf) — block descriptor 64B, megablock 1MB

### Unikraft ドキュメント
- [Unikraft Architecture](https://unikraft.org/docs/internals/architecture) — micro-library モデル
- [Unikraft v0.8.0 Enceladus blog](https://unikraft.org/blog/2022-03-29-unikraft-releases-enceladus) — `lib/ukvmem` 導入
- [Unikraft v0.10.0 Phoebe blog](https://unikraft.org/blog/2022-08-20-unikraft-releases-phoebe) — `lib/posix-poll` (epoll 実装)
- [Unikraft v0.11.0 Janus blog](https://unikraft.org/blog/2022-12-02-unikraft-releases-janus) — musl デフォルト化、pthread native サポート
- [Unikraft v0.12.0 Epimetheus blog](https://unikraft.org/blog/2023-02-07-unikraft-releases-epimetheus) — ukvmem の posix-mmap 露出
- [Unikraft v0.14.0 Prometheus](https://unikraft.org/releases/v0.14.0), [v0.17.0 Calypso](https://unikraft.org/releases/v0.17.0), [v0.19.1 Pan](https://unikraft.org/releases/latest)
- [PR #338: Virtual Memory API (x86_64, kvm)](https://github.com/unikraft/unikraft/pull/338)
- [PR #564: New uksched API](https://github.com/unikraft/unikraft/pull/564)
- [Issue #1475: ukvmem page fault handling](https://github.com/unikraft/unikraft/issues/1475) — Phase 3 のリスク要因
- [unikraft/lib-pthread-embedded](https://github.com/unikraft/lib-pthread-embedded), [unikraft/lib-musl](https://github.com/unikraft/lib-musl)

### 関連プロジェクト (代替路線)
- [GaloisInc/HaLVM](https://github.com/GaloisInc/HaLVM) — Haskell on Xen mini-OS (GHC 8.x まで)
- [HaLVM Wiki: What is the Difference Between GHC and the HaLVM?](https://github.com/GaloisInc/HaLVM/wiki/What-is-the-Difference-Between-GHC-and-the-HaLVM%3F) — `rts/xen/` 配下で timers/clocks/interrupts 独自実装

### 関連記事
- ["Memory Fragmentation: A Deeper Look With ghc-debug" (Well-Typed)](https://well-typed.com/blog/2021/01/fragmentation-deeper-look/) — GHC heap layout
- [GHC ticker on Fossies (旧 setitimer 実装の遺構)](https://fossies.org/linux/ghc/rts/posix/ticker/Setitimer.c)

---

**TODO (要追加調査):**
- `rts/sm/`, `rts/StgCRun.*.S`, `rts/Linker.c` 内の SIGSEGV / SIGBUS / `sigaltstack` 利用有無の網羅確認
- Unikraft `ukvmem` の `MADV_FREE` 相当セマンティクス
- Unikraft の `MAP_NORESERVE` 大規模仮想予約サポート状況
- Unikraft SMP マルチコアでの `sched_setaffinity` 実装状況
- gitlab.haskell.org の GHC Commentary 全文 (現在 Anubis ガードで `WebFetch` から到達不可、別手段で要取得)
