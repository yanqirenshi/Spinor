# Research: 先行事例 (HaLVM 等) の分析と現代の GHC におけるベアメタル実行の実現可能性調査

> Issue: [#57](https://github.com/yanqirenshi/Spinor/issues/57)  (親: [#45](https://github.com/yanqirenshi/Spinor/issues/45))
> 姉妹タスク: [#56 GHC RTS の POSIX 依存性の洗い出し](https://github.com/yanqirenshi/Spinor/issues/56)
> 関連ドキュメント: [unikernel-architecture.md](../manual/public/docs/vision/unikernel-architecture.md)

## 1. エグゼクティブサマリ

歴史的に Haskell をベアメタルで動かしてきた 2 系統のプロジェクト — Galois の **HaLVM** (Xen 上)[^halvm-repo] と PSU の **House**[^house-pdx] — はいずれもメンテナンスが停止している。HaLVM は GHC 8.0.2 (2017 年頃の release) で時計が止まり、リポジトリは 2018-12-06 の typo 修正コミットを最後に活動が途絶え[^halvm-commits]、2020-03-10 に GaloisInc によって **archived (read-only)** へ移行した[^halvm-meta]。House は GHC 6.8.2 (2008 年) ベースのままで、ドキュメントの最終更新は 2017 年だがコードは GHC 6 系のまま放置されている[^haskellwiki-kernel]。両者の「失敗の本質」は **「GHC 本体を fork し続けることの保守コスト」** に集約される。

結論として、Spinor OS が HaLVM 路線 (= GHC fork + Xen 専用パッチ) を 2026 年時点で復活させる戦略は推奨しない。代わりに **(a) Phase 1 PoC で実証済みの AOT C → Unikraft ルートを引き続き主軸とし、(b) GHC 公式の wasm32-wasi バックエンド (GHC 9.6 で merge 済み)[^tweag-merged] を上に乗せる Unikraft + WAMR モデル[^unikraft-wamr] を Phase 3 候補として PoC 化し、(c) 長期的には Spinor self-hosting への pivot で GHC 依存そのものを縮退させる**、の 3 段構えが妥当である。Spinor の現在のホストである Windows 11 UCRT64/MSYS2 環境は、wasm バックエンドの公式サポート対象外 (Linux/macOS のみ)[^ghc-wasm-doc] なので、Linux ホストでのクロスビルドを前提とした CI への移行も併せて検討する必要がある。

## 2. HaLVM (Haskell Lightweight Virtual Machine) の解剖

### 2.1 プロジェクト概要と歴史

HaLVM は Galois Inc. が 2006 年頃に開始した、GHC を Xen ハイパーバイザ向けに移植したプロジェクトである[^halvm-wiki]。OS 抽象化を捨て、Haskell プログラムが Xen ドメイン (DomU) として直接ブートする「ライブラリ OS」型 Unikernel を実現する。1066 stars、83 forks (2026-05 時点) と Haskell 系 OS プロジェクトとしては最大級の認知度を得た[^halvm-meta]。

主要なリリースは以下のとおり:

| バージョン | 採用 GHC | 備考 |
|---|---|---|
| 1.x 系 | GHC 7 系 | 初期 PoC、Xen 限定 |
| 2.0 Developer Preview | GHC 7.8 | Threaded RTS と SMP 対応[^halvm-wiki] |
| 2.3.0 | GHC 8.0.1 | CHANGELOG の最初のエントリ[^halvm-changelog] |
| 2.4.0 | GHC 8.0.2 | **最後の GHC アップグレード**[^halvm-changelog] |

リポジトリ活動の実態を GitHub API で確認すると、`master` への最新コミットは `3ec1d7e` (2018-12-06, typo 修正 PR の merge) で[^halvm-commits]、`halvm-ghc` (GHC fork) の最終 push は 2018-01-28 となっている[^halvm-ghc-meta]。さらに 2020-03-10 に正式に archive 化されており、Issues は 37 件 open のまま凍結された[^halvm-meta]。

### 2.2 アーキテクチャ (Xen + GHC RTS パッチ)

HaLVM Wiki の「What is the Difference Between GHC and the HaLVM?」[^halvm-diff] によれば、HaLVM は stock GHC に対し以下の改造を加えている:

1. **`crt0.o` の置換**: 標準 C ランタイムの起動コードを削除し、`halvm-ghc/rts/xen/` 配下の独自サブシステムが Xen との橋渡しを担う。
2. **`minlibc` の同梱**: 必要最小限の C 標準ライブラリ実装をリポジトリ内に持つ (Xen 上には musl/glibc が存在しないため)。
3. **`libm` を Julia から借用**: 数値演算ライブラリは Julia プロジェクトの実装を取り込んでいる。
4. **Threaded RTS の "deep magic" な改造**: スレッドスケジューラ部分が Xen の VCPU モデルに合わせて書き換えられている。
5. **ヘッダの自前生成**: GHC/GCC がコンパイル時に検出する struct サイズ等を、ホスト Linux ではなく Xen ターゲットの値に固定する仕組み。
6. **クロスコンパイラ build**: HaLVM は GHC を stage1 クロスコンパイラとして build する。stage2 を build しないため、後述の Template Haskell 問題が発生する。

これらの改造は GHC のソースツリーに直接当たるパッチ集合として `halvm-ghc` 別リポジトリ (GHC mirror の fork) に蓄積されている[^halvm-ghc-meta]。

### 2.3 GHC 8 以降への追従が止まった技術的障壁

GHC 8.0.2 で時計が止まった主因として、複数の構造的問題が指摘できる:

1. **Template Haskell × Cross-compiler ジレンマ**: HaLVM Wiki の説明では「HaLVM は stage1 compiler を build するが、Template Haskell は本来 stage2 を要求する」と明記されている[^halvm-diff]。HaLVM チームは Template Haskell の macro compiler が "Intel/Xen" を target するよう build system を改造して回避していたが、GHC 8.2 以降この領域は **Hadrian (Shake-based build system) への移行**[^ghc-submodules]、`stage` 概念の見直し、cross-compilation サポートの本格化など、毎リリース大きく変動した。Galois のパッチを追従させる作業量がリリースごとに膨張した。

2. **submodule 地獄**: GHC の build には libraries (`base`, `bytestring`, `text` 等) の submodule pin が含まれ、HaLVM は対応する Xen 向けパッチを各 submodule にも当てる必要があった[^halvm-changelog]。GHC のアップグレードは事実上 N 個の submodule 同時アップグレードを意味し、各々で API 互換性破壊と戦う必要があった。

3. **Threaded RTS の進化**: GHC 8.2 以降、non-moving GC[^ghc-nonmoving-todo] (GHC 8.10〜) や IOManager (Linux で epoll/io_uring の本格活用) など、RTS は急速に「Linux 以外で動かしづらい」方向へ進化した。HaLVM の "deep magic" な RTS パッチを追従コストは指数関数的に増大した。**詳細は #56 を参照**。

   > **TODO (要追加調査):** non-moving GC の Xen 移植可能性については primary source を未確認。

4. **人員リソース**: HaLVM の最終的なメンテナは Adam Wick (Galois) 1 人に依存していた[^infoq-wick]。Galois の business priority がフォーマル検証や暗号方面 (Cryptol, SAW) にシフトし、HaLVM への投資が継続できなくなった、というのが状況証拠から推測できる最大の要因。Issue tracker には GHC 8.2 以降への upgrade を要求する Issue が複数あるが、いずれも未対応のまま archive された[^halvm-meta]。

5. **Xen 自体の相対的衰退**: 2018-2020 頃から Xen の存在感は KVM (Firecracker, Cloud Hypervisor) にシフトした。HaLVM は Xen-only のため、エコシステムの動きと逆行した。

### 2.4 「失敗の本質」の分析

総合すると、HaLVM が止まった本質は **「単一の研究機関が、活発に変化する巨大コンパイラ (GHC) をハイパーバイザ向けに fork し続ける」モデルの構造的不可能性** にある。これは人員不足という偶発要因ではなく、ほぼ必然と評価すべきである:

- GHC のリリースサイクルは年 1-2 回、各回で submodule・RTS・build system が大きく動く。
- HaLVM のような out-of-tree fork は「数千行レベルのパッチセットを毎リリース rebase する」労働を、対価なしで永続的に背負うことになる。
- 同じ理由で、長らく単独 fork として続いていた **GHCJS** も GHC 8.10 で時計が止まり、最終的に GHC 本体への merge (JS backend, IOG 主導) によってのみ存続を確保した[^iog-js]。
- **Asterius** (Haskell-to-Wasm の前世代) も同様で、2022-11-24 に archive され、GHC 公式 wasm backend へ統合された[^asterius-archived]。

すなわち過去 10 年で、GHC を out-of-tree で fork するアプローチは **GHCJS → JS backend マージ、Asterius → wasm backend マージ、HaLVM → 単純に死亡** という 3 つの事例で「mainline 統合できなかった fork は死ぬ」というパターンを示している。Spinor OS が同じ轍を踏まないためには、**GHC mainline 上に存在する抽象化レイヤー (wasm32-wasi) に乗る** か、**そもそも GHC への依存をなくす (self-hosting)** という方向しかない。

## 3. 現代の代替アプローチ

### 3.1 `ghc -fllvm` 経由のベアメタル実行

GHC の `-fllvm` フラグは GHC の native code generator の代わりに LLVM IR を経由するバックエンドを選ぶオプションだが、これは **target triple そのものを切り替える機構ではない**。`-fllvm` は依然として GHC の標準 RTS とリンクし、ホスト OS の libc/pthread に依存する。`-fllvm` を経由しても `x86_64-unknown-none` のようなターゲットへの直接コード生成はできない。

過去には PSU の **House** プロジェクトが GHC 6.8.2 にパッチを当て、RTS が「OS のシステムコール」ではなく「自前の bare-metal カーネル呼び出し」を使うよう書き換えた[^haskellwiki-kernel]。House は IA32 上で動作し、NE2000 ドライバや TCP/IP スタックまで実装された研究プロトタイプだが、GHC 6 系のまま 17 年以上更新されていない。シャットダウン時に RTS がメモリを解放しないという既知の leak も未修正のままである[^haskellwiki-kernel]。

**結論:** `-fllvm` を「ベアメタル抜け穴」として使う実例は現代の GHC では確認できない。LLVM IR を出力しても、その上に GHC RTS の POSIX 依存が乗ったままなので、ベアメタル化には結局 HaLVM 型の RTS パッチが必要になる。この経路は **取らない方が良い**。

### 3.2 GHC Wasm (WASI) バックエンド

2022-11 に Tweag I/O が開発していた wasm バックエンドが GHC mainline へ merge され、**GHC 9.6 (2023-03 release)** から公式に利用可能になった[^tweag-merged]。Asterius の後継であり、決定的な違いは **「GHC 公式の RTS をそのまま使う」** 点である:

- target triple: `wasm32-wasi`[^ghc-wasm-doc]
- 実行ランタイム: `wasmtime`, `wasmedge`, `wasmer`, `wasm3` 等の WASI 対応エンジン[^tweag-merged]
- 機能カバレッジ: STM, profiling, eventlog 等、Asterius で動かなかった機能が「out of the box」で動く[^tweag-merged]
- GC: GHC 標準の GC をそのまま wasm 上で動かす (これが Asterius との最大の差別化ポイント)

ただし制約も多い:

- **依然 tech preview**: 公式 binary distribution に含まれず、`ghcup` でデフォルト install できない[^ghc-wasm-doc]。
- **ホスト OS の制約**: 手動 build できるのは `{x86_64, aarch64}-{linux, darwin}` のみで、**Windows ホストは未サポート**[^ghc-wasm-doc]。
- **single-target compiler**: GHC は依然 single-target なので、host GHC とは別に wasm32-wasi-ghc を build/install する必要がある。
- **WASI の POSIX 抽象は部分的**: wasi-preview1 は file/clock/random 等の基本のみで、network や thread は wasi-preview2 (Component Model) を待つ必要がある。

#### Unikernel との接続

「Wasm モジュールを Unikernel として起動する」アプローチは複数の実装が存在する:

- **Unikraft + WAMR**: `unikraft/lib-wamr` および `unikraft/app-wamr` が Intel WAMR を Unikraft 外部ライブラリとして port している。WAMR の AOT コンパイラ (LLVM ベース) で wasm を native code に落とし込み、interpreter/JIT を除外することで TCB を最小化可能[^unikraft-wamr]。
- **Unikraft + wazero**: Unikraft Cloud のブログでは Go 実装の wazero を Unikraft 上で動かすデモが示され、cold start 20ms 程度を達成[^unikraft-wasm-blog]。
- **OSv + wasmer / NanoVMs + wasm**: 他の Unikernel ファミリーでも同様のサンドボックスモデルが採用されている。
- **urunc**: 「runc の Unikernel 版」として、Wasm を含む Unikernel 化されたワークロードを containerd 経由でデプロイする runtime[^cloudkernels-urunc]。

**Spinor 視点での組み立て**: 「`spinor-repl.hs` を `wasm32-wasi-ghc` で `.wasm` にし、その `.wasm` を WAMR (on Unikraft) で実行する」という 2 段ロケットが、現実的な Phase 3 ルートになり得る。GHC fork は不要で、すべて upstream mainline 上の機能だけで構成できる点が大きな利点。

### 3.3 その他の選択肢 (GHC JS backend, Asterius 等)

- **GHC JavaScript backend** (GHC 9.6 で merge[^iog-js]): IOG が GHCJS を mainline に取り込んだもの。Unikernel とは無関係だが、「out-of-tree fork → mainline 統合」の成功例として参照価値がある。Template Haskell サポートが未だ不完全な点も、Phase 3 で意識すべき制約として記録しておく。
- **Asterius**: 2022-11-24 に archive 済み[^asterius-archived]。歴史的参考のみ。
- **Mirage OS (OCaml)**: 比較対象として無視できない。OCaml が unikernel 用に選ばれた理由は「single-threaded runtime」「systems programming に適した module system」「Xen 内部での OCaml 利用実績」であり、Haskell が同じ位置を取れなかった構造的差異を示唆する[^mirageos-blog]。MirageOS は 2025-02 時点で OCaml 5 (effect handlers ベースの concurrency) にも移行に成功しており、コア組織 (Tarides) が継続的にメンテしている点も、HaLVM との対比で参考になる。
- **HermitCore / Hermit**: Rust ベースの Unikernel。C アプリのリンクも可能で、`unikernel-architecture.md` でも第二候補として記載済み。

## 4. Spinor への適用

### 4.1 ビルド環境の整合性 (UCRT64 / MSYS2 / cabal)

Spinor の現状の開発環境は Windows 11 上の UCRT64/MSYS2 + cabal + GHC 9.6+ である。HaLVM 路線が Fedora 23+ Linux のみサポート[^halvm-supported] だった事実と対比すると、いずれの選択肢にも以下の課題がある:

| 戦略 | UCRT64/MSYS2 整合性 |
|---|---|
| HaLVM 復活 | × (Linux only、Xen ホスト前提) |
| `ghc -fllvm` bare metal | × (RTS パッチ必須、Windows での実例なし) |
| GHC wasm backend | △ (公式は Linux/macOS のみ。Windows ホストは未サポート[^ghc-wasm-doc]) |
| AOT C → Unikraft (Phase 1) | △ (Unikraft ビルドは Linux ホスト推奨。WSL2 で回避可能) |
| Spinor self-hosting | ◎ (GHC 依存を最終的に縮退できれば、ビルド環境は自由) |

要するに **どの選択肢を取っても、最終的には Linux (もしくは WSL2) を CI/ビルド環境として持つ必要がある**。これは HaLVM 時代と本質的には変わらず、Windows ネイティブのまま Unikernel をビルドできる解は現時点で存在しない。

### 4.2 cross-compilation の要否

| 経路 | cross-compile 必要か |
|---|---|
| Phase 1 (AOT C → Unikraft) | △ Unikraft 側で `gcc` cross-compile (`x86_64-linux-gnu-gcc` 等) を実行。Spinor 本体 (GHC) は host で完結。 |
| Phase 3a (GHC binary を Unikraft の musl 互換上で動かす) | ○ static link した Linux/x86_64 ELF を作る。host GHC が Linux ターゲットなら問題なし。MSYS2 上の GHC は Windows ELF を吐くので、Linux 向けには **WSL2 or Docker でビルド** が必須。 |
| Phase 3b (GHC wasm backend → WAMR/Unikraft) | ○ wasm32-wasi-ghc という cross compiler の install が必須。**Windows ホストでは公式サポートなし**。 |

### 4.3 既存 Phase 1 PoC との接続

`unikernel-architecture.md` で計画されている Phase 1 PoC (AOT C → Unikraft) は、本研究の結論と完全に整合する:

- Phase 1 は **GHC RTS を Unikernel に持ち込まない** ことで、HaLVM 型の保守地獄を回避している。
- AOT 出力の C コードは POSIX libc の薄いサブセットにしか依存せず[^unikernel-arch-spinor]、Unikraft の musl で完全に充足可能。
- Phase 3 への移行時には、本研究で推奨する「wasm32-wasi + WAMR/Unikraft」を **Phase 3a の前段** として組み込むことで、HaLVM 路線を完全に skip する道が開ける。

Phase 1 の成果物 (`output.c` + `runtime/spinor.c` + Unikraft Kraftfile) はそのまま、Phase 3 で `spinor self-compile` できるようになった暁の self-hosted binary を Unikraft で動かす土台として再利用できる。

## 5. 持続可能な保守戦略の提案

| # | 戦略 | 概要 | メリット | リスク | 推奨条件 |
|---|---|---|---|---|---|
| **A** | **GHC fork 路線 (HaLVM 再現)** | `halvm-ghc` を fork し、GHC 9.6+ を Unikraft/KVM 向けに patch | RTS まで含めた完全制御 | GHCJS/Asterius/HaLVM 全部が死んだパターン。人員ゼロで開始する Spinor では確実に同じ運命 | **採用不可** |
| **B** | **GHC wasm32-wasi + Unikernel** | GHC mainline の wasm backend で `.wasm` を作り、Unikraft + WAMR で実行 | upstream に乗るので fork 不要。Asterius の轍を踏まない | wasm backend が tech preview。Windows ホストは公式未サポート。Network/Threading は wasi-preview2 待ち | **本命**。Linux ホストの CI を整備でき、tech preview の不安定さを許容できる場合 |
| **C** | **AOT C → Unikraft (現行 Phase 1 の延長)** | Spinor の AOT 出力した C/LLVM IR を Unikraft の musl 上で動かす。GHC RTS は持ち込まない | 最も低リスク。実装済み | REPL/動的評価機能を Unikernel に持ち込めない (静的にコンパイル済みのプログラムだけ動く) | サービス向け Spinor application を unikernel として deploy するユースケース |
| **D** | **Spinor Self-hosting への pivot** | Spinor 自身で REPL/コンパイラを書き直し、GHC 依存をなくす | The Ultimate Dream の核心。GHC のバージョン進化と無関係になる | 数年がかりの大工事。GC、型推論器、マクロ系を Spinor 上に再実装する必要 | 長期戦略。Phase 1/2 で AOT が成熟した後 |
| **E** | **GHC Linux/x86_64 binary をそのまま Unikraft musl で動かす PoC** | static link した stock GHC binary を Unikraft の Linux ABI 互換層で実行 | パッチゼロ、検証が早い | Unikraft の `mmap`/`pthread`/signal 実装の完成度に依存。動かない箇所は upstream に PR が必要 | Phase 3a の最初の実験として 1-2 week 投じる価値あり。動けば B/D への中間ステップになる |

**推奨ロードマップ:** `C (継続) → E (PoC で feasibility 判定) → B (本命) → D (長期)` の順に進める。A は明示的に却下する。

## 6. 推奨される次のステップ

1. **Linux CI 環境の整備**: Phase 3 のどのルートを取っても Linux host (もしくは WSL2) が必要。GitHub Actions Linux runner で `wasm32-wasi-ghc` の install/cache を回す CI を Issue として起票。
2. **Phase 3a PoC: Stock GHC binary on Unikraft**: Spinor 本体を `cabal build --enable-executable-static` で static link した Linux/x86_64 ELF にし、Unikraft の Linux ABI 互換 (`lib-posix-process`, `lib-musl`) 上で動くかを 1-2 week で検証。動けば戦略 E が成立し、本命 B への踏み台が完成する。
3. **Phase 3b PoC: Hello World on wasm32-wasi + WAMR on Unikraft**: `wasm32-wasi-ghc -o hello.wasm Hello.hs` → `unikraft/app-wamr` で実行、までを「最小の Haskell-on-Unikernel」として再現する。これが動けば、HaLVM 路線を捨てて B に commit する判断ができる。
4. **Self-hosting 進捗評価の Issue 起票**: 戦略 D の前提として、Spinor が「自身の `Eval.hs` をコンパイルできる」までにどれだけのギャップがあるかを定量化する Issue を起票 (型推論器の Spinor 移植、GC の Spinor 移植 等)。
5. **#56 との接続**: 本書は GHC RTS の内部 API 単位の調査には踏み込んでいない。戦略 E (stock GHC binary on Unikraft) の feasibility を判断するには #56 の調査結果が必須なので、両 Issue の completion を Phase 3 着手の precondition として明示する。

> **TODO (要追加調査):**
> - Adam Wick の最終的な HaLVM EOL アナウンス (Galois ブログ、Twitter 等) の一次ソースを未取得。InfoQ インタビュー[^infoq-wick] (2016) 以降の本人発言を要追加調査。
> - GHC wasm backend を Windows ホストで build する非公式手順 (`ghc-wasm-meta` の Windows fork 等) の存在有無を要確認。
> - Unikraft の `pthread_key_create` / `signal` / `mmap` の完成度を、Unikraft 公式 conformance テスト結果から定量化する作業は #56 のスコープ。

## 7. 参考資料

### HaLVM 系 (一次ソース)

- HaLVM repository (archived): https://github.com/GaloisInc/HaLVM
- HaLVM commit log (last commit 2018-12-06): https://github.com/GaloisInc/HaLVM/commits/master
- HaLVM CHANGELOG.md: https://github.com/GaloisInc/HaLVM/blob/master/CHANGELOG.md
- HaLVM Wiki — "What is the Difference Between GHC and the HaLVM?": https://github.com/GaloisInc/HaLVM/wiki/What-is-the-Difference-Between-GHC-and-the-HaLVM%3F
- HaLVM Wiki — Supported Systems: https://github.com/GaloisInc/HaLVM/wiki/SupportedSystems
- halvm-ghc (patched GHC fork, archived): https://github.com/GaloisInc/halvm-ghc
- InfoQ interview with Adam Wick (2016): https://www.infoq.com/interviews/wick-security-unikernels/

### House / 旧世代 bare-metal Haskell

- House project (Programatica): https://programatica.cs.pdx.edu/House/
- "A Principled Approach to Operating System Construction in Haskell" (ICFP 2005): https://dl.acm.org/doi/10.1145/1086365.1086380
- HaskellWiki — Kernel Modules: https://wiki.haskell.org/Kernel_Modules

### GHC Wasm / JS backend

- Tweag blog "WebAssembly backend merged into GHC" (2022-11-22): https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/
- GHC 9.6 User's Guide — Using the GHC WebAssembly backend: https://downloads.haskell.org/~ghc/9.6-latest/docs/users_guide/wasm.html
- GHC 9.14 (latest) WebAssembly backend doc: https://downloads.haskell.org/ghc/latest/docs/users_guide/wasm.html
- Asterius (archived 2022-11-24): https://github.com/tweag/asterius
- IOG — "JavaScript backend merged into GHC" (2022-12-13): https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/

### Unikernel / Wasm 統合

- Unikraft + WAMR: https://github.com/unikraft/lib-wamr / https://github.com/unikraft/app-wamr
- Unikraft blog "Unikernels and WebAssembly: Friends or Foes?": https://unikraft.com/blog/unikernels-and-wasm/
- Cloudkernels "Sandboxing WASM with Unikernels": https://blog.cloudkernels.net/posts/wasm-urunc/
- MirageOS on OCaml 5 (2025-02): https://tarides.com/blog/2025-02-06-mirageos-on-ocaml-5/
- "Unikernels: The Rise of the Virtual Library Operating System" (CACM): https://cacm.acm.org/practice/unikernels/

### Spinor 内部資料

- `manual/public/docs/vision/unikernel-architecture.md` — Phase 1 PoC 設計書
- `manual/public/docs/vision/the-ultimate-dream.md` — Spinor OS 構想
- Issue #45 (parent): https://github.com/yanqirenshi/Spinor/issues/45
- Issue #56 (sister task): https://github.com/yanqirenshi/Spinor/issues/56

[^halvm-repo]: HaLVM repository: https://github.com/GaloisInc/HaLVM (archived 2020-03-10)
[^halvm-meta]: GitHub API `GET /repos/GaloisInc/HaLVM` (2026-05-16 取得): `archived: true`, `pushed_at: 2018-12-06`, `stargazers_count: 1066`, `open_issues_count: 37`
[^halvm-commits]: GitHub API `GET /repos/GaloisInc/HaLVM/commits?per_page=5`: 最新コミット `3ec1d7e4` (2018-12-06, "Fix typo + inconsistent indents in examples/HighLevel/Halfs/Halfs.hs")
[^halvm-changelog]: HaLVM CHANGELOG.md: https://github.com/GaloisInc/HaLVM/blob/master/CHANGELOG.md (v2.4.0: "Upgrade halvm-ghc to GHC 8.0.2", v2.3.0: GHC 8.0.1)
[^halvm-wiki]: HaLVM Wiki home: https://github.com/GaloisInc/HaLVM/wiki ("HaLVM 2.0 Developer's Preview supports GHC 7.8" 等)
[^halvm-diff]: "What is the Difference Between GHC and the HaLVM?" — https://github.com/GaloisInc/HaLVM/wiki/What-is-the-Difference-Between-GHC-and-the-HaLVM%3F
[^halvm-supported]: HaLVM Wiki — SupportedSystems: https://github.com/GaloisInc/HaLVM/wiki/SupportedSystems ("Galois does its primary HaLVM development work on Fedora")
[^halvm-ghc-meta]: GitHub API `GET /repos/GaloisInc/halvm-ghc`: `archived: true`, `pushed_at: 2018-01-28`, `default_branch: halvm`
[^house-pdx]: Programatica House: https://programatica.cs.pdx.edu/House/
[^haskellwiki-kernel]: HaskellWiki — Kernel Modules: https://wiki.haskell.org/Kernel_Modules ("House patches modified the GHC runtime system (RTS) to use the proper Kernel calls, instead of allocating its own memory" — GHC 6.8.2 ベース、最終更新 2017-02)
[^tweag-merged]: Tweag — "WebAssembly backend merged into GHC" (2022-11-22): https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/
[^ghc-wasm-doc]: GHC 9.6 User's Guide §14 "Using the GHC WebAssembly backend": https://downloads.haskell.org/~ghc/9.6-latest/docs/users_guide/wasm.html ("manual builds support {x86_64, aarch64}-{linux, darwin}", "tech preview", "still a single-target compiler")
[^iog-js]: IOG — "JavaScript backend merged into GHC" (2022-12-13): https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/
[^asterius-archived]: Asterius repository (archived 2022-11-24): https://github.com/tweag/asterius ("DEPRECATED in favor of ghc wasm backend")
[^unikraft-wamr]: Unikraft WAMR port: https://github.com/unikraft/lib-wamr および https://github.com/unikraft/app-wamr
[^unikraft-wasm-blog]: Unikraft blog — "Unikernels and WebAssembly: Friends or Foes?": https://unikraft.com/blog/unikernels-and-wasm/
[^cloudkernels-urunc]: Cloudkernels blog — "Sandboxing WASM with Unikernels for Secure Cloud-Native Deployments": https://blog.cloudkernels.net/posts/wasm-urunc/
[^mirageos-blog]: Tarides — "MirageOS on OCaml 5!" (2025-02-06): https://tarides.com/blog/2025-02-06-mirageos-on-ocaml-5/
[^infoq-wick]: InfoQ interview — "Adam Wick on Security, Formal Methods, Types, Unikernels, HaLVM, DRM": https://www.infoq.com/interviews/wick-security-unikernels/
[^ghc-submodules]: GHC GitLab — submodules conventions: https://gitlab.haskell.org/ghc/ghc/-/wikis/working-conventions/git/submodules
[^ghc-nonmoving-todo]: TODO: GHC non-moving GC (Ben Gamari, GHC 8.10+) のベアメタル移植可能性は本研究では一次資料未確認。
[^unikernel-arch-spinor]: 内部資料 `manual/public/docs/vision/unikernel-architecture.md` 第 3.1 節 (libc 依存リスト)
