# Research: Approach 3 — 「Linear Spinor」 純粋 R&D Lisp マシン構想

> Issue: [#62](https://github.com/yanqirenshi/Spinor/issues/62)  (親ビジョン: [The Ultimate Dream](../../manual/public/docs/vision/the-ultimate-dream.md))
> 関連姉妹タスク: [#56 GHC RTS POSIX 依存性](https://github.com/yanqirenshi/Spinor/issues/56) / [#57 Haskell unikernel 史](https://github.com/yanqirenshi/Spinor/issues/57) / [#45 Approach 2 ベアメタル Haskell](https://github.com/yanqirenshi/Spinor/issues/45) / [#46 Approach 3 親 Issue](https://github.com/yanqirenshi/Spinor/issues/46)
> 関連ドキュメント: [unikernel-architecture.md](../../manual/public/docs/vision/unikernel-architecture.md), [the-ultimate-dream.md](../../manual/public/docs/vision/the-ultimate-dream.md)
> 立ち位置: 本書は **Approach 1 (AOT C → Unikraft) と Approach 2 (Haskell RTS 移植) を「実用」「ランタイム移植」として尊重しつつ、それらと完全に独立した第三の R&D 路線**として設計する純粋探求論文である。100 年後も通用する Lisp マシン設計を志向し、現行の Spinor が即座に到達できない領域までを射程に入れる。

## 1. エグゼクティブサマリ

「Linear Spinor」は、**Haskell も C も介在させずに動作するベアメタル Lisp マシン**のためのアーキテクチャ提案である。設計の中核は次の 3 つの公理に集約される。

1. **「線形性をデフォルトに、共有を opt-in に」** — Spinor の S 式そのものに線形性を宿らせ、所有権 (Ownership) と借用 (Borrowing) をコンパイル時に静的検証する。GC は「型システム上で『共有が必要』と明示された値」だけが訪れる別世界として隔離する (4 層メモリモデル)。
2. **「カーネルは Lisp、unsafe は型で隔離」** — 生メモリ操作・MMIO・インラインアセンブリを `(unsafe ...)` 形式で Rust 流に隔離・追跡し、カーネル内で何が "信頼ベース" かを型レベルで把握できるようにする。
3. **「Mezzano から学び、Haskell から借り、Rust に倣い、Spinor で書き直す」** — 4 系統の知見を融合する:
   - **Mezzano**[^mezzano-repo] からは、ベアメタル上で動く Lisp の **オブジェクト表現** (`+address-tag-cons+` 等のタグ付きポインタ) と **ブートストラップ手順** (KBoot + 自己コンパイル) を継承する。
   - **Haskell** からは、**Hindley-Milner 型推論 (Algorithm W)**、**Linear Haskell**[^linear-haskell] の「linearity on arrows」設計、**ADT + パターンマッチ**、**Expr / Val の厳密な分離 (構文と値の型レベル分離)** を借りる。Spinor 自身が現在 Haskell で実装されている事実は偶然ではなく、**型システムの DNA 自体が Haskell 系**である。Approach 3 でランタイム依存は切るが、**意味論の遺産は受け継ぐ**。
   - **Rust** からは、**借用検査 (NLL)**、**`unsafe` ブロックによる safe / unsafe 境界の型レベル可視化**、**`#[global_allocator]` パターン**、**`no_std` ベアメタル開発作法** を取り込む。
   - その上で Mezzano の GC のみに依存する設計を、**線形性 + 借用 + リージョン + opt-in GC のハイブリッド**で置き換える。Carp[^carp-repo] が示した「Lisp + 借用検査 + GC レス」の実証を Spinor の OS スコープへ拡張する。

**現状資産の再評価:** Spinor は既に「Linear Spinor の core」となる構成要素を実装済みである。

| 既存資産 | 場所 | Linear Spinor における位置付け |
|---|---|---|
| `Linearity { Linear, Unrestricted }` | [Type.hs:18](../../src/Spinor/Type.hs:18) | 線形性修飾子の基盤。本書ではここに `Affine` `Borrowed` `Owned` `Shared` を追加する案を提示。|
| `BorrowCheck` モジュール | [BorrowCheck.hs](../../src/Spinor/BorrowCheck.hs) | `DoubleUse` / `Unconsumed` / `UseAfterMove` 検出済み。本書ではこれをライフタイム解析へ拡張する道筋を示す。|
| `EscapeAnalysis` モジュール | [EscapeAnalysis.hs](../../src/Spinor/EscapeAnalysis.hs) | `EWithRegion` / `EAllocIn` の逃避解析が稼働済み。本書では「サブシステム隔離リージョン」への拡張を提案する。|
| `EWithRegion` / `EAllocIn` AST | [Syntax.hs:154-155](../../src/Spinor/Syntax.hs:154) | リージョン構文が既に S 式に統合済み。Linear Spinor の Tier 2 メモリ層として再活用する。|
| Codegen (C) | [Codegen.hs](../../src/Spinor/Compiler/Codegen.hs) | Approach 1 の到達点。本書の Stage 0/1 ブートストラップで再利用する。|

**判定:** **GO (R&D 推進可)**。Linear Spinor は「未着手のグリーンフィールド計画」ではなく、**既存の借用検査・リージョン・線形性修飾子を OS 記述レベルに昇華させる進化路線**として位置付けられる。先行事例 (Mezzano は GC ベース、Carp はホスト OS 前提) のいずれも到達していない「線形性 × リージョン × ベアメタル」という未踏領域に、Spinor は既に半歩踏み込んでいる。残る課題は本書で詳述する **(a) ライフタイムの導入、(b) unsafe 隔離プリミティブ、(c) ブート初期化、(d) セルフホスト** の 4 つである。

---

## 2. プロジェクト位置付けと 3 アプローチの相互関係

### 2.1 3 アプローチの再定義

| Approach | 通称 | 性格 | GHC 依存 | C 依存 | 主たるリスク |
|---|---|---|---|---|---|
| 1 | AOT C-Codegen | 実用・最短 | なし (実行時) | あり (生成 C を gcc) | C 言語 ABI と libc 表面に縛られる |
| 2 | Haskell RTS on Unikernel | ランタイム移植 | あり (要 PoC) | あり (musl) | GHC の進化と out-of-tree 保守地獄 ([#57](https://github.com/yanqirenshi/Spinor/issues/57) 参照) |
| **3** | **Pure R&D Lisp Machine** | **究極の探求** | **なし** | **なし** | **言語自体の進化が必要 (本書で詳述)** |

Approach 3 は他 2 つと **対立関係ではなく補完関係**にある。Approach 1 は短期収益、Approach 2 は中期スケール、Approach 3 は長期的な純粋性 (= 100 年後の Lisp マシンの姿) を担う。R&D 成果物は Approach 1/2 にもバックポート可能 (例: 線形性ベースの GC レス ALGORITHM は Approach 1 の `runtime/spinor.c` の leak 問題を解決する)。

### 2.2 R&D の制約緩和

本書は実装期日もリソースも約束しない。**「100 年後の Lisp マシン設計に必要な思考実験」**を目的とし、以下の制約を明示的に解除する:

- **既存 Spinor との後方互換性:** 不要 (将来の v2 言語と仮置きする)
- **Haskell 実装可能性:** 不要 (最終的には Spinor 自身で書き直す)
- **既存ハードウェアへの即応:** 不要 (アーキテクチャは抽象 ISA で設計可)

逆に、本書で守るべきは次の制約のみである:

- **GC への暗黙依存禁止:** すべての値はライフタイムが型上で説明可能でなければならない
- **POSIX 抽象禁止:** ファイル / プロセス / シグナル等の概念は OS 自身が定義する
- **C ランタイム禁止:** `malloc` / `free` / `printf` 等は Spinor で再実装する

---

## 3. 先行事例の解剖と再定義

### 3.1 Mezzano (Common Lisp OS, 2010 年代〜現役)

Mezzano は froggey 氏単独メンテの Common Lisp ベアメタル OS である[^mezzano-repo]。x86-64 で実機ブートし、KBoot ブートローダ経由で起動する[^mezzano-kboot]。READMEから抽出された主要構成要素は以下の通り[^mezzano-readme]:

| 要素 | 内容 |
|---|---|
| 実装言語 | Common Lisp 99.8% (ホストはセルフコンパイル後は SBCL 不要) |
| コンパイラ | **SSA ベースの新バックエンド**、unboxed 値表現サポート |
| GC | **世代別コピー GC** (young-gen / old-gen の二世代、それぞれに newspace/oldspace 二領域) |
| ブートローダ | KBoot (BIOS/UEFI 両対応、GRUB モジュール `mezzano.mod` も提供) |
| 対応 HW | x86-64 (公式)、AArch64 (一部実機で動作確認済み) |
| 機能 | SMP、virtio-net、Intel HDA 音声、FAT32/EXT2/3/4、Virgl 経由 3D、GUI |
| ライセンス | MIT |

#### 3.1.1 オブジェクト表現 (タグ付きポインタ)

`system/gc.lisp`[^mezzano-gc] から抽出されるアドレスタグスキームは次の通り[^mezzano-gc-fetch]:

| タグ定数 | 意味 |
|---|---|
| `+address-tag-general+` | 汎用ヒープオブジェクト (ベクタ、構造体、関数等) |
| `+address-tag-cons+` | cons セル専用領域 (リスト集中配置で局所性向上) |
| `+address-tag-pinned+` | GC で移動しないオブジェクト (DMA バッファ、ハードウェア構造体等) |
| `+address-tag-stack+` | dynamic-extent (DX) 割り当てによるスタックオブジェクト |

これらタグは `(ldb (byte +address-tag-size+ +address-tag-shift+) address)` でアドレス上位ビットから抽出される。さらに各オブジェクトには:

- **`+address-old-generation+`**: 世代ビット (0 = young, 1 = old)
- **`+address-semispace+`**: 半空間ビット (現在の newspace を `*young-gen-newspace-bit*` / `*old-gen-newspace-bit*` で追跡し、GC 後にフリップ)
- **世代間ポインタ書き込みバリア**: old-gen から young-gen への参照 (intergenerational-pointers) の妥当性を検証

**Spinor 視点での示唆:**
- ✅ **タグスキームは流用価値が高い**: cons 領域の分離 (`+address-tag-cons+` 相当) は局所性と GC 効率の両面で有利。Spinor の `VList` 表現を tagged pointer 化する際の素直な出発点となる。
- ✅ **`+address-tag-pinned+` は MMIO・DMA を扱うカーネル記述で必須**: Linear Spinor の `unsafe (with-pin ...)` 構文の意味論的根拠になる。
- ⚠️ **しかし全体として GC 依存が強い**: Mezzano は「GC を OS の中心に据える」設計で、線形性によるリソース管理という発想は持たない。Spinor はここで分岐し、**「GC は Tier 4、線形性が Tier 1」**のハイブリッドモデルを採る (§5)。
- ⚠️ **保守性リスク**: Mezzano は実質単独メンテナで、停止リスクが構造的に高い。Spinor は同じ轍を踏まないため、**最初からセルフホスト前提でブートストラップ手順を書面化する** (§7)。

#### 3.1.2 ブートストラップ手順

Mezzano のブート手順は次の通り (公開情報から再構成)[^mezzano-readme][^mezzano-kboot]:

1. **クロスビルド**: ホストの SBCL で `MBuild` リポジトリを実行し、約 5 分で `mezzano.image` (約 5.4 GB の生ディスクイメージ) を生成
2. **第一回起動**: コールドイメージ内には**最小限のカーネル**のみが含まれる。初回ブート時にネットワーク経由でホストのファイルサーバから自身のソースコードを取得し、**自己コンパイル**を実行
3. **KBoot がカーネルをロード**: BIOS/UEFI どちらでも統一手順。GRUB を経由する場合も `mezzano.mod` 内に KBoot ロジックがポートされている

**Spinor 視点での示唆:**
- ✅ **「コールドイメージを最小化し、初回ブートで自己コンパイル」**というパターンは Linear Spinor のブート設計に直接採用可能。Stage 0 の小ささを維持できる。
- ⚠️ **5 GB の生イメージはやや大きい**: ネットワーク経由のソース取得を初回に限定すれば縮小可能。Spinor では **イメージ内に圧縮済み S 式ソース全体を含める**設計を提案する (§7.2)。

### 3.2 Carp (静的型付け Lisp + 借用検査, 現役)

Carp[^carp-repo] は本書にとって最も近接した既存事例である。「**GC のない Lisp**」を 2016 年から開発し続けており、Spinor が目指す「Lisp + 線形性」の概念実証として既に動いている。

#### 3.2.1 メモリ管理モデル

Carp 公式ドキュメント[^carp-memory] からの抽出:

- **線形型 (Linear types) によるスコープベースの所有権**: 値はそれを生成した関数 / let スコープが所有し、スコープ離脱時に削除される。返り値として渡せばその時点で所有権が移譲される。
- **借用 (Borrowing)**: `&` リーダーマクロまたは `ref` 特殊形式で参照を作る。参照は非線形 (自由にコピー可能)。参照値から線形値を再構築するには `@` (deep copy) を使う。
- **ライフタイム**: 関数シグネチャ上で型変数として明示できる。例: `(sig id (Fn [(Ref String a)] (Ref String a)))`
- **検査タイミング**: 参照の生存期間を 3 種に分類 (`LifetimeOutsideFunction` / `LifetimeInsideFunction` / `LifetimeMixed`)、関数 return 時にダングリング参照がないかを検証
- **デフォルト線形型**: `String`, `Pattern`, `Array`, `Box`, `Function` (これらは GC ではなく所有権で管理される)

例 (Carp ドキュメントから引用)[^carp-memory]:

```lisp
;; ✗ 二重使用は型エラー
(let [string @"linear types!"
      other-string string
      yet-another-string string]  ;; "You're using a given-away value string"
  ())

;; ✓ 借用 + 移動の組合せ
(let [string   @"hello, linear world!"
      reversed (reverse &string)]   ;; & で借用
  (concatenate string reversed))    ;; ここで string が消費される
```

**Spinor 視点での示唆:**
- ✅ **「Lisp 構文 + Rust 流借用検査」が現実に動くことが Carp で実証済み**。Linear Spinor の設計仮説「S 式と所有権は接続できる」は、Carp の存在によって直接の反証材料を持たない。
- ✅ **`&` (借用) と `@` (deep copy) の二項対比は Spinor にも採用可能**: Spinor の既存 `BorrowCheck` モジュールを拡張するだけで実装できる。
- 🔍 **Carp と Linear Spinor の差別化点**:
  - Carp は **ホスト OS (Linux/macOS) 前提**で、ベアメタル動作はサポート対象外。Linear Spinor は OS 自身を記述する。
  - Carp の所有権は **すべての値に等しく適用** (`Int` などの値型は除く)。Linear Spinor は §4 で論じる **線形性階層 (Owned / Borrowed / Shared / Linear / Affine)** を導入し、より細かい制御を可能にする。
  - Carp には **リージョン (arena) 機構がない**。Spinor は既に `EWithRegion` を持つ (§5.3 で OS サブシステム隔離に拡張)。

### 3.3 Henry Baker's Linear Lisp (1992)

「Lively Linear Lisp — 'Look Ma, No Garbage!'」[^baker-paper] は本書の理論的前提となる古典である。要点:

- **線形論理を Lisp に持ち込み、GC を不要にする**ことが可能であることを示した
- **使用回数 = 1 の制約**: すべての変数は厳密に一度だけ参照される
- **`RPLACX` (破壊的更新) を許容**: 線形性の制約下でも更新は安全
- **非線形 Lisp の定数倍以内の性能**: 「behind-the-scenes hash consing」により実現
- **コピー / ガベージコレクションの両方が不要**: 「The Linear Lisp Machine offers many of the same capabilities as combinator/graph reduction machines, but without their copying and garbage collection problems」(検索結果メタ情報経由で確認[^baker-paper])

**Spinor 視点での示唆:**
- ✅ **理論的後ろ盾**: Linear Spinor が「机上の空論」ではないことの 30 年越しの証拠。
- ⚠️ **純粋線形は実用性に難**: Vale の作者の分析[^vale-linearity]によれば、純粋線形型システムは「観察者・コールバック・グラフ構造・相互参照」を扱えない。これらはすべて OS 記述で頻出するパターンであり、**純粋 Linear Lisp はそのままでは OS 用途に使えない**。
- → **解決策**: §4 で論じる **線形性階層**で、線形 (Linear) と非線形 (Shared) を共存可能にする。Linear Haskell[^linear-haskell] が選んだ「**矢印に線形性を付与し、関数の引数 / 返り値レベルで切り替える**」設計を踏襲する。

### 3.4 Rust no_std + ゼロコスト抽象化

Rust の `#![no_std]` モード[^rust-no-std] は Linear Spinor が学ぶべき直接の祖型である:

- **`core` クレートのみに依存**: `std` の libc/OS 抽象を排除し、ベアメタルで動く
- **`#[global_allocator]` 属性**: `GlobalAlloc` トレイトを実装した static 変数を登録することで、`Box` や `Vec` 等の動的アロケーションを no_std 環境で利用可能に[^rust-globalalloc]
- **アロケータ実装の選択肢**: bump、linked-list (`good_memory_allocator`)、fixed-size block、buddy system 等[^phil-opp-allocators]
- **OOM 時の挙動**: `alloc_error_handler` で panic / halt / 再試行を選択可能

phil-opp の "Writing an OS in Rust"[^phil-opp] が示す bare-metal Rust OS の設計パターン:

1. **ブートローダが 4 段ページテーブルを設定** (x86-64 では paging が必須)
2. **Frame allocator がブートローダから受け取った Multiboot info を元に物理フレームを管理**
3. **Heap allocator は GlobalAlloc trait 経由で alloc クレートと接続**
4. **borrow checker が動的アロケーションのエラーを静的に防ぐ**

**Spinor 視点での示唆:**
- ✅ **`GlobalAlloc` の発想を Spinor に持ち込む**: `(define-allocator <impl>)` のような特殊形式で、カーネルのアロケータを置き換え可能にする (テスト時 / 本番時で別実装に切替)
- ✅ **borrow checker による静的エラー防止**は Spinor の `BorrowCheck` の延長で実現可能
- ⚠️ **Rust の `unsafe` キーワードは Spinor に対応物がない**: §6 で `(unsafe ...)` 構文を提案
- ⚠️ **Rust は単一スレッド前提のアロケータが多い**: Spinor は SMP 前提なら lock-free アロケータ (TLSF / mimalloc 風) を最初から設計する必要

### 3.5 Haskell — 「型システムの基層」 としての位置付け

ここまで論じた Mezzano / Carp / Baker / Rust は、Lisp / OS / 線形性のいずれかの軸で Linear Spinor の直接の比較対象となる。一方、**Haskell は「比較対象」ではなく「設計言語の基層」** という独特の位置を占める。Approach 3 は GHC ランタイム依存を切るが、**設計の DNA は Haskell 系である**ことを明示しておく。

#### 3.5.1 Linear Spinor が Haskell から継承するもの

| 要素 | Haskell 由来 | Linear Spinor での扱い |
|---|---|---|
| **Hindley-Milner 型推論 (Algorithm W)** | Haskell 標準 | Spinor の `Infer.hs` で既に実装済み。Linear Spinor でもコア推論器は HM ベースを維持 |
| **Linear types via arrows** | Linear Haskell (Bernardy et al. 2018)[^linear-haskell] | §4.2 の「`Linear` を矢印 (関数型) に付与する」設計。値型ではなく `(Fn [(Linear T)] U)` の形で線形性を表現 |
| **ADT + パターンマッチ** | Haskell の標準機能 | Spinor の `EData` / `EMatch` (Syntax.hs:149-150) に既に存在。OS 記述でも全面活用 |
| **Expr / Val の分離** | Haskell 流の "構文と値を型レベルで分離" | Spinor の `Expr` (Syntax.hs) と `Val` (Val.hs) は意図的に別型。評価前後の状態混同を型で防ぐ |
| **モナディック制御** | `IO` モナド、`State` モナド等 | Spinor の評価器 (`Eval.hs`) は `Either` / `State` モナドで構築。`unsafe` ブロックを `(Unsafe T)` モナド風に表現する案 (§6.1) も Haskell 流 |
| **正格評価の意味論** | Spinor は Haskell と異なり call-by-value を採用するが、**Strict Haskell** (`-XStrict`) の意味論を参考にしている |
| **GADT / Rank-N 多相** | Haskell 拡張機能 | §8.1 で「セルフホストに必要な機能」として明示。Linear Spinor の Phase B 移植時に必要 |
| **Multiplicity (`Multiplicity` 型)** | GHC 9.x の Linear Haskell | §4.2 の Linearity 階層 (`Linear` / `Affine` / `Owned` / `Borrowed` / `Shared`) は GHC の `Many` / `One` 概念を 5 段に拡張したもの |
| **実装言語そのもの** | 現行 Spinor は GHC 9.6 で実装 | Phase A〜D を経て Linear Spinor で再実装するが、**少なくとも Phase D 完了までは Haskell が「コンパイラのコンパイラ」** |

#### 3.5.2 Haskell から継承「しない」もの

逆に、Approach 3 では明示的に手放すものもある:

- ❌ **遅延評価** — call-by-value を維持 (Spinor の現行方針継承)。OS のレイテンシ予測可能性のため。
- ❌ **GC への暗黙依存** — Haskell は GC を前提とした抽象が多い (`Data.IORef` など)。Linear Spinor は §5 の 4 Tier モデルで明示化する。
- ❌ **`unsafePerformIO` 系の「型システム外脱出」** — Rust の `unsafe` ブロックの形 (型レベルで追跡される) で再導入する。
- ❌ **GHC RTS** — これが Approach 3 の出発点。POSIX 抽象、pthread、capability/scheduler はすべて Linear Spinor で再実装。

#### 3.5.3 「Haskell は比較対象か?」の整理

| 観点 | 立ち位置 |
|---|---|
| OS 比較 (Mezzano, Linux 等と並べる) | ❌ Haskell は OS ではない (HaLVM/House は archived[^halvm-status]) |
| 言語比較 (Carp, Rust 等と並べる) | △ 比較可能だが Lisp 系ではない |
| **設計母体** | ✅ **これが本書の主張**。Linear Spinor は「Haskell の意味論を持つ Lisp」 (Spinor のコンセプトそのまま) を OS スコープに広げる試み |

[^halvm-status]: Approach 2 調査 (`labo/research-haskell-unikernel-history.md`) で確認済み。HaLVM は GHC 8.0.2 で停止、2020 年に archived。

### 3.6 比較サマリ

| 軸 | Mezzano | Carp | Linear Lisp (Baker) | Rust no_std | Haskell (GHC) | **Linear Spinor (提案)** |
|---|---|---|---|---|---|---|
| 言語パラダイム | Common Lisp | Lisp + ML | 純粋 Linear Lisp | Rust | 純粋関数型 + Linear (拡張) | **静的型 Lisp + Linear** |
| ベアメタル | ✅ x86-64 | ❌ ホスト OS 前提 | ❌ 学術 | ✅ x86-64/ARM | ❌ (HaLVM/House は停止) | ✅ 設計上 |
| GC | ✅ 世代別コピー | ❌ なし (linear) | ❌ なし | ❌ なし | ✅ 世代別 (GHC RTS) | ✅ **opt-in (Tier 4)** |
| 借用検査 | ❌ | ✅ scope+lifetime | ❌ (純粋 linear) | ✅ NLL | ❌ | ✅ **設計上 (拡張)** |
| 線形型 | ❌ | ✅ default linear | ✅ default linear | ❌ (affine) | ✅ Linear Haskell (arrow) | ✅ **階層的** |
| リージョン | ❌ | ❌ | ❌ | △ (`bumpalo` 等) | △ (実験的拡張のみ) | ✅ **既存 + 拡張** |
| unsafe ブロック | ❌ (全部 trusted) | ❌ | ❌ | ✅ | △ (`unsafePerformIO`) | ✅ **設計上** |
| HM 型推論 | △ (動的型寄り) | ✅ | ❌ | ❌ (明示型) | ✅ **元祖** | ✅ Haskell 由来 |
| ADT / パターンマッチ | ❌ (CLOS) | ✅ | ❌ | ✅ (`enum`) | ✅ **元祖** | ✅ Haskell 由来 |
| セルフホスト | ✅ 99.8% Lisp | △ Haskell ホスト | (未実装) | ✅ rustc-in-Rust | ✅ GHC-in-Haskell | 🎯 **目標** |
| メンテ規模 | 単独 | 小コミュニティ | 学術止 | Linux Foundation 級 | Haskell.org 級 | TBD |

**結論: Linear Spinor の設計空間は埋まっていない。** 「ベアメタル + 線形型 + opt-in GC + リージョン + S 式 + HM 型推論 + unsafe 隔離」を全部満たす言語/OS は現存しない。Mezzano は GC 依存、Carp はホスト OS 前提、Rust は Lisp ではない、Haskell は OS ランタイムへ移植困難。Linear Spinor は **Mezzano の Lisp OS 系譜 + Carp の linear Lisp 実証 + Rust の OS 開発作法 + Haskell の型システム遺産** を統合する初の試みとなる。これが Approach 3 を R&D として進める価値である。

---

## 4. 「Linear Spinor」型システムの構想

### 4.1 既存資産の確認

Spinor は既に以下を実装済み:

```haskell
-- src/Spinor/Type.hs
data Linearity = Linear | Unrestricted
data Type = ... | TLinear Linearity Type | ...

-- src/Spinor/BorrowCheck.hs
data VarState = Owned | Borrowed | Consumed | Dropped
data BorrowError = DoubleUse ... | Unconsumed ... | UseAfterMove ...
```

これは **Wadler の「Linear types can change the world」を Lisp に持ち込んだ Carp の延長線** にあり、Linear Spinor の出発点として十分に機能する。本書は次の 4 つの拡張を提案する。

### 4.2 線形性修飾子の階層化

Carp は事実上「全部 Linear」の単純なモデルだが、OS 記述の現実 (グローバル状態、共有メモリ、コールバック登録) を考えると階層が必要である。次の 5 段階を提案する:

| 修飾子 | 意味 | 使用回数 | デフォルトドメイン |
|---|---|---|---|
| `Linear` | 厳密に **一度だけ** 使用 | == 1 | リソースハンドル (ファイル / ソケット / DMA バッファ) |
| `Affine` | 高々 **一度** 使用 (drop 可) | <= 1 | ローカル所有値 |
| `Owned` | スコープ内で所有、`drop` 自動挿入 | 任意 (move まで) | デフォルトの値型 |
| `Borrowed<'a>` | 別の値からの借用、ライフタイム `'a` 内有効 | 任意 (read-only) | 関数引数 |
| `Shared` | GC 管理対象、共有可能 | 任意 | 高水準データ構造 (Tier 4) |

S 式上の表現案:

```lisp
;; 関数シグネチャでの線形性指定
(sig open-file (Fn [(Borrowed String)] (Linear FileHandle)))

;; 線形値の宣言
(linear (let ((handle (open-file &"foo.txt")))
  (read-line handle)        ;; ← ここで handle は consume される
  ;; (read-line handle)     ;; ← これがあるとコンパイルエラー
  ))

;; 借用
(let ((s (string-new "hello")))
  (string-length &s)        ;; & で借用
  s)                        ;; 元の所有権は維持される
```

**設計判断:**
- **GHC の Linear Haskell に倣う**[^linear-haskell]: 線形性を **値ではなく矢印 (関数型)** に付与する。`(Fn [(Linear T)] U)` は「`T` を線形に消費して `U` を返す関数」を意味する。
- これにより既存の Spinor 関数 (Unrestricted) との後方互換性を維持しながら、OS カーネル記述部分だけ漸進的に Linear 化できる
- `Linear` と `Affine` の区別: `Linear` は「忘れてはいけない」(ファイルハンドルのクローズ漏れ検出用)、`Affine` は「忘れてもよい」(一般のローカル値)

### 4.3 借用と参照

Carp の `&` / `@` 二項対比を踏襲しつつ、Mezzano の `+address-tag-pinned+` 概念を取り込む:

| 構文 | 意味 |
|---|---|
| `&value` | 不変借用 (read-only)。`Borrowed<'a>` 型を生成 |
| `&mut value` | 可変借用。`BorrowedMut<'a>` 型。同時に存在できるのは唯一 1 つ |
| `@value` | deep copy で新たな線形値を生成 |
| `(pin value)` | GC 移動の対象外にする (`+address-tag-pinned+` 相当)。DMA / 割り込みハンドラでの使用 |
| `(forget value)` | 線形値を意図的に drop しない (リーク。型システムで明示) |

**規則 (Rust NLL に準拠):**
- 同時借用は「不変借用 N 個」OR「可変借用 1 個」のいずれか (排他)
- 借用の生存期間は元の所有値の生存期間以下 (関数 return 時にダングリング検査)

### 4.4 ライフタイムとリージョン

既存の `EWithRegion sp regionName body` (Syntax.hs:154) を **2 つの役割**で再活用する:

#### 4.4.1 構文糖としての借用ライフタイム

```lisp
;; リージョン r 内のすべての借用は、r のスコープ内で有効
(with-region r
  (let ((buf (alloc-in r (make-buffer 4096))))
    (process &buf)))
```

リージョン名 `r` は内部的にライフタイム変数として扱われる。Carp が型シグネチャで陽に書く `(Ref String a)` は、Spinor では `with-region` で暗黙化される。

#### 4.4.2 OS サブシステム隔離リージョン (新規)

Linear Spinor 独自の拡張。OS のサブシステム (ネットワーク / ファイルシステム / GUI) ごとに **独立したリージョンを永続割り当て**し、サブシステム間の値の受け渡しは明示的なシリアライズ (`(transfer src-region dst-region value)`) を要求する:

```lisp
;; サブシステムリージョンの宣言
(define-region net-region   :tier 1 :gc nil)
(define-region fs-region    :tier 1 :gc nil)
(define-region gui-region   :tier 4 :gc t)   ;; GUI は GC あり

;; サブシステム間通信は明示
(define (handle-http-request req)
  (let* ((body  (http/read-body req))           ;; net-region 内
         (path  (transfer net-region fs-region (parse-path body))))
    (fs/write-file path body)))                  ;; fs-region 内
```

**効果:**
- **障害局所化**: GUI が暴走しても net-region には影響しない (リージョン破棄で復旧)
- **メモリリーク追跡**: どのリージョンが膨張したかが OS レベルで観測可能
- **GC の段階導入**: Tier 1 (kernel) は GC レス、Tier 4 (アプリ層) のみ GC、を強制可能 (§5)

### 4.5 借用チェッカーのコンパイルフェーズ統合

既存の `BorrowCheck` モジュールは AST を 1 パスで走査するシンプルな実装である。Linear Spinor では **コンパイラパイプラインの正式なフェーズ**として位置付ける:

```
Parse (Syntax.hs)
  ↓ Expr
Expand (Expander.hs)        — マクロ展開
  ↓ Expr (展開済み)
Infer (Infer.hs)            — Hindley-Milner 型推論
  ↓ TypedExpr
LinearityCheck (NEW)        — 線形性ルールの推論 (本書 §4.2 のタイプ階層)
  ↓ LinearTypedExpr
BorrowCheck (拡張)          — ライフタイム / 借用 / move の検査
  ↓ ValidatedExpr
EscapeAnalysis (拡張)       — リージョン逃避検査 + サブシステム境界検査
  ↓ ValidatedExpr
Codegen / LLVM              — Linear Spinor IR への落とし込み
```

**新規追加が必要な解析:**

1. **線形性推論 (Linearity Inference)**: `Linear` や `Affine` の修飾子が明示されていない式について、使用回数を解析して自動的に推論する (Linear Haskell 風の"unrestricted by default, strict on annotated arrows" モデル)
2. **借用グラフ構築**: Rust NLL[^oxide-paper] に倣い、各借用の生存範囲を制御フロー上で計算
3. **drop 順序の決定**: スコープ離脱時に複数の Owned 値を drop する順序を、依存関係 (借用関係) に基づいて静的に決定。これにより C codegen 時の `free()` 呼び出しが完全に静的に決まる

### 4.6 例: ドライバ層の二重解放回避

最も典型的な「Linear Spinor の利益」を示す例:

```lisp
;; ❌ Approach 1 (現行 C codegen) では runtime での leak / use-after-free が発生し得る
(define (legacy-driver dev)
  (let ((buf (dma-alloc 4096)))
    (configure-device dev buf)
    (release-buffer buf)
    (release-buffer buf)))    ;; 二重解放: コンパイル時に検出されない

;; ✅ Linear Spinor では型システムで弾かれる
(sig dma-alloc       (Fn [Int]                   (Linear DmaBuffer)))
(sig release-buffer  (Fn [(Linear DmaBuffer)]    Unit))

(define (linear-driver dev)
  (let ((buf (dma-alloc 4096)))
    (configure-device dev &buf)        ;; 借用なので buf は consume されない
    (release-buffer buf)))             ;; ここで buf を consume
    ;; (release-buffer buf)            ;; ← これを書くと "Linear value 'buf' used more than once"

;; ✅ 解放忘れもコンパイル時に検出
(define (forget-driver dev)
  (let ((buf (dma-alloc 4096)))
    (configure-device dev &buf)))
    ;; ↑ 'buf' must be consumed before scope ends
```

これは既存の `BorrowCheck.hs` の `DoubleUse` / `Unconsumed` エラーが **すでにそのまま使える**ことを意味する (今ある実装の射程が、本格的な OS 利用に届いている)。

---

## 5. ハイブリッド・メモリ管理アーキテクチャ

### 5.1 4 層メモリモデル

Linear Spinor は **「線形性が下、GC が上」** の階層を採る。各 Tier は独立したアロケータと責任モデルを持つ。

| Tier | 名称 | 管理方式 | 用途 | アロケータ実装案 |
|---|---|---|---|---|
| **Tier 0** | **Static / BSS** | リンク時固定 | カーネルコード、固定テーブル | リンカスクリプト |
| **Tier 1** | **Owned (Linear)** | 線形型による静的 drop | デバイスドライバ、割込みハンドラ、ハードウェアリソース | bump (関数フレーム上) |
| **Tier 2** | **Region (Arena)** | リージョン破棄で一括解放 | リクエスト処理、サブシステム単位の状態 | TLSF ベースの arena |
| **Tier 3** | **Pinned** | 明示的 (un)pin、Mezzano の `+address-tag-pinned+` 相当 | DMA バッファ、固定アドレスを要求するもの | 専用 frame allocator |
| **Tier 4** | **Shared (GC)** | 世代別コピー GC (Mezzano 風) | アプリ層、REPL のヒストリ、ユーザデータ | 世代別コピー GC |

**設計原則: 上の Tier から下の Tier への参照は禁止される (倒立)。**

理由: Tier 4 (GC) のオブジェクトは GC で移動する可能性があるため、Tier 1 (Linear) からポインタで参照すると壊れる。逆方向 (Linear → GC) は安全 (GC は Linear 領域を「ルート」として扱える)。

### 5.2 Tier 間の安全な通信

異なる Tier の値を受け渡す手段を、すべて型レベルで制御する:

```lisp
;; ✅ Tier 1 (Owned) → Tier 4 (Shared) は move で OK
(define (tier1-to-tier4 (linear data))
  (gc-box data))    ;; ここで GC 領域に移譲、元の data は consume

;; ✅ Tier 4 (Shared) → Tier 1 (Owned) は deep copy が必要
(define (tier4-to-tier1 (shared data))
  (linearize @data))    ;; @ で deep copy → 新しい線形値

;; ❌ Tier 1 → Tier 4 を借用で繋ぐのは禁止 (型エラー)
(define (bad-bridge (linear data))
  (gc-register-callback (lambda () &data)))    ;; ← lifetime error

;; ✅ Tier 3 (Pinned) は明示的にロックする
(with-pin ((dma-buf (alloc-pinned 4096)))
  (start-dma device dma-buf)        ;; この間 GC でも動かない
  (wait-dma device))                ;; with-pin 終了で unpin
```

**型シグネチャでの Tier 表現案:**

```
TLinear Linear T          → Tier 1
TRegion 'r T              → Tier 2 (リージョン 'r の中)
TPinned T                 → Tier 3
TShared T                 → Tier 4
```

### 5.3 リージョンによる OS サブシステム隔離

§4.4.2 で提案したサブシステムリージョンを、メモリ管理の観点から再定義する。

```
┌─────────────────────────────────────────────┐
│ Tier 4: Shared GC heap                      │   ← REPL / アプリ
├─────────────────────────────────────────────┤
│ Tier 2: Region Arena                        │
│   ┌──────────┬──────────┬──────────┐        │
│   │ net-rgn  │ fs-rgn   │ gui-rgn  │  ...   │   ← サブシステムごと
│   └──────────┴──────────┴──────────┘        │
├─────────────────────────────────────────────┤
│ Tier 3: Pinned (DMA, MMIO mappings)         │
├─────────────────────────────────────────────┤
│ Tier 1: Linear Owned (kernel / drivers)     │
├─────────────────────────────────────────────┤
│ Tier 0: Static / BSS (kernel image)         │
└─────────────────────────────────────────────┘
```

**OS 設計上のメリット:**
- **マイクロカーネル的隔離をリージョン経由で実現**: プロセス境界 (POSIX 概念) の代わりにリージョン境界を使う。MMU を介さず型チェックで隔離するため、**コンテキストスイッチコストがゼロ**
- **ホットスワップ可能**: あるサブシステムが壊れた場合、そのリージョンのみ破棄して再構築可能 (Mezzano の "live recompile" 哲学を OS レベルで構造化)
- **デバッグ可観測性**: `(region-stats net-rgn)` でリージョン使用量を即時確認可能

### 5.4 GC アルゴリズムの選定 (Tier 4)

Tier 4 のみが GC 対象。Mezzano の世代別コピー GC[^mezzano-gc] を出発点とし、**Immix**[^immix] (mark-region) を比較対象に検討する:

| 候補 | 利点 | 欠点 | 採用判定 |
|---|---|---|---|
| **世代別コピー (Mezzano 流)** | 実装事例あり、若い世代の収集が高速 | コピーコスト、半空間で実メモリ 2 倍 | ✅ Stage 1 採用 |
| **Immix mark-region** | コピーレスでフラグメンテーション抑制 | 実装複雑度高 | △ Stage 2 検討 |
| **Boehm 保守的 GC** | 既存実装利用可能、C との互換性 | 精度低 (false retention)、Spinor 哲学に反する | ❌ 不採用 |
| **MMTk (Plan-based)** | 複数 GC をプラガブルに選択可能 | Rust 実装、Spinor からの ffi コスト | △ 長期検討 |

**判断:** Mezzano が実機で動かしている世代別コピー GC のスキームを Spinor で再実装する。`+address-tag-cons+` のような cons 専用領域も導入し、リスト操作の局所性を確保する。

### 5.5 オブジェクト表現 (タグ付きポインタ案)

64-bit ワード上のレイアウト案 (Mezzano + LuaJIT 風 NaN-boxing 検討):

```
63                   48 47                                3 2 1 0
┌──────────────────────┬──────────────────────────────────┬─────┐
│   Tier tag (16 bit)  │   Address (45 bit, 8-aligned)    │ tag │
└──────────────────────┴──────────────────────────────────┴─────┘
                                                            │ │ └─ fixnum bit (0 = pointer, 1 = fixnum)
                                                            │ └─── cons bit (Mezzano 風 cons 専用領域フラグ)
                                                            └───── pinned bit
```

| Tier タグ (上位 16 bit) | 意味 | Mezzano 対応 |
|---|---|---|
| `0x0001` | Tier 1 Linear (Owned) | (新規) |
| `0x0002` | Tier 2 Region | (新規) |
| `0x0003` | Tier 3 Pinned | `+address-tag-pinned+` |
| `0x0004` | Tier 4 Shared (GC) | `+address-tag-general+` |
| `0xFFFx` | Immediate (fixnum, char, single-float) | (Mezzano 同等) |

**留意:**
- x86-64 の正式仮想アドレスは 48 bit (canonical address) なので、上位 16 bit を tag に使う設計は AMD64 仕様上は妥当だが、Intel LAM (Linear Address Masking) や AArch64 TBI (Top-Byte Ignore) が普及するまで「ハードウェアが Tier タグを無視してくれる」とは限らない。**仮想アドレス空間を意図的に下位 48 bit に制限してタグ付け**する Mezzano と同じ作戦が現実的。
- **`Bool` `Char` `Int` (small) は immediate** (ヒープに置かない)。Spinor の `VInt Integer` は現状ボックス化されているが、Linear Spinor では fixnum (61-bit など) と bignum を分離する。

---

## 6. システム記述用プリミティブ (`unsafe` レイヤー)

OS は **究極的には型システムを破る** (MMIO レジスタ書込み、割込みベクタ書換、ページテーブル操作) 必要がある。Rust の `unsafe` ブロックに倣い、これを **型レベルで隔離・追跡する**仕組みを提案する。

### 6.1 `(unsafe ...)` 特殊形式

```lisp
;; unsafe ブロック内でのみ生メモリ操作が許可される
(unsafe
  (poke64 0xFEE000B0 0x000000FB)   ;; APIC EOI レジスタ
  (peek64 0xFEC00010))              ;; IOAPIC RedirectionTable
```

**型システム上の扱い:**
- `unsafe` 式は型シグネチャに `Unsafe` フラグを伝播させる
- `unsafe` 関数を呼ぶ側も `unsafe` ブロック内にいなければエラー
- **safe 関数の中で unsafe ブロックを「閉じ込める」**ことで、OS の安全インタフェースを構築する

```lisp
;; safe な公開 API、内部で unsafe を使用
(sig apic-eoi (Fn [] Unit))
(define (apic-eoi)
  (unsafe (poke64 +apic-eoi-register+ 0)))   ;; 内部の unsafe を隠蔽

(sig handle-irq (Fn [(Borrowed Cpu)] Unit))
(define (handle-irq cpu)
  (process-interrupt &cpu)
  (apic-eoi))    ;; ← safe なので unsafe ブロックなしで呼べる
```

### 6.2 物理アドレス型と MMIO

Rust の `core::ptr` に対応する型を、Spinor の型階層に追加する:

| 型 | 用途 | 演算 |
|---|---|---|
| `PhysAddr` | 物理アドレス | `(phys-add)`, `(phys-to-virt)` (unsafe) |
| `VirtAddr` | 仮想アドレス | `(virt-add)`, `(virt-to-phys)` (unsafe) |
| `Mmio T` | メモリマップド I/O 領域 | `(mmio-read mmio offset)`, `(mmio-write mmio offset val)` (unsafe) |
| `RawPtr T` | 生ポインタ | `(deref ptr)` (unsafe), アライメントチェック静的 |

これらは **すべての操作が `unsafe` を要求する**ことで、誤用を防ぎつつ OS の正当な要求 (デバイス操作) を可能にする。

### 6.3 インラインアセンブリ

x86-64 の `cli` / `sti` / `hlt` / `wrmsr` / `rdtsc` 等を呼ぶ手段が必要:

```lisp
;; アセンブリのテンプレート構文 (Rust の asm! 風)
(unsafe
  (asm "cli"))                                ;; 割込み無効化

(unsafe
  (asm "wrmsr"
       :in  (("ecx" msr-num) ("eax" lo) ("edx" hi))
       :clobber ("memory")))

(define (rdtsc)
  (unsafe
    (asm "rdtsc"
         :out (("eax" lo) ("edx" hi))
         :returns (or-shift hi 32 lo))))
```

Spinor のマクロシステムを拡張し、アセンブリ命令ごとに safe wrapper を生成する仕組みを `(define-asm-primitive ...)` として整備する。

### 6.4 `(forget)` と `mem::forget` 相当

線形値を意図的に drop しないケース (例: `Box::leak` 相当) は明示的に書く:

```lisp
;; OS の起動時に確保した制御構造体を永続させる
(define +global-cpu-table+
  (forget (alloc-cpu-table 256)))    ;; intentional leak; 起動時のみ
```

**`forget` は unsafe ではない** (メモリ安全性は保たれる、リソースが解放されないだけ) が、**`forget` の出現箇所はリンタ警告で集約レポート**するべき。

---

## 7. ベアメタルブートと初期化

### 7.1 ブートローダ選定

| 候補 | 採用判定 | 理由 |
|---|---|---|
| **Limine** | ◎ 第一候補 | モダン、x86-64 + AArch64 対応、Higher-half kernel をデフォルト、UEFI/BIOS 両対応 |
| **GRUB 2 (Multiboot 2)** | ○ 第二候補 | 普及度高、しかし設定が冗長 |
| **KBoot (Mezzano 流)** | △ | 動作実績あり、コミュニティ規模が小さい |
| **自作ブートローダ** | × (将来) | Stage 0 を Spinor で書くのは Stage 5+ の楽しみ |

Stage 0/1 では **Limine** を採用し、Spinor カーネルを `kernel.elf` として `limine.cfg` でロード可能にする。リンカスクリプトで higher-half (`0xFFFFFFFF80000000` 〜) に配置。

### 7.2 ブート段階構成

| Stage | 名称 | 入力 | 出力 | 含む内容 |
|---|---|---|---|---|
| 0 | **Mini Bootstrap** | Limine + ELF | ページング ON、コンソール初期化 | Spinor インタプリタの **C 移植版** (1 万行以下) |
| 1 | **Cold Image** | Stage 0 + 圧縮済み Spinor ソース | 自己コンパイル後のフルカーネル | Spinor カーネル全体 (圧縮 S 式で同梱) |
| 2 | **Warm Image** | Stage 1 + ユーザデータ | 永続化されたフルシステム | Tier 4 GC ヒープも含む |

Mezzano の手法 (5 GB の生イメージ + ネット経由ソース取得) を改良し、**圧縮 S 式 (zstd) を ELF セクションに埋め込み**、Stage 0 が自己展開・自己コンパイルするモデルとする。

```
Limine
   ↓ (ELF load + paging setup)
Stage 0: Mini Spinor Interpreter (C)        ← Approach 1 の codegen で生成可
   ↓ (decompress + parse + eval kernel sources)
Stage 1: Self-hosted Linear Spinor Kernel
   ↓ (initialize Tier 4 GC, sub-system regions)
Stage 2: User REPL on bare metal
```

### 7.3 自己コンパイル戦略

Stage 0 の "Mini Spinor" は機能を意図的に絞る:

| 機能 | Stage 0 | Stage 1 |
|---|---|---|
| Parser | ✅ | ✅ |
| Evaluator | ✅ (treewalking) | ✅ (LLVM JIT) |
| Type inference | ❌ (untyped) | ✅ (HM + Linear) |
| Borrow check | ❌ | ✅ |
| GC | ✅ (mark-sweep, conservative) | ✅ (世代別コピー) |
| Codegen | ❌ | ✅ (LLVM AOT) |

Stage 0 は **「すでにコンパイル済みの Stage 1 カーネルソースを評価できる最小評価器」**に徹する。Stage 1 が立ち上がったら Stage 0 のメモリ領域は破棄され、以降は Linear Spinor が支配する。

---

## 8. セルフホストへのロードマップ

Linear Spinor を Linear Spinor で書く (= GHC への依存を完全に切る) ためのギャップ分析。

### 8.1 現状の Spinor (Haskell 製) で不足している機能

| 機能 | 現状 | セルフホストに必要な拡張 |
|---|---|---|
| **線形性 / 借用検査** | あり (Tier 0) | ライフタイム変数 / NLL / リージョン推論を実装 (§4.5) |
| **ADT (代数的データ型)** | `EData` あり | GADT、ランク 2 多相 (型推論器・コンパイラの記述に必要) |
| **マクロ系** | `EMacro` あり | **マクロのマクロ** (マクロ生成マクロ)、phase separation の厳格化 |
| **モジュールシステム** | あり (`packages/`) | 循環依存検出、recursive modules、別コンパイル単位への参照 |
| **低レベル型** | `Int`, `Bool` のみ (boxed) | `U8` `U16` `U32` `U64`, `I8`...`I64`, `F32` `F64` (unboxed)、`PhysAddr` (§6.2) |
| **配列** | `VList` (linked list) | `(Array T n)` (固定長), `(Vector T)` (動的) — どちらも unboxed 表現 |
| **構造体 / レコード** | `VData Text [Val]` | フィールド名アクセス、unboxed フィールドレイアウト制御 |
| **FFI** | なし (Haskell プリミティブで吸収) | C ABI 互換の `(extern-c ...)` (Stage 0 用)、最終的には不要に |
| **インラインアセンブリ** | なし | §6.3 |
| **生ポインタ操作** | なし | §6.2 |
| **コンパイラ自身のメタプログラミング** | 限定的 | `(quote-type ...)` (型を値として扱う)、コンパイル時評価 (`comptime`) |

### 8.2 段階的セルフホスト計画

```
Phase A: コンパイラの一部を Spinor で書く (現在)
   - twister/ で標準ライブラリを Spinor 化済み
   - 次: パーサ → Spinor 化 (megaparsec を Spinor で再実装)

Phase B: 評価器・型推論器を Spinor で書く
   - 評価器は最も simple、まず移植
   - 型推論 (HM + Linear) は 2-3 千行規模
   - GC は Tier 4 のみ、自前実装

Phase C: コード生成 (LLVM IR / Native) を Spinor で書く
   - Codegen.hs (~数千行) を移植
   - LLVM IR の S 式表現を整備

Phase D: GHC 依存ゼロ達成
   - bootstrap: Linear Spinor で書かれたコンパイラを、現行の Haskell 製 Spinor でビルド
   - 自己ビルド: 自身でコンパイルして fixed point に到達
   - GHC は完全に不要に

Phase E: ベアメタル化 (本書 §7 の Stage 0/1 構築)
   - Stage 0 を最小 C で記述 (1 万行以下を目標)
   - Stage 1 以降は Linear Spinor で書く
```

### 8.3 自己言及的データ構造

セルフホストで避けて通れない「**Spinor のコンパイラが Spinor のコンパイラを表現する**」問題。Lisp の伝統的な強み (S 式 = データ = コード) を最大限活用するが、線形性の導入で次の点に注意:

- **AST ノードは Linear か Shared か?** — 推奨: Tier 4 (Shared/GC)。コンパイラの中間表現は共有が多いため、線形にすると不便。`(define-data Expr ...)` を `:tier shared` で宣言。
- **マクロ展開は Linear で記述可能か?** — **YES**。展開関数は `(Fn [(Linear Expr)] (Linear Expr))` として定義し、入力 AST を消費して新 AST を生成する。Linear Haskell の Linear Lens と同じ発想。
- **型推論器の自己参照** — 型 `Type` が型 `Type` への参照を持つ場合、Tier 4 の GC で循環参照を解消する。Tier 1 (Linear) では循環参照は表現できない (これは設計上の制約として受け入れる)。

---

## 9. 比較マトリクス: Mezzano / Carp / Rust OS / Haskell / Linear Spinor

| 軸 | Mezzano | Carp | Rust no_std OS | Haskell (GHC) | **Linear Spinor (提案)** |
|---|---|---|---|---|---|
| ベースパラダイム | Common Lisp | ML 風 Lisp | Rust | 純粋関数型 + Linear (拡張) | 静的型 Lisp + 線形 |
| 型推論 | △ (動的型寄り) | ✅ HM | ❌ (明示型ベース) | ✅ HM (元祖) | ✅ HM (Haskell 由来) |
| ADT / パターンマッチ | △ (CLOS) | ✅ | ✅ (`enum` + `match`) | ✅ (元祖) | ✅ (Haskell 由来) |
| メモリ管理 | 世代別 GC のみ | Linear のみ | Affine + alloc クレート | 世代別 GC (RTS) | **4 Tier ハイブリッド** |
| 借用検査 | ❌ | ✅ scope-based + lifetime | ✅ NLL | ❌ | ✅ NLL + リージョン |
| 線形型 | ❌ | ✅ default linear | ❌ (affine) | ✅ Linear Haskell (arrow) | ✅ 階層的 (5 段) |
| リージョン | ❌ | ❌ | △ (`bumpalo` 等) | △ (実験的拡張) | ✅ **OS サブシステム隔離** |
| unsafe 隔離 | ❌ (全部 trusted) | ❌ | ✅ `unsafe` block | △ (`unsafePerformIO`) | ✅ `(unsafe ...)` 形式 |
| インラインアセンブリ | ✅ (Lisp form) | △ | ✅ `asm!` macro | △ (FFI 経由) | ✅ `(asm ...)` form |
| タグ付きポインタ | ✅ (`+address-tag-*+`) | ❌ (素の C) | △ (任意) | △ (RTS 内部) | ✅ Tier 別 + immediate |
| ベアメタル | ✅ x86-64 + AArch64 | ❌ ホスト OS | ✅ x86-64 + ARM | ❌ (HaLVM/House 停止) | ✅ (設計上) |
| ブートローダ | KBoot | (該当なし) | bootloader crate / Limine | (該当なし) | Limine |
| セルフホスト | ✅ 99.8% Lisp | △ (Haskell ホスト) | ✅ rustc-in-Rust | ✅ GHC-in-Haskell | 🎯 (Phase D) |
| ライセンス | MIT | Apache 2.0 | MIT/Apache | BSD-3 | (TBD) |
| メンテ規模 | 単独 | 小 | 巨大 | 巨大 | 小 (Spinor チーム) |
| 既存 LoC | ~50万 (Lisp) | ~3万 (Haskell+Carp) | (Linux: ~3000万) | ~80万 (GHC + base) | ~1万 (Haskell) → 目標 ~5万 (Linear Spinor) |

**所感:** Linear Spinor は **4 つの先行系譜の交差点に位置する**:
- **Mezzano** (Lisp OS 系) からは **オブジェクト表現とブート手順**
- **Carp** (linear Lisp) からは **Lisp + 借用検査の実証**
- **Rust no_std** (system 系) からは **借用検査・unsafe・no_std 開発作法**
- **Haskell** (型システム系) からは **HM 型推論・ADT・linearity-on-arrows・実装言語**

このうち **どの 1 つも、Linear Spinor が目指す「ベアメタル + 線形型 + opt-in GC + リージョン + S 式 + HM 型推論 + unsafe 隔離」の全体を満たさない**。Mezzano は GC 依存が強く、Carp はホスト OS 前提、Rust は Lisp ではない、Haskell は OS ランタイムへの移植が構造的に停止している ([Approach 2 史的調査](../research-haskell-unikernel-history.md))。Linear Spinor の独自性は **「これら 4 つを統合する」**点にあり、ここに R&D の価値がある。

---

## 10. ロードマップと推奨される次のステップ

短期 (3 ヶ月) → 中期 (1 年) → 長期 (3 年〜) の段階整理。

### 短期 (Phase R0: 基盤強化)

| # | タスク | 関連 Issue | 目的 |
|---|---|---|---|
| R0-1 | **線形性の修飾子階層 (Affine 追加)** を `Type.hs` に実装 | (新規 Issue) | §4.2 の階層化を着手 |
| R0-2 | **借用構文 `&` / `@` のパーサ追加** | (新規) | §4.3 の借用導入 |
| R0-3 | **`BorrowCheck` をライフタイム対応に拡張** | (新規) | §4.5 の NLL 化 |
| R0-4 | **`(unsafe ...)` 構文の AST 追加** | (新規) | §6.1 の素地 |
| R0-5 | **既存リージョンを Tier 化** (`:tier 1 :gc nil` 等の宣言追加) | (新規) | §5.3 のサブシステム隔離 |

これらは **既存実装の小増分** であり、Approach 1/2 のいずれにも悪影響を与えずに進められる。

### 中期 (Phase R1: Linear Spinor 言語の確立)

| # | タスク | 目的 |
|---|---|---|
| R1-1 | unboxed 型 (`U8` ... `F64`) と固定長配列の実装 | OS 記述プリミティブ |
| R1-2 | `(asm ...)` 形式 + Codegen での LLVM inline asm マッピング | §6.3 |
| R1-3 | `PhysAddr` / `VirtAddr` / `Mmio` 型の標準ライブラリ化 | §6.2 |
| R1-4 | Tier 4 GC (世代別コピー) の `runtime/spinor.c` への実装 | Mezzano 風 GC を C で書く |
| R1-5 | Limine 連携の Stage 0 PoC ("Hello, Linear Spinor OS!") | §7.1-§7.2 |

### 長期 (Phase R2: セルフホストとブートストラップ)

| # | タスク | 目的 |
|---|---|---|
| R2-1 | パーサ (`Syntax.hs`) を Linear Spinor で書き直す | Phase A |
| R2-2 | 型推論器 (`Infer.hs`) を Linear Spinor で書き直す | Phase B |
| R2-3 | コード生成器 (`Codegen.hs`, `LLVM.hs`) を Linear Spinor で書き直す | Phase C |
| R2-4 | GHC ゼロ達成 (Phase D) | 純粋性の達成 |
| R2-5 | Stage 0 を 1 万行以下の C で再実装、Stage 1 以降を Linear Spinor 化 | §7.3 |

---

## 11. リスクと未解決課題

### 11.1 設計レベルのリスク

| リスク | 評価 | 緩和策 |
|---|---|---|
| **線形性が Lisp の動的さを殺す** | ⚠️ 中 | Tier 4 (GC + Shared) を残し、REPL とアプリ層は伝統的 Lisp 体験を維持 |
| **借用検査のエラーメッセージが不可解になる** | ⚠️ 高 | Rust の "NLL diagnostic" 改善の知見をフル活用、AI 補助も検討 |
| **ベアメタル開発の検証が困難** | ⚠️ 高 | QEMU + GDB の早期統合、`cabal test` から QEMU 起動できる設計 |
| **セルフホストに 3 年以上かかる** | ⚠️ 中 | Phase A〜D を並列ではなく逐次進行、Approach 1/2 と独立 |

### 11.2 技術的未解決課題

- **割込み中の借用検査セマンティクス**: 割込みハンドラから線形値を扱うとき、メイン実行コンテキストの線形性カウントとどう整合させるか? — Rust の `Send`/`Sync` に相当する trait が必要
- **アロケータの自己ホスト**: Tier 4 の GC を Linear Spinor で書く場合、GC 自身が GC 対象になってはならない (chicken-and-egg)。`(no-gc ...)` のようなアノテーションが必要
- **マルチコア (SMP)**: 線形値のコア間移送、lock-free アロケータ、TLS (Thread-Local Storage) 相当の概念が未設計
- **デバッガ統合**: 線形値の "consumed" 状態を REPL からインスペクトする手段
- **AI-Native 統合 (`(ask-llm ...)`)**: ビジョンドキュメントで掲げた LLM 統合と Linear Spinor の関係は本書未踏

### 11.3 さらなる調査 (TODO)

> **TODO (要追加調査):**
> - Henry Baker のオリジナル論文 PDF[^baker-paper] の本文取得 (本調査では検索結果メタ情報経由のみ確認)。WebFetch では PDF バイナリが読めなかったため、印刷版 / OCR 経由で本文を確認し、`hash-cons` の具体的アルゴリズムを Linear Spinor のコピー回避戦略に活かせるか確認する。
> - Mezzano の compiler/ ディレクトリ内の SSA ベースバックエンド[^mezzano-readme]の詳細 (どの IR 表現か、reg alloc スキーム) を一次ソースで確認する。Linear Spinor の LLVM 経由路線と Mezzano 流の自前 SSA 路線を比較するため。
> - Linear Haskell[^linear-haskell] の `Multiplicity` 概念の最新実装 (GHC 9.x) と、それが Spinor の `Linearity` enum をどう拡張すべきかの示唆。
> - **Mezzano が "Make x86-64 work on AArch64" を実機で動かしている**[^mezzano-readme]点について、移植時に問題になった主要事項 (タグ表現の bit width、ページサイズ差異 4K vs 16K 等) の一次ソース。Spinor の AArch64 対応設計に直接関係する。
> - 形式検証された OS (seL4, Verus) の知見を Linear Spinor の借用検査の健全性証明にどう取り込めるか。
> - Vale の "Generational References"[^vale-linearity] と Linear Spinor の ライフタイム + `+address-tag-pinned+` の組み合わせの優劣比較。

---

## 12. 結論

「Linear Spinor」は、**Spinor が既に持っている断片的な要素 (`Linearity`, `BorrowCheck`, `EscapeAnalysis`, `EWithRegion`) を、ベアメタル OS 記述に耐える首尾一貫した型システムへと結晶化させる構想** である。

設計の独自性 (4 系統の融合):
1. **GC は opt-in** (Tier 4 のみ)。Mezzano が「Lisp = GC 必須」を破った先に Spinor が立つ。
2. **線形 + 借用 + リージョン + GC の 4 Tier ハイブリッド**。Carp の "全部 linear" でも、Mezzano の "全部 GC" でもない。
3. **OS サブシステムをリージョンで隔離**。MMU を介さない型レベルの隔離は、コンテキストスイッチコストゼロのマイクロカーネルを意味する。
4. **`unsafe` を Spinor の S 式に持ち込む**。Rust の `unsafe` ブロックを Lisp 構文で再構成し、安全と性能の境界線を型上で可視化する。
5. **Haskell の型システム遺産を Lisp の構文に統合**。HM 型推論 + Linear Haskell の "linearity on arrows" + ADT を S 式上で再構成。Spinor の「Lisp の構文 + Haskell の意味論」というプロジェクト原点を、OS 記述レベルへ自然に拡張する。
6. **セルフホスト前提**。Mezzano が抱える「単独メンテナリスク」と Approach 2 の「GHC fork 保守地獄」(HaLVM の死因) を、最初から GHC 依存ゼロを目指すことで構造的に回避する。

100 年後の Lisp マシンに最も近い設計だと我々は考える。Approach 1 が短期に Spinor アプリを動かし、Approach 2 が中期に REPL を持ち込むなら、**Approach 3 は「Spinor が言語であることを超えて『計算機の在り方』を再定義する」** 試みである。

本書はホワイトペーパーであり、即時の実装計画ではない。しかし §10 の Phase R0 (短期) の 5 タスクは、**既存実装の小拡張** で達成可能で、Approach 1/2 と並行して進められる。これらが揃った時点で、Linear Spinor は「思考実験」から「コミット可能な R&D ブランチ」へと格上げできる。

---

## 付録 A: コード擬似例の総まとめ

### A.1 デバイスドライバの Linear 実装

```lisp
;; ─── 型シグネチャ ─────────────────────────
(sig dma-alloc       (Fn [Int]                             (Linear DmaBuffer)))
(sig configure-net   (Fn [(Borrowed NetDevice) (Borrowed DmaBuffer)] Unit))
(sig start-dma       (Fn [(Borrowed NetDevice) (Linear DmaBuffer)]   (Linear DmaTransfer)))
(sig wait-dma        (Fn [(Linear DmaTransfer)]                       (Linear DmaBuffer)))
(sig release-buffer  (Fn [(Linear DmaBuffer)]                         Unit))

;; ─── ドライバ本体 ─────────────────────────
(define (net-tx-packet (borrowed dev) (borrowed payload))
  (let* ((buf      (dma-alloc 1500))           ;; Linear, must be consumed
         ()        (configure-net &dev &buf)   ;; Borrowed: buf は consume されない
         (xfer     (start-dma &dev buf))       ;; ★ ここで buf を consume
         (used-buf (wait-dma xfer)))           ;; ★ xfer を consume、buf を再取得
    (release-buffer used-buf)))                ;; ★ used-buf を consume

;; ─── 違反例 (コンパイルエラー) ────────────
(define (broken-net-tx (borrowed dev))
  (let ((buf (dma-alloc 1500)))
    (start-dma &dev buf)))   ;; ★ Error: 'buf' must be consumed (リーク)
```

### A.2 サブシステムリージョン分離

```lisp
(define-region net-rgn :tier 2 :gc nil :max-size (* 16 1024 1024))
(define-region fs-rgn  :tier 2 :gc nil :max-size (* 64 1024 1024))
(define-region ui-rgn  :tier 4 :gc t   :max-size (* 32 1024 1024))

(define (web-server-loop)
  (with-region net-rgn
    (let loop ((conn (accept &+listen-socket+)))
      (let ((req  (alloc-in net-rgn (parse-http-request conn))))
        (handle-request req)
        (loop (accept &+listen-socket+))))))

(define (handle-request (borrowed req))
  ;; ファイル I/O は fs-rgn で行う
  (with-region fs-rgn
    (let ((path (transfer net-rgn fs-rgn (request-path req))))
      (fs/serve-file path))))
```

### A.3 unsafe を含む割込みハンドラ

```lisp
(sig +apic-eoi+ PhysAddr)
(define +apic-eoi+ (phys-addr 0xFEE000B0))

(sig apic-eoi (Fn [] Unit))
(define (apic-eoi)
  (unsafe (poke32 +apic-eoi+ 0)))

(sig handle-timer-irq (Fn [(Linear IrqContext)] Unit))
(define (handle-timer-irq ctx)
  (let ((tick (unsafe (rdtsc))))
    (scheduler-tick &ctx tick)
    (apic-eoi)
    (return-from-irq ctx)))      ;; ★ ctx を consume
```

---

## 13. 参考資料

### Spinor 内部資料

- `src/Spinor/Type.hs:18` — `data Linearity = Linear | Unrestricted`
- `src/Spinor/BorrowCheck.hs` — 既存の借用検査実装
- `src/Spinor/EscapeAnalysis.hs` — リージョン逃避解析
- `src/Spinor/Syntax.hs:153-155` — `EWithRegion` / `EAllocIn` AST
- `specs/exp_ownership_spec.md` — 線形型 / 所有権仕様 (Experimental)
- `specs/exp_regions_spec.md` — リージョン仕様 (Experimental)
- `manual/public/docs/vision/the-ultimate-dream.md` — Spinor OS ビジョン
- `manual/public/docs/vision/unikernel-architecture.md` — Approach 1 設計書
- `labo/research-ghc-rts-dependencies.md` — Approach 2 RTS 調査 (Issue #56)
- `labo/research-haskell-unikernel-history.md` — Approach 2 史的調査 (Issue #57)

### Mezzano (一次ソース)

- [GitHub: froggey/Mezzano](https://github.com/froggey/Mezzano) — 本体リポジトリ
- [Mezzano/system/gc.lisp](https://github.com/froggey/Mezzano/blob/master/system/gc.lisp) — GC 実装 (`+address-tag-*+` 定義含む)
- [Mezzano/tools/kboot/](https://github.com/froggey/Mezzano/tree/master/tools/kboot) — KBoot 統合
- [Mezzano/tools/kboot/kboot.cfg](https://github.com/froggey/Mezzano/blob/master/tools/kboot/kboot.cfg) — ブート設定例
- [MBuild](https://github.com/froggey/MBuild) — クロスビルドシステム

### Carp (一次ソース)

- [GitHub: carp-lang/Carp](https://github.com/carp-lang/Carp) — 本体
- [Carp/docs/Memory.md](https://github.com/carp-lang/Carp/blob/master/docs/Memory.md) — メモリ管理ドキュメント
- [Borrow Checking, The Carp Way](https://blog.veitheller.de/Borrow_Checking,_The_Carp_Way.html) — 設計解説ブログ

### Linear Types / Linear Lisp 学術文献

- Henry G. Baker, "Lively Linear Lisp — 'Look Ma, No Garbage!'" SIGPLAN Notices 27(8): 89-98 (1992) — [PDF (UT Austin)](https://www.cs.utexas.edu/~hunt/research/hash-cons/hash-cons-papers/BakerLinearLisp.pdf), [ACM DL](https://dl.acm.org/doi/10.1145/142137.142162)
- Philip Wadler, "Linear types can change the world!" (1990) — 線形論理を PL に持ち込む先駆け
- Bernardy et al., "Linear Haskell: practical linearity in a higher-order polymorphic language" POPL 2018 — [ACM](https://dl.acm.org/doi/10.1145/3158093), [arXiv](https://arxiv.org/pdf/1710.09756)
- [GHC User's Guide §6.4.22 Linear types](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/linear_types.html)
- Aaron Weiss et al., "Oxide: The Essence of Rust" (2019) — [arXiv](https://arxiv.org/pdf/1903.00982)
- [Substructural type systems (Wikipedia)](https://en.wikipedia.org/wiki/Substructural_type_system)

### Rust no_std / OS 開発

- [The Embedded Rust Book — `no_std`](https://docs.rust-embedded.org/book/intro/no-std.html)
- [Writing an OS in Rust (phil-opp)](https://os.phil-opp.com/) — 全章
- [phil-opp: Allocator Designs](https://os.phil-opp.com/allocator-designs/) — bump / linked-list / fixed-size block
- [phil-opp: Heap Allocation](https://os.phil-opp.com/heap-allocation/) — `GlobalAlloc` 統合
- [Rust RFC 2480 — liballoc](https://rust-lang.github.io/rfcs/2480-liballoc.html)
- [embedded-alloc](https://github.com/rust-embedded/embedded-alloc) — heap allocator for embedded
- [good_memory_allocator](https://docs.rs/good_memory_allocator) — dlmalloc 風 no_std アロケータ

### 関連 GC 文献

- Blackburn & McKinley, "Immix: A Mark-Region Garbage Collector with Space Efficiency, Fast Collection, and Mutator Performance" PLDI 2008 — [PDF](https://www.steveblackburn.org/pubs/papers/immix-pldi-2008.pdf)
- [Tagged pointer (Wikipedia)](https://en.wikipedia.org/wiki/Tagged_pointer)
- [Numbers and tagged pointers in early Lisp implementations (snellman.net)](https://www.snellman.net/blog/archive/2017-09-04-lisp-numbers/)

### 設計議論 / 思想的背景

- Vale 開発者ブログ, "What Vale Taught Me About Linear Types, Borrowing, and Memory Safety" — [verdagon.dev](https://verdagon.dev/blog/linear-types-borrowing)
- Borretti, "Linear Types and Exceptions" — [borretti.me](https://borretti.me/article/linear-types-exceptions)
- Borretti, "Type Systems for Memory Safety" — [borretti.me](https://borretti.me/article/type-systems-memory-safety)

### 関連 OS / ランタイム

- [KBoot bootloader](https://github.com/aejsmith/kboot)
- [Limine bootloader](https://github.com/limine-bootloader/limine)
- [seL4 (formally verified microkernel)](https://sel4.systems/) — 健全性証明の参考
- [Yalo: Lisp OS on bare metal x86-64](https://github.com/whily/yalo) — 別の Lisp OS 試行

[^mezzano-repo]: GitHub: froggey/Mezzano — https://github.com/froggey/Mezzano
[^mezzano-readme]: Mezzano README (2026-05-16 取得) — 言語比率 99.8% Common Lisp、SSA ベースの新コンパイラ、世代別 GC、KBoot ブート、x86-64 / AArch64 対応、SMP / virtio-net / FAT/EXT/3D サポート
[^mezzano-gc]: Mezzano `system/gc.lisp` — https://github.com/froggey/Mezzano/blob/master/system/gc.lisp
[^mezzano-gc-fetch]: 本調査で WebFetch により抽出された定数: `+address-tag-general+`, `+address-tag-cons+`, `+address-tag-pinned+`, `+address-tag-stack+`, `+address-old-generation+`, `+address-semispace+`, `*young-gen-newspace-bit*`, `*old-gen-newspace-bit*`
[^mezzano-kboot]: Mezzano `tools/kboot/` ディレクトリと `kboot.cfg` — KBoot 経由ブート、GRUB module `mezzano.mod` も提供
[^carp-repo]: GitHub: carp-lang/Carp — https://github.com/carp-lang/Carp ("A statically typed lisp, without a GC, for real-time applications.")
[^carp-memory]: Carp ドキュメント `docs/Memory.md` — https://github.com/carp-lang/Carp/blob/master/docs/Memory.md (本書に引用した借用 / lifetime / 線形値の規則の出典)
[^baker-paper]: Henry G. Baker, "Lively Linear Lisp — 'Look Ma, No Garbage!'" SIGPLAN Notices 27(8):89-98 (1992) — https://dl.acm.org/doi/10.1145/142137.142162 / PDF: https://www.cs.utexas.edu/~hunt/research/hash-cons/hash-cons-papers/BakerLinearLisp.pdf — 本調査では PDF バイナリの直接読込みに失敗したため検索結果のメタ情報経由で要点を確認
[^vale-linearity]: Vale ブログ, "What Vale Taught Me About Linear Types, Borrowing, and Memory Safety" — https://verdagon.dev/blog/linear-types-borrowing — 「memory safety doesn't come from borrow checking, not exactly. It comes from their types being affine」を含む議論
[^linear-haskell]: Bernardy, Boespflug, Newton, Peyton Jones, Spiwack, "Linear Haskell: practical linearity in a higher-order polymorphic language" POPL 2018 — https://dl.acm.org/doi/10.1145/3158093 / arXiv https://arxiv.org/pdf/1710.09756
[^rust-no-std]: The Embedded Rust Book — `no_std` https://docs.rust-embedded.org/book/intro/no-std.html
[^rust-globalalloc]: Rust RFC 2480 — liballoc / `#[global_allocator]` 安定化 (Rust 1.28) https://rust-lang.github.io/rfcs/2480-liballoc.html
[^phil-opp]: Philipp Oppermann, "Writing an OS in Rust" https://os.phil-opp.com/
[^phil-opp-allocators]: phil-opp, "Allocator Designs" https://os.phil-opp.com/allocator-designs/ — bump / linked-list / fixed-size block の比較
[^immix]: Blackburn & McKinley, "Immix: A Mark-Region Garbage Collector with Space Efficiency, Fast Collection, and Mutator Performance" PLDI 2008 — https://www.steveblackburn.org/pubs/papers/immix-pldi-2008.pdf
[^oxide-paper]: Aaron Weiss et al., "Oxide: The Essence of Rust" (2019) https://arxiv.org/pdf/1903.00982 — Rust の NLL 借用検査を形式化した論文
