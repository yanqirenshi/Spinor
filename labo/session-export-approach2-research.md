# Session Export — Approach 2 調査セッション記録

> エクスポート日: 2026-06-14
> 注: Claude Code ネイティブの `/export` は対話型ビルトインのため、本ファイルは
> その代替として会話を整理して書き起こした「セッション記録」です(全 raw ログではなく要約形式)。
> 引き継ぎの要点は [../HANDOFF.md](../HANDOFF.md) を参照。

---

## セッションの目的

[Issue #45](https://github.com/yanqirenshi/Spinor/issues/45)(The Ultimate Dream / Approach 2: Spinor 本体 = Haskell ランタイムのベアメタル動作)に関連する調査を、Engineer (Claude Code) として実施する。Issue #45 は説明用の親 Issue で、実作業は別途立てた子 Issue で進める方針。

## 経緯(時系列)

### 1. 状況把握
- Issue #45 を確認し、子 Issue が [#56](https://github.com/yanqirenshi/Spinor/issues/56)(GHC RTS の POSIX 依存性)と [#57](https://github.com/yanqirenshi/Spinor/issues/57)(HaLVM 等の先行事例分析)であることを特定。
- 既存資産を確認: `os/Kraftfile`, `os/Makefile.uk`, `build-os.sh`, `runtime/spinor.{c,h}`, `manual/public/docs/vision/{unikernel-architecture,the-ultimate-dream}.md`。Phase 1 PoC (AOT C → Unikraft) は開通済みで、今回は Phase 3 (GHC ランタイム本体の Unikernel 化) の事前調査という位置づけと理解。

### 2. チーム編成と並列調査
ユーザーの依頼で #56 / #57 対応チームを編成。Lead Engineer (自分) + Researcher 2 名の構成で、`general-purpose` エージェントを 2 体並列起動:
- **Researcher A (#56):** GHC 9.6+ RTS の POSIX 依存 4 領域(メモリ / スレッド / タイマー・シグナル / I/O イベント)を一次ソース付きで洗い出し、Unikraft 対応マトリクスを作成。
- **Researcher B (#57):** HaLVM / House の歴史と停滞要因、現代の代替(`ghc -fllvm` / GHC Wasm backend / GHC JS backend / Asterius / MirageOS)、Spinor への適用と持続可能な保守戦略を調査。

PR 方針: Issue ごとに 2 PR に分割(ユーザー選択)。スコープは現状どおり。

### 3. レポート化と PR
- [labo/research-ghc-rts-dependencies.md](research-ghc-rts-dependencies.md) を作成 → PR [#59](https://github.com/yanqirenshi/Spinor/pull/59) → マージ / #56 クローズ。
- [labo/research-haskell-unikernel-history.md](research-haskell-unikernel-history.md) を作成 → PR [#60](https://github.com/yanqirenshi/Spinor/pull/60) → マージ / #57 クローズ。
- 当初は `tasks/` に配置したが、ユーザー指示で `labo/` ディレクトリを新設し `git mv` で移動 → PR [#61](https://github.com/yanqirenshi/Spinor/pull/61) → マージ。

### 4. Q&A と方針確認
- 「HaLVM 型の GHC fork とは何か」を解説(GHC ソース本体を別 repo に fork し、Xen/ベアメタル向け RTS パッチを永続メンテするモデル。HaLVM/GHCJS/Asterius が示すとおり、mainline に統合されない fork は構造的に死ぬ)。
- ユーザーより「**Approach 3 は他アプローチと切り離して R&D 的に進めたい**」との方針表明(→ 後に PR #64 で `labo/a1_ai_native/` に分離されたことを確認)。
- ユーザーがセッション内容(Approach 2 = GHC Wasm × Unikraft/WAMR で GHC 無改造のまま Spinor OS を生成する本命プラン)の理解確認。概ね正確と回答しつつ、確度の補正 3 点(tech preview / WASI preview1 の network・thread 制約 / 起動・サイズ数値は推定)を補足。

### 5. 引き継ぎ
- ローカル master を `origin/master` に fast-forward 更新。
- 別 PC への移行依頼を受け、(1) 作業は GitHub 上にあり clone で復元可、(2) 会話/メモリはマシンローカルで公式同期なし、を確認。
- 本セッション記録 + [HANDOFF.md](../HANDOFF.md) を作成し、引き継ぎ Issue を Project 46 に起票。

## 主要な技術的結論

詳細は [research-haskell-unikernel-history.md](research-haskell-unikernel-history.md) §5 / [research-ghc-rts-dependencies.md](research-ghc-rts-dependencies.md) §6 を参照。

1. **GHC RTS の Unikernel 化は技術的に GO 可能**(✅16 / ⚠️7 / ❌1)。9.6+ で `setitimer` が廃止され pthread+ppoll 方式になったこと、I/O が epoll/eventfd で Unikraft posix-poll/posix-eventfd にそのまま乗ることが追い風。ボトルネックはメモリ層(`lib/ukvmem` の `mprotect` / 1MB アライン / `madvise` 挙動)。
2. **本命ルート = GHC wasm32-wasi backend + Unikraft/WAMR**(戦略 B)。GHC mainline 機能なので fork 不要。
3. **HaLVM 型 fork(戦略 A)は明示却下。**
4. **前提条件:** Windows ホストでは wasm backend 非対応 → Linux/WSL2 CI への移行が必須。

## 次アクション候補

[HANDOFF.md §5](../HANDOFF.md) 参照(Linux CI 整備 / Phase 3a・3b PoC / SIGSEGV 調査 / self-hosting 評価)。
