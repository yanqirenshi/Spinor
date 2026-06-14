# HANDOFF — Approach 2 調査セッション引き継ぎ

> 作成日: 2026-06-14 / 引き継ぎ元セッション: Claude Code (worktree `angry-ishizaka-aea5c2`)
> 対象: [The Ultimate Dream / Approach 2](https://github.com/yanqirenshi/Spinor/issues/45) — Spinor 本体 (Haskell ランタイム) のベアメタル動作
> セッション記録(export 相当): [labo/session-export-approach2-research.md](labo/session-export-approach2-research.md)

別 PC でこのテーマの続きに入るための引き継ぎメモ。**実作業はすべて `master` にマージ済みなので、別 PC では `git clone` だけで最新状態が手に入る。**

---

## 1. このセッションでやったこと

[Issue #45](https://github.com/yanqirenshi/Spinor/issues/45) (Approach 2 の親) 配下の調査タスク 2 件を、Researcher 2 名のチーム編成で並列実施し、レポート化 → PR → マージまで完了した。

| Issue | 内容 | PR | 状態 |
|---|---|---|---|
| [#56](https://github.com/yanqirenshi/Spinor/issues/56) | GHC RTS の POSIX 依存性の洗い出し | [#59](https://github.com/yanqirenshi/Spinor/pull/59) | ✅ Merged / Closed |
| [#57](https://github.com/yanqirenshi/Spinor/issues/57) | 先行事例 (HaLVM 等) の分析と現代 GHC の実現可能性 | [#60](https://github.com/yanqirenshi/Spinor/pull/60) | ✅ Merged / Closed |
| (整理) | 2 レポートを `tasks/` → `labo/` へ移動 | [#61](https://github.com/yanqirenshi/Spinor/pull/61) | ✅ Merged |

成果物 (現在 `master` 上):
- [labo/research-ghc-rts-dependencies.md](labo/research-ghc-rts-dependencies.md)
- [labo/research-haskell-unikernel-history.md](labo/research-haskell-unikernel-history.md)

## 2. 現在のリポジトリ状態 (2026-06-14 時点)

- `master` HEAD: `60a471c` (このセッション後に別作業も入っている)
- 関連して既にマージ済みの隣接作業:
  - **Approach 3 は `labo/a1_ai_native/` に分離済み** ([#64](https://github.com/yanqirenshi/Spinor/pull/64), Issue #62 "Linear Spinor" R&D ホワイトペーパー) — 「Approach 3 は切り離して R&D 的に進める」というユーザー方針が反映済み。
  - **Spinor OS 自動ビルド CI** ([#55](https://github.com/yanqirenshi/Spinor/pull/55), Issue #54) — kraftkit ベースのパイプライン。
- 既存の Phase 1 PoC 資産: `os/Kraftfile`, `os/Makefile.uk`, `build-os.sh`, `runtime/spinor.{c,h}`, `manual/public/docs/vision/unikernel-architecture.md`

## 3. Approach 2 の結論 (調査の核心)

**「GHC 本体を一切 fork/改造せず、低コストで Spinor OS を実現できる本命ルート = GHC Wasm backend × Unikraft/WAMR」**。

推奨ロードマップ (research-haskell-unikernel-history.md §5):

| 戦略 | 概要 | 評価 |
|---|---|---|
| **C** | AOT C → Unikraft (現行 Phase 1 の延長) | 継続(低リスク) |
| **E** | 素の GHC バイナリを Unikraft musl 上で動かす 1〜2 週 PoC | feasibility 判定 |
| **B** | **GHC wasm32-wasi backend + Unikraft/WAMR** | **本命** |
| **D** | Spinor self-hosting (GHC 依存を縮退) | 長期 |
| ~~A~~ | ~~HaLVM 型の GHC fork 再現~~ | **明示却下** |

却下理由: HaLVM / GHCJS / Asterius はいずれも「単一機関が GHC を out-of-tree fork し続ける」構造的不可能性で停止 (HaLVM は GHC 8.0.2 / 2020 archive)。mainline に乗る wasm32-wasi なら fork 不要。

## 4. 計画前に握っておくべき確度の補正

レポートの結論は「検証すべき有力仮説」であり、動く実装が確認できたわけではない。以下の留保あり:

1. **GHC Wasm backend は今も "tech preview"。** WASI preview1 が抽象化するのは file/clock/random 等の基本のみで、**ネットワークとスレッドは wasi-preview2 (Component Model) 待ち** → REPL の TCP 接続・マルチスレッド評価は現時点で素直に乗らない可能性。
2. **「起動数ミリ秒・イメージ数MB」は隣接事例 (Phase 1 C 経路 / wazero デモ ~20ms) からの推定値**で、GHC-Wasm REPL の実測ではない。RTS/GC を含む分イメージは膨らむ前提。
3. **GHC Wasm backend は Windows ホスト (UCRT64/MSYS2) を公式サポートしない** (Linux/macOS のみ) → **どのルートを取っても Linux または WSL2 ベースの CI が前提条件**。

## 5. 次アクション候補 (Issue 起票候補)

PM (User & Gemini) 判断待ち。両レポートの「次のステップ」から:

- [ ] **Linux/WSL2 CI 環境の整備** (GitHub Actions Linux runner + `wasm32-wasi-ghc` のキャッシュ) — すべての路線の前提
- [ ] **Phase 3a PoC: 素の GHC バイナリ on Unikraft** (戦略 E、1〜2 週 spike)
- [ ] **Phase 3b PoC: Hello World on wasm32-wasi + WAMR/Unikraft** (戦略 B 本命の最小検証)
- [ ] GHC RTS の `SIGSEGV` / `sigaltstack` 利用有無の網羅調査 (#56 の未解決 TODO)
- [ ] Spinor self-hosting 進捗の定量評価 (戦略 D の前提)

## 6. 別 PC でセッションを再開する手順

Claude Code の会話セッション/メモリは**マシンローカルで、公式のマシン間同期は存在しない**(確認済み)。

1. **作業の移行 (必須・これだけで十分):**
   ```powershell
   git clone git@github.com:yanqirenshi/Spinor.git <任意のパス>
   cd <任意のパス>
   ```
   → `labo/` 配下に本調査レポート、`HANDOFF.md` もこれで入る。worktree の再作成は不要。
2. **会話の続きが要る場合:** 旧 PC で `/export` を実行してテキストを持ち出し、新 PC の新規 `claude` セッション冒頭に貼って参照させる。または本 `HANDOFF.md` を読ませる(推奨・軽量)。
3. **やってはいけない:** `.claude/settings.local.json` のコピー(マシン固有の権限 allowlist)。共有設定は `.claude/settings.json` 側で clone により同期される。
4. **メモリ:** `~/.claude/projects/<...>/memory/` はマシンローカルで自動同期されない。本セッションでは永続メモリ保存は行っていないのでコピー対象は無し。

## 7. ローカルクリーンアップ (任意)

このセッションで作ったマージ済みブランチは破棄可:
- ローカル: `claude/issue-56-ghc-rts-deps`, `claude/issue-57-haskell-unikernel-history`, `claude/move-research-to-labo`
- リモート: 同名 (PR マージ時に GitHub 側で削除済みの可能性あり)

---

*この HANDOFF はセッション引き継ぎ用。恒久的な設計書は `specs/`、調査ログは `labo/` を参照。*
