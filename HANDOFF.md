# HANDOFF: Linear Spinor Phase R0 セッション引継ぎ

> **作成日:** 2026-05-17
> **引継元ブランチ:** `claude/session-a1-ai-native` (origin に push 済)
> **目的:** 別 PC または別セッションで Phase 3 Linear Spinor 作業を継続するための自己完結型引継書
> **関連 Issue:** [#62](https://github.com/yanqirenshi/Spinor/issues/62) (親 R&D) / [#65](https://github.com/yanqirenshi/Spinor/issues/65) (R0-1) / [#66](https://github.com/yanqirenshi/Spinor/issues/66) (R0-2)
> **関連ドキュメント:** [Approach 3 ホワイトペーパー](labo/a1_ai_native/research-spinor-os-approach3-rd.md)

## 1. 現在の状態

### 1.1 ブランチ・コミット

```
branch: claude/session-a1-ai-native (origin から fetch 可能)
HEAD:   ba62f48 feat(linear-spinor): rewrite Algorithm W to track resource consumption (Issue #66)
prev:   3e25a77 feat(linear-spinor): introduce Mult (multiplicity) and ownership AST nodes (Issue #65)
base:   origin/master の最新を取り込み済み (60a471c Merge pull request #64)
```

### 1.2 検証状態

| 検証項目 | 結果 |
|---|---|
| `cabal build` | ✅ PASS (全 25 モジュール + executable リンク成功) |
| `cabal test` | ✅ PASS (254 examples, 0 failures) |
| 線形変数 Move エラー検出 | ✅ smoke test 確認 |
| 線形変数 Resource Leak 検出 | ✅ smoke test 確認 |
| `if` 分岐不整合検出 | ✅ smoke test 確認 |

## 2. 完了した作業

### 2.1 Issue #65 [Phase R0-1] 多重度と所有権 AST の追加 (commit `3e25a77`)

**`Spinor.Syntax`:**
- `data Mult = One | Many | Borrow deriving (Show, Eq, Ord)` 追加
- `Expr` に 4 新コンストラクタ: `EBorrow / EDeref / EUnsafe / EMove SourceSpan Expr`
- `exprSpan` を新コンストラクタ対応に拡張
- `Mult` を export リストに追加

**`Spinor.Type`:**
- `TArrMult Mult Type Type` 追加 (Linear Haskell 流の多重度付き矢印)
- `showMult :: Mult -> Text` ヘルパー追加
- `showType` で `(Int -%One> Bool)` 形式表示に対応
- 既存 `TArr` / `TLinear Linearity Type` は legacy として残置

**`Spinor.Eval`:**
- 4 新コンストラクタを **素通し評価** (`eval (EBorrow _ e) = eval e` 等)
- `exprToVal` に変換ケース追加

**`Spinor.Infer`:**
- `instance Types Type` の `apply`/`ftv` に `TArrMult` ケース追加
- `unify` に `TArrMult` 同士のケース追加 (多重度一致時のみ unify)
- 4 新コンストラクタを TODO スタブ (素通し + コメント)
- `inferQuote` も対応

### 2.2 Issue #66 [Phase R0-2] 型推論器のリソース消費トラッキング化 (commit `ba62f48`)

**シグネチャ変更:**
```haskell
-- 旧
infer :: TypeEnv -> Expr -> Infer (Subst, Type)
-- 新
infer :: TypeEnv -> Expr -> Infer (Subst, Type, TypeEnv)
                                            ^^^^^^^^ 残存環境
```

**Infer モナド拡張:**
```haskell
data InferState = InferState
  { isCounter :: Int        -- フレッシュ型変数カウンタ (既存)
  , isMoved   :: Set Text   -- 消費済み線形変数名 ★ 新規
  }
```

**設計判断:**
- 線形変数判定 (簡易ヒューリスティック): `Scheme [] (TLinear Linear _)` or `Scheme [] (TArrMult One _ _)`
  - 多相な線形関数は非線形扱い (twister 互換性のため、Issue 仕様で明示的に許容)
- `ESym` ルックアップ時に線形なら `markMoved x` + env から削除
- ルックアップ失敗時 `isMovedName x` で「未定義」と「use-after-move」を区別
- `let` 終端で残存線形変数があれば **Resource Leak エラー**
- `if` で then/else の MovedSet 差分があれば **分岐不整合エラー**
- `if`/`match` のマージ: env は intersection、movedSet は union
- `fn` の本体推論時に MovedSet を一時退避→復元 (capture が呼出側に漏れない)

**呼び出し元更新:**
- 同モジュール内: `inferDefine`, `inferTopDefine`, `inferApp`, `inferBranch`, パターン処理を新 3 タプル対応
- 外部: `MCP.hs` の 1 箇所を `Right (_, typ, _envAfter) ->` パターンに更新
- テスト: `inferTop` 経由のため変更不要

### 2.3 ドキュメント整備 (前半セッションで完了済、既に master にマージ)

- `labo/a1_ai_native/research-spinor-os-approach3-rd.md` — Approach 3 R&D ホワイトペーパー (#62 のホワイトペーパー成果物。PR #63 + #64 でマージ済)

## 3. 別 PC でのセットアップ手順

```bash
# 1. リポジトリ取得 (まだ無ければ)
git clone git@github.com:yanqirenshi/Spinor.git
cd Spinor

# 2. セッションブランチを取得して切替
git fetch origin
git checkout claude/session-a1-ai-native

# 3. ビルド検証 (環境前提: GHC 9.6+, cabal-install 3.0+)
cabal build       # 全モジュール + executable ビルド
cabal test        # 254 examples PASS が期待値
```

### 3.1 環境前提

- GHC 9.6+ (`ghcup install ghc 9.6.7` を推奨)
- cabal-install 3.0+
- Windows (UCRT64/MSYS2) or Linux/macOS

### 3.2 動作確認用 smoke test

`cabal repl` で `Spinor.Infer` を読み込んで:

```haskell
-- 線形変数を 1 回使用 → 成功
runInfer $ infer (Map.insert "x" (Scheme [] (TLinear Linear TInt)) Map.empty)
                 (ESym dummySpan "x")

-- 同変数を 2 回使用 → use-after-move エラー
runInfer $ infer envWithLinearX
                 (EList dummySpan [ESym dummySpan "cons", ESym dummySpan "x", 
                                    EList dummySpan [ESym dummySpan "cons", 
                                                      ESym dummySpan "x", 
                                                      EList dummySpan []]])
```

## 4. ワークフロー (現セッションのルール)

- ✅ **このブランチで継続作業** — `claude/session-a1-ai-native` でのみコミット
- ❌ **PR 作成は不要** — push のみ
- ❌ **master 直接コミットは禁止**
- ✅ **テスト破壊許容** — twister/ や E2E 互換は無視可、ただし `cabal test` は形として通る状態を維持

## 5. 未着手の TODO (次の作業候補)

### 5.1 短期 (R0-3 候補)

1. **`EBorrow / EDeref / EUnsafe / EMove` の本格意味論実装**
   - 現状: 素通しスタブ + TODO コメント
   - 目標: 借用は元の所有権を保持、ムーブは consume、unsafe は型レベル隔離マーカ
2. **構文パーサの拡張**
   - 現状: `&expr / *expr / @expr / (unsafe ...)` を生成する経路がない
   - 目標: `Syntax.hs` の `parseExpr` に `pBorrow / pDeref / pMove / pUnsafe` を追加
3. **`Scheme` への多重度メタ追加**
   - 現状: 多相な線形関数は非線形扱い (簡易ヒューリスティック)
   - 目標: `Scheme [Mult] [Text] Type` で多重度を量子化対象に

### 5.2 中期 (R1 候補)

4. **`BorrowCheck.hs` の新 4 コンストラクタ対応** (`-Wincomplete-patterns` 警告解消)
5. **`Server.hs` の `exprToText` / `exprToLispText`** REPL 表示対応
6. **`Expander.hs` のマクロ展開時の素通し処理**
7. **legacy 廃止** — `TArr` / `TLinear Linearity Type` を `TArrMult` に完全移行

### 5.3 長期 (R2 候補)

8. **Tier 別アロケータ実装** (ホワイトペーパー §5.1 の 4 Tier モデル)
9. **`(unsafe ...)` 内でのみ使える `peek/poke` プリミティブ**
10. **セルフホストへの第一歩**: パーサを Linear Spinor で書き直し試行

## 6. 関連リソース

### 6.1 内部ドキュメント

- [`labo/a1_ai_native/research-spinor-os-approach3-rd.md`](labo/a1_ai_native/research-spinor-os-approach3-rd.md) — Approach 3 ホワイトペーパー (4 Tier モデル、線形性階層、unsafe 設計の根拠)
- [`labo/research-ghc-rts-dependencies.md`](labo/research-ghc-rts-dependencies.md) — Approach 2 RTS 調査 (#56)
- [`labo/research-haskell-unikernel-history.md`](labo/research-haskell-unikernel-history.md) — Approach 2 史的調査 (#57)
- [`CLAUDE.md`](CLAUDE.md) — プロジェクト全体のコンテキスト・ワークフロー

### 6.2 GitHub

- [Spinor Issues #62](https://github.com/yanqirenshi/Spinor/issues/62) — R&D 親 Issue (Approach 3)
- [Spinor Issues #65](https://github.com/yanqirenshi/Spinor/issues/65) — Phase R0-1 (✅ 実装済)
- [Spinor Issues #66](https://github.com/yanqirenshi/Spinor/issues/66) — Phase R0-2 (✅ 実装済)
- [Project 46: Lispマシンの創生](https://github.com/users/yanqirenshi/projects/46) — 究極目標プロジェクトボード
- [Project 48](https://github.com/users/yanqirenshi/projects/48) — セッション引継ぎ管理用

## 7. 引継プロンプト (新 PC の Claude Code セッションへ貼り付け用)

新 PC で Claude Code を起動したら、以下をそのまま貼り付けると本セッション同等の文脈で再開できます:

```
別 PC から移動してきました。Spinor プロジェクトの Phase 3 Linear Spinor
リファクタリングを継続したい。HANDOFF.md (リポジトリ root) を参照して
状態を把握し、ワークフロー (claude/session-a1-ai-native ブランチで継続、
PR 不要) を踏襲してください。

直近の到達点:
- Issue #65 (Mult/AST 拡張) と #66 (Infer.hs リソース追跡化) 実装完了
- cabal build / cabal test (254/254) PASS
- ブランチ: claude/session-a1-ai-native (origin に push 済)

次の指示を待ちます。
```

---

**注:** 本ドキュメントは本セッション (2026-05-17, 旧 PC) で作成。本セッション固有の
ローカル状態 (`~/.claude/projects/.../memory/`、`.claude/settings.local.json` の
WebFetch 許可ドメイン等) は新 PC に転送されません。
