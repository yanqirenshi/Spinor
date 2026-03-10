# WSL2 引継情報: LLVM Backend Investigation

**作成日:** 2026-03-07
**ブランチ:** `spike/llvm-hs-investigation`
**Issue:** https://github.com/yanqirenshi/Spinor/issues/9

---

## 1. 現在の状況

### 完了した作業

| 作業 | 状態 | 備考 |
|------|------|------|
| ブランチ作成 | ✅ | `spike/llvm-hs-investigation` |
| llvm-hs 導入試行 | ✅ | Windows で失敗 |
| 代替ライブラリ調査 | ✅ | llvm-tf/llvm-ffi を推奨 |
| 調査レポート作成 | ✅ | `docs/spike-llvm-investigation.md` |
| Issue コメント投稿 | ✅ | 調査サマリを記録済み |

### 未完了の作業

| 作業 | 状態 | 備考 |
|------|------|------|
| LLVM インストール | ❌ | WSL2 で実施 |
| llvm-tf/llvm-ffi ビルド | ❌ | WSL2 で試行 |
| JIT プロトタイプ動作確認 | ❌ | `(+ 10 (* 2 5))` → `20` |
| CLI 統合 (`spinor jit`) | ❌ | `app/Main.hs` 修正 |

---

## 2. Windows で発生した問題

### 依存関係の競合

```
llvm-hs-pure 9.0.0 requires transformers < 0.6
GHC 9.6.7 has transformers-0.6.1.0 installed
→ 依存関係解決不可能
```

### llvm-hs の GHC 互換性

- llvm-hs 9.0.1 は GHC 8.0.2 ~ 8.6.5 のみ対応
- GHC 9.x では使用不可
- GitHub に llvm-12 ブランチがあるが Hackage 未公開

---

## 3. WSL2 での作業手順

### Step 1: LLVM のインストール

```bash
# Ubuntu/Debian
sudo apt update
sudo apt install llvm-15 llvm-15-dev clang-15

# バージョン確認
llvm-config-15 --version

# シンボリックリンク作成 (必要に応じて)
sudo ln -s /usr/bin/llvm-config-15 /usr/bin/llvm-config
```

### Step 2: リポジトリのクローン/同期

```bash
# Windows のリポジトリを WSL2 にクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor
git checkout spike/llvm-hs-investigation
git pull origin spike/llvm-hs-investigation
```

### Step 3: cabal ファイルの修正

現在の `spinor.cabal` には `llvm-hs` が設定されていますが、**`llvm-tf` + `llvm-ffi` に変更**することを推奨します。

```diff
- llvm-hs-pure >= 9.0 && < 10,
- llvm-hs      >= 9.0 && < 10
+ llvm-ffi >= 15.0 && < 16,
+ llvm-tf  >= 15.0 && < 16
```

**注意:** バージョンは LLVM のメジャーバージョンと一致させる必要があります。

### Step 4: LLVM.hs の書き換え

`src/Spinor/Compiler/LLVM.hs` は llvm-hs 用に書かれているため、llvm-tf 用に書き換えが必要です。

**llvm-tf の基本的な使い方:**

```haskell
import LLVM.Core
import LLVM.ExecutionEngine

-- IR 生成
codegen :: Expr -> IO Module
codegen expr = do
  m <- newModule
  -- ... IR 構築 ...
  return m

-- JIT 実行
runJIT :: Module -> IO Integer
runJIT m = do
  ee <- createExecutionEngine m
  fn <- getFunction ee "main"
  result <- run fn []
  return result
```

### Step 5: ビルドと検証

```bash
cabal update
cabal build

# テスト実行
echo "(+ 10 (* 2 5))" > test-llvm.spin
cabal run spinor -- jit test-llvm.spin
# 期待出力: 20
```

---

## 4. 作成済みファイル

### src/Spinor/Compiler/LLVM.hs

```haskell
-- 現在は llvm-hs 用のコード (コンパイル不可)
-- WSL2 で llvm-tf 用に書き換えが必要

module Spinor.Compiler.LLVM
  ( codegen   -- Expr -> AST.Module
  , runJIT    -- AST.Module -> IO (Either Text Integer)
  ) where

-- 対応する式:
-- - EInt: 整数リテラル → i64
-- - (+ e1 e2): 加算
-- - (* e1 e2): 乗算
```

### docs/spike-llvm-investigation.md

詳細な調査レポート (Windows での調査結果)

---

## 5. CLI 統合の設計

`app/Main.hs` に以下のコマンドを追加:

```haskell
-- spinor jit <file>
case args of
  ["jit", file] -> do
    src <- T.readFile file
    case parseExprs file src of
      Left err -> putStrLn $ "Parse error: " ++ show err
      Right exprs -> do
        result <- runJIT (codegen (last exprs))
        case result of
          Left err -> putStrLn $ "JIT error: " ++ T.unpack err
          Right n  -> print n
```

---

## 6. 参考リンク

### ライブラリ

- [llvm-tf (Hackage)](https://hackage.haskell.org/package/llvm-tf)
- [llvm-ffi (Hackage)](https://hackage.haskell.org/package/llvm-ffi)
- [llvm-hs (GitHub)](https://github.com/llvm-hs/llvm-hs) - 参考のみ

### ドキュメント

- [LLVM Language Reference](https://llvm.org/docs/LangRef.html)
- [LLVM JIT Tutorial](https://llvm.org/docs/tutorial/BuildingAJIT1.html)

### Issue

- https://github.com/yanqirenshi/Spinor/issues/9

---

## 7. 期待される成果物

1. **動作する JIT プロトタイプ**
   - `(+ 10 (* 2 5))` → `20`
   - 整数演算 (+, *, -) のサポート

2. **CLI コマンド**
   - `spinor jit <file>` で JIT 実行

3. **Issue への完了報告**
   - 実行結果とコード抜粋
   - 今後の課題 (関数定義、GC など)

---

## 8. 注意事項

- **GHC バージョン:** WSL2 でも GHC 9.x を使用する場合、llvm-hs は使用不可
- **LLVM バージョン:** llvm-tf/llvm-ffi のバージョンと LLVM のバージョンを一致させる
- **FFI リンク:** `llvm-config --libs` の出力を確認し、必要なライブラリがリンクされていることを確認

---

**Good luck on WSL2!** 🐧
