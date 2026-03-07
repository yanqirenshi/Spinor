# LLVM Backend Investigation Report (Spike)

**Date:** 2026-03-07
**Branch:** `spike/llvm-hs-investigation`
**Issue:** [#9 - LLVM バックエンドの調査と JIT 実行プロトタイプの作成](https://github.com/yanqirenshi/Spinor/issues/9)

## Executive Summary

Windows (GHC 9.6.7) 環境での `llvm-hs` 導入を試みましたが、**依存関係の互換性問題**と **LLVM ネイティブライブラリの不在**により、ビルドに失敗しました。

## 環境情報

| 項目 | バージョン |
|------|------------|
| OS | Windows 10 (26200.7922) |
| GHC | 9.6.7 |
| Cabal | 3.14.2.0 |
| LLVM | **未インストール** |

## 発生した問題

### 1. 依存関係の競合 (Critical)

```
Error: [Cabal-7107]
Could not resolve dependencies:
[__2] rejecting: transformers-0.6.1.0/installed-0.6.1.0
      (conflict: llvm-hs-pure => transformers>=0.3 && <0.6)
```

**原因:**
- `llvm-hs-pure` 9.0.0 (2019年リリース) は `transformers < 0.6` を要求
- GHC 9.6.7 には `transformers-0.6.1.0` がインストール済み
- 依存関係グラフが解決不可能

### 2. LLVM ネイティブライブラリの不在

```bash
$ llvm-config --version
llvm-config: command not found
```

Windows 環境に LLVM がインストールされていないため、仮に Haskell パッケージの依存関係が解決できても、FFI バインディングのリンクで失敗する可能性が高い。

### 3. llvm-hs プロジェクトのメンテナンス状況

- **最終リリース:** 9.0.1 (2019年9月)
- **対応 GHC:** 8.0.2 ~ 8.6.5 (GHC 9.x 非対応)
- **対応 LLVM:** LLVM 9
- **OrcJIT:** LLVM 10 で削除された旧 API を使用
- **開発状況:** llvm-12 ブランチが存在するが Hackage 未公開

## 代替案の調査

### 推奨: llvm-tf + llvm-ffi

| パッケージ | 説明 | LLVM 対応 |
|-----------|------|----------|
| `llvm-ffi` | FFI バインディング | LLVM 13-21 |
| `llvm-tf` | 高レベル API (型族使用) | llvm-ffi に依存 |
| `llvm-extra` | 追加ユーティリティ | 同上 |

**利点:**
- アクティブにメンテナンスされている
- 最新の LLVM バージョンに対応
- GHC 9.x と互換性がある可能性が高い

**課題:**
- Windows での LLVM インストールが依然として必要
- API が llvm-hs とは異なる

### その他の選択肢

1. **GHC 内蔵の LLVM バックエンド (`-fllvm`)**
   - GHC 自体が LLVM バックエンドを持つ
   - Spinor 評価器からの直接利用は困難

2. **C コード生成の継続 (現行方式)**
   - `Spinor.Compiler.Codegen` で既に C99 生成を実装済み
   - GCC/Clang でコンパイル可能
   - 現時点では最も現実的

3. **Cranelift (Rust エコシステム)**
   - より軽量な JIT コンパイラ
   - Haskell バインディングは不在

## 作成したファイル

### src/Spinor/Compiler/LLVM.hs (プロトタイプ)

```haskell
-- | LLVM Backend for Spinor
--
-- 整数リテラル、+、* のみをサポートする最小限の実装
module Spinor.Compiler.LLVM
  ( codegen   -- Expr -> AST.Module
  , runJIT    -- AST.Module -> IO (Either Text Integer)
  ) where
```

**注意:** このファイルは llvm-hs がビルドできないため、コンパイル不可能です。

### spinor.cabal への変更

```diff
+ Spinor.Compiler.LLVM
  ...
+ llvm-hs-pure >= 9.0 && < 10,
+ llvm-hs      >= 9.0 && < 10
```

## 今後の課題 (Future Considerations)

### JIT 実現に向けて

1. **Linux/macOS 環境での再検証**
   - LLVM のインストールが容易
   - llvm-tf/llvm-ffi のビルドを試す

2. **関数定義のサポート**
   - Spinor の `fn` を LLVM 関数にマッピング
   - クロージャの環境をどう渡すか (スタック or ヒープ)

3. **GC (ガベージコレクション) の統合**
   - LLVM の GC サポート (Shadow Stack, Statepoint) を調査
   - または外部 GC (Boehm GC) との統合

4. **既存 C バックエンドとの関係**
   - AOT コンパイル: C バックエンド (安定性重視)
   - JIT 実行: LLVM バックエンド (対話性重視)
   - 両者の共存が理想

### Windows 固有の対策

```bash
# MSYS2 で LLVM をインストール
pacman -S mingw-w64-x86_64-llvm mingw-w64-x86_64-clang

# 環境変数の設定
export LLVM_CONFIG=/mingw64/bin/llvm-config
```

ただし、MSYS2 の LLVM と GHC (native Windows) の ABI 互換性は未検証。

## 結論

| 項目 | 状態 |
|------|------|
| llvm-hs の導入 | **失敗** (GHC 9.x 非対応) |
| JIT プロトタイプ | **未完成** |
| 難易度の評価 | **高** (Windows では特に) |
| 推奨次ステップ | Linux 環境で llvm-tf を試す |

**所見:** Windows 環境での Haskell + LLVM 統合は非常に困難です。C バックエンドによる AOT コンパイルが現実的なアプローチであり、JIT は Linux/macOS 環境での実験を推奨します。
