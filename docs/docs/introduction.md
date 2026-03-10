# Introduction

## Spinor とは

Spinor は Haskell で実装された静的型付け Lisp 処理系です。

**"Lisp の構文を持ち、Haskell の意味論を持つ言語"** をコンセプトに設計されています。

### 特徴

- **Hindley-Milner 型推論**: 明示的な型注釈なしで、コンパイル時に型安全性を保証
- **正格評価 (Call-by-value)**: 予測可能な評価順序
- **代数的データ型 (ADT)**: `data` による型定義と `match` によるパターンマッチ
- **マクロシステム**: Lisp の伝統的なマクロによるメタプログラミング
- **モジュールシステム**: `module` と `import` による名前空間管理

## インストール

### 必要な環境

- GHC 9.6 以上
- Cabal 3.0 以上

### ソースからのビルド

```bash
# リポジトリのクローン
git clone https://github.com/yanqirenshi/Spinor.git
cd Spinor

# ビルド
cabal build

# REPL の起動
cabal run spinor
```

> **Note:** Spinor は OpenBLAS, OpenCL, GLFW などの C ライブラリに依存しています。
> 詳細な環境構築手順については [Build Guide](build) を参照してください。

## クイックスタート

### REPL の起動

```bash
$ cabal run spinor
Spinor REPL (step16)
Loading Twister environment...
Twister loaded.
spinor> (+ 1 2)
:: Int
3
```

### スクリプトの実行

```bash
$ cabal run spinor -- hello.spin
```

### ネイティブバイナリへのコンパイル

```bash
# C 言語へのトランスパイル + GCC でのコンパイル
$ cabal run spinor -- build main.spin
```

### 利用可能なコマンド

| コマンド | 説明 |
|:---------|:-----|
| `spinor` | REPL を起動 |
| `spinor <file>` | スクリプトを実行 |
| `spinor build <file>` | ネイティブバイナリにコンパイル |
| `spinor compile <file>` | C ソースコードに変換 |
| `spinor server` | Swank サーバーを起動 (SLY/SLIME 連携) |
| `spinor lsp` | LSP サーバーを起動 |
| `spinor docgen` | リファレンスドキュメントを生成 |

## 開発環境

### Emacs + SLY

Spinor は Swank プロトコルに対応しており、Emacs の SLY から接続できます。

```bash
# Swank サーバーの起動 (デフォルト: ポート 4005)
$ cabal run spinor -- server
```

Emacs 側:
```elisp
M-x sly-connect RET localhost RET 4005
```

### LSP (Language Server Protocol)

VS Code など LSP 対応エディタでの利用:

```bash
$ cabal run spinor -- lsp
```

詳細なセットアップ手順は各エディタのドキュメントを参照してください。

## 次のステップ

- [Syntax](syntax/atoms) - 言語の構文を学ぶ
- [Build Guide](build) - 環境構築の詳細ガイド
- [API Reference](api-index) - 組み込み関数のリファレンス
