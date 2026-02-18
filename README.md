# Spinor (スピノル)

> **Static Lisp with Haskell Semantics**

Spinor は Haskell で実装された静的型付け Lisp 処理系です。
ロジャー・ペンローズの **スピノル (Spinor)** と **ツイスター (Twistor)** の関係にインスパイアされ、「最小単位の単純な核」から「複雑で表現力豊かな世界」を構築することを目指したプロジェクトです。

## Features

- **Hindley-Milner Type System** — Let多相を備えた静的型推論 (Algorithm W)
- **Native Compilation** — C言語経由でのネイティブバイナリ生成
- **Tail Call Optimization (TCO)** — 自己再帰のループ変換によるスタック消費の抑制
- **WebAssembly Support** — ブラウザ上で動作する REPL ([Demo](docs/index.html))
- **SLY/SLIME Integration** — Swank プロトコルによる Emacs インタラクティブ開発

## Quick Start

### 必要環境

- GHC 9.6 以上
- cabal-install 3.0 以上
- Cコンパイラ (GCC 等) — ネイティブコンパイル機能を使用する場合

### REPL を起動

```bash
cabal run spinor
```

Twister (標準ライブラリ) が自動的にロードされます。

```lisp
Spinor REPL (step16)
Loading Twister environment...
Twister loaded.

spinor> (map (fn (x) (* x x)) (cons 1 (cons 2 (cons 3 nil))))
:: [Int]
(1 4 9)

spinor> (let id (fn (x) x) (if (id #t) (id 1) 0))
:: Int
1

spinor> (fact 5)
:: Int
120
```

### ネイティブバイナリを生成

```bash
cabal run spinor -- build hello.spin
```

`hello.spin` を C言語にトランスパイルし、GCC でネイティブバイナリを生成します。

## Commands

| コマンド | 説明 |
|---------|------|
| `spinor` | REPL を起動 (デフォルト) |
| `spinor <file>` | ファイルをバッチ実行 |
| `spinor build <file>` | ネイティブバイナリを生成 |
| `spinor compile <file>` | Cソースコードのみを出力 |
| `spinor server [--port <port>]` | SLY 接続用の Swank サーバーを起動 |

## Documentation

- [Language Reference](docs/reference.md) — 特殊形式とプリミティブ関数
- [Emacs Integration](docs/emacs.md) — SLY/spinor-mode の設定方法

## Philosophy & Architecture

### 1. Spinor (The Kernel)

- **役割:** 「物理法則」— 不変の公理系
- **実装:** Haskell
- **特徴:** 最小限の核。Hindley-Milner 型推論、Let多相、マクロ展開フェーズを備えた評価器
- **Motto:** "Keep the kernel small."

### 2. Twister (The Userland)

- **役割:** 「化学反応」— カーネルの上に構築される世界
- **実装:** Spinor 言語自身 (Self-hosted)
- **特徴:** 標準ライブラリ群。リスト操作、論理演算、制御構造を定義
- **Goal:** 単純な原理から豊かな環境が立ち上がることを実証する

## Folder Structure

```text
Spinor/
├── app/
│   └── Main.hs           -- エントリーポイント (REPL, Compiler, Server)
├── src/Spinor/
│   ├── Syntax.hs         -- AST, Parser
│   ├── Eval.hs           -- 評価器 (Evaluator)
│   ├── Infer.hs          -- 型推論 (Type Inference / Unification)
│   ├── Expander.hs       -- マクロ展開 (Macro Expander)
│   ├── Compiler/
│   │   └── Codegen.hs    -- C言語コード生成
│   └── Server.hs         -- Swank サーバー
├── runtime/
│   └── spinor.c          -- ネイティブ実行用ランタイム
├── twister/              -- 標準ライブラリ (Spinor 製)
│   ├── core.spin         -- 基本論理・マクロ
│   ├── list.spin         -- リスト操作
│   └── math.spin         -- 数学関数
├── docs/                 -- WASM REPL デモ
└── editors/
    └── emacs/
        └── spinor-mode.el  -- Emacs メジャーモード
```

## Influences

- **Common Lisp & Arc:** 構文の柔軟性と表現力の追求
- **Haskell:** 意味論（セマンティクス）、型安全性、そして実装言語として
- **Twistor Theory:** 命名規則と構造的哲学の基盤
