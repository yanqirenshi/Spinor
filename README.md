# Spinor (スピノル)

> "Principles are simple, structures are complex."
> (原理は単純に、構造は複雑に)

Spinor は Haskell で実装された静的型付け Lisp 処理系です。
ロジャー・ペンローズの **スピノル (Spinor)** と **ツイスター (Twistor)** の関係にインスパイアされ、「最小単位の単純な核」から「複雑で表現力豊かな世界」を構築することを目指したプロジェクトです。

## Philosophy (哲学) & Architecture

### 1. Spinor (The Kernel)

* **役割:** 「物理法則」。
* **実装:** Haskell。
* **特徴:** 最小限の公理系。Hindley-Milner 型推論、Let多相、マクロ展開フェーズを備えた評価器。
* **Motto:** "Keep the kernel small."

### 2. Twister (The Userland / Bootstrap)
* **役割:** 「化学反応」。
* **実装:** Spinor 言語自身 (Self-hosted)。
* **特徴:** Spinor の上に構築される標準ライブラリ群。リスト操作、論理演算、制御構造 (`cond` 等) を定義します。
* **Goal:** 単純な原理から豊かな環境が立ち上がることを実証する。

---

## Features (実装済み機能)

* **Static Typing:** Hindley-Milner 型推論 (Algorithm W) による完全な静的型付け。
* **Let Polymorphism:** `let` 束縛における多相性のサポート。
* **Macro System:** `mac` によるユーザー定義マクロと、評価前の展開フェーズ (Expansion Phase)。
* **Twister Library:** `map`, `filter`, `foldl`, `cond` などを提供する標準ライブラリ。
* **REPL:** 型情報を表示する対話環境。

## Folder Structure (構成)

```text
Spinor/
├── app/
│   └── Main.hs           -- エントリーポイント (REPL, Bootloader)
├── src/Spinor/
│   ├── Syntax.hs         -- AST, Parser
│   ├── Eval.hs           -- 評価器 (Evaluator)
│   ├── Infer.hs          -- 型推論 (Type Inference / Unification)
│   ├── Expander.hs       -- マクロ展開 (Macro Expander)
│   ├── Type.hs           -- 型定義
│   ├── Primitive.hs      -- プリミティブ関数
│   └── Val.hs            -- 値の定義
├── twister/              -- 標準ライブラリ (Userland)
│   ├── boot.spin         -- ブートローダー
│   ├── core.spin         -- 基本論理・マクロ
│   ├── list.spin         -- リスト操作
│   └── math.spin         -- 数学関数
├── tasks/                -- 開発履歴 (AIへの指示書)
├── spinor.cabal          -- ビルド定義
└── README.md
```

## Usage (実行方法)

### 必要環境

* GHC 9.6 以上
* cabal-install 3.0 以上

### 実行

```bash
cabal run spinor
```

REPL が起動し、自動的に `twister/` 配下のライブラリがロードされます。

```lisp
Spinor REPL (step14)
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

## Development History (開発の軌跡)

このプロジェクトは、Gemini (設計・文脈保持) と Claude CLI (実装) のペアプログラミングにより、以下の14ステップで構築されました。

1. **AST & Parser:** S式の読み込み
2. **Evaluator:** 状態を持つ計算機
3. **Functions:** クロージャの実装
4. **Primitives:** 再帰とリスト基本操作
5. **Bootstrap:** `load` 機能と Twister の分離
6. **Stdlib Expansion:** 数学・リスト関数の拡充
7. **Macros:** ユーザー定義マクロ (`mac`)
8. **Varargs:** 可変長引数と `cond`
9. **Expansion Phase:** マクロ展開と評価の分離
10. **Type System Base:** 型定義と単一化 (Unification)
11. **Inference:** Algorithm W の実装
12. **Generalization:** `define` の多相化
13. **Let Polymorphism:** `let` のカーネル化と多相対応
14. **Final Polish:** 型安全な Twister ライブラリの完成

## Influences

* **Common Lisp & Arc:** 構文の柔軟性と表現力の追求。
* **Haskell:** 意味論（セマンティクス）、型安全性、そして実装言語として。
* **Twistor Theory:** 命名規則と構造的哲学の基盤。
