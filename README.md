# Spinor

静的型付け Lisp — "Lisp の構文を持ち、Haskell の意味論を持つ言語"。

## 必要環境

- GHC 9.6 以上
- cabal-install 3.0 以上

[GHCup](https://www.haskell.org/ghcup/) でまとめてインストールできます。

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## ビルド

```bash
cabal build
```

## 実行

```bash
cabal run spinor
```

REPL が起動します。

```
Spinor REPL (step1)
spinor> 42
EInt 42
spinor> #t
EBool True
spinor> (+ 1 2)
EList [ESym "+",EInt 1,EInt 2]
spinor> (define x (+ 10 20))
EList [ESym "define",ESym "x",EList [ESym "+",EInt 10,EInt 20]]
```

Ctrl-D で終了します。

## フォルダ構成

```
Spinor/
├── app/                -- 実行エントリーポイント
│   └── Main.hs
├── src/                -- ライブラリ
│   └── Spinor/
│       └── Syntax.hs   -- パーサー・AST (Spinor.Syntax)
├── tasks/              -- 実装指示ファイル
│   └── step1.md
├── test/               -- テスト (将来)
├── spinor.cabal        -- ビルド定義
├── .gitignore
└── README.md
```

## プロジェクトの進め方

設計や仕様の「頭脳部分」はこのチャット（Gemini）で、
実際のコーディングや実装の「手足部分」は Claude CLI で、という使い分け。

Gemini はプロジェクトの全体像と文脈を保持し続け、
Claude に渡すべき「正確な指示書」を作成する役割を担います。

## Philosophy (哲学)

```
Principles are simple, structures are complex.
(原理は単純に、構造は複雑に)
```
Spinor は Haskell で実装された静的型付け Lisp 処理系です。
このプロジェクトは、ロジャー・ペンローズの数理物理学における **スピノル (Spinor)** と **ツイスター (Twistor)** の関係にインスパイアされています。

ツイスターがスピノルの対から構成されるように、この言語は「最小単位の単純な核 (Spinor)」から「複雑で表現力豊かな世界 (Twister)」を構築することを目指しています。

### Architecture

プロジェクトは明確な2つのレイヤーで構成されています。

1.  **Spinor (Kernel)**
    * **役割:** 「物理法則」。
    * **実装:** Haskell。
    * **概念:** 最小限の公理系。原子的で不変。評価器、型推論、そして最小限のプリミティブのみを提供します。
    * **Motto:** "Keep the kernel small."

2.  **Twister (Userland / Bootstrap)**
    * **役割:** 「化学反応」。
    * **実装:** Spinor 言語自身による記述。
    * **概念:** Spinor の上に構築される標準ライブラリ群。単純な原理を組み合わせ、リスト操作、論理演算、制御構造などの複雑な構造を定義します。
    * **Goal:** To demonstrate that a rich environment can emerge from simple principles.

## Influences

* **Common Lisp & Arc:** 構文の柔軟性と表現力の追求。
* **Haskell:** 意味論（セマンティクス）、型安全性、そして実装言語として。
* **Twistor Theory:** 命名規則と構造的哲学の基盤。

## Links

- [スピノール (Wikipedia)](https://ja.wikipedia.org/wiki/%E3%82%B9%E3%83%94%E3%83%8E%E3%83%BC%E3%83%AB)
- [Arcからの挑戦](https://practical-scheme.net/wiliki/wiliki.cgi?Arc%E3%81%8B%E3%82%89%E3%81%AE%E6%8C%91%E6%88%A6)
