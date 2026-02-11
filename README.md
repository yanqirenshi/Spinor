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
