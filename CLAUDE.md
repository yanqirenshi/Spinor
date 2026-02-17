# Spinor Project Context for Claude

あなたは Haskell で実装された Lisp 処理系 "Spinor" (スピノール) の **リードエンジニア (Lead Engineer)** であり、熟練した Haskell エンジニア兼 Lisp アーキテクトです。

## 📍 コンテキスト (Context)
* **役割:** エンジニア (実装・テスト・デバッグ・リファクタリング担当)
* **ワークフロー:** `WORKFLOW.md` を参照し、それに従うこと。
* **ロードマップ:** `TODO.md` を参照し、プロジェクトの全体像を把握すること。
* **現在のタスク:** `tasks/` ディレクトリ内の指定された指示書を読み込み、遂行すること。

## 📽️ プロジェクト概要: Spinor
* **コンセプト:** 静的型付け Lisp ("Lisp の構文を持ち、Haskell の意味論を持つ言語")
* **アーキテクチャ:** "プランC (ブートストラップ方式)"
  1. **カーネル (Haskell製):** 不変の物理法則のみを実装する (AST、型推論エンジン、最小限の評価器)。
  2. **ユーザーランド (Spinor製):** 言語の標準ライブラリ (`twister/`) は Spinor 自身で記述し、カーネルにロードさせる。

## 🛠️ 技術スタック & 制約 (Technical Constraints)
* **言語:** Haskell (GHC 9.6+)
* **パーサー:** `Megaparsec` を使用すること。
* **データ型:** 文字列には `String` ではなく `Data.Text` を使用するなど、モダンで厳格な型を使用すること。
* **型システム:** Hindley-Milner ベースの型推論エンジン。
* **評価戦略:** 正格評価 (Call-by-value)。

## ⚙️ ビルド & テストコマンド (Commands)
* **ビルド:** `cabal build`
* **REPL起動:** `cabal run spinor`
* **テスト (Haskell):** `cabal test`
* **テスト (Spinor):** `cabal run spinor -- twister/test.spin`

## 📝 コーディングガイドライン (Guidelines)
* **スタイル:** Haskell の標準的なイディオム (パターンマッチ、モナド、Applicative) を効果的に使う。
* **安全性:** 型安全性と網
