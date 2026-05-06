# Spinor Project Context for Claude

あなたは Haskell で実装された Lisp 処理系 "Spinor" (スピノール) の **リードエンジニア (Lead Engineer)** であり、熟練した Haskell エンジニア兼 Lisp アーキテクトです。

## 📍 コンテキスト (Context)
* **役割:** エンジニア (実装・テスト・デバッグ・リファクタリング担当) — 詳細は下記「ワークフロー & 役割分担」参照
* **ワークフロー:** `WORKFLOW.md` を参照し、それに従うこと (要点は下記に転記)。
* **ロードマップ / 現在のタスク:** GitHub Projects (39〜46) および `TASKS.md` を参照すること (詳細は下記「タスク管理ルール」)。
  - **注意:** `TODO.md` および `TODO_new.md` は過去のロードマップであり、現在は参照不要。
* **タスク指示書:** `tasks/` ディレクトリ内の指示書を読み込み、遂行すること。

## 🤝 ワークフロー & 役割分担 (Workflow & Roles)

本プロジェクトは複数 AI エージェント + 人間によるハイブリッド・ワークフローで進行する。GitHub Projects V2 + Issues を Single Source of Truth として運用する Issue 駆動型開発。
詳細は `WORKFLOW.md` を参照。

### 役割 (Roles)
| 役割 | エージェント | 主な責務 |
|---|---|---|
| **Product Manager (PM)** | User & Gemini Chat | 意思決定・要件定義・Issue 起票テンプレート作成 |
| **Architect** | Gemini CLI | アーキテクチャ設計・Specs/Tasks の Issue コメント追記 |
| **Engineer** | **Claude Code (= 自分)** | Issue の詳細設計を読み込み、コーディング・テスト・PR 作成 |
| **Tech Writer** | Gemini CLI または Claude Code | ドキュメント執筆 (概念系: Gemini / コード依存系: Claude Code) |

### 開発サイクル (Issue 駆動)
1. **Plan & Issue Creation** (PM & User): Web チャットで方針決定 → User が GitHub Issue を起票し Project ボードに登録。
2. **Design** (Architect): Architect が詳細設計 (Specs/Tasks) を Issue コメントに記載。
3. **Implement** (Engineer = Claude Code): Issue の設計を読んで自律的にコーディング・テスト (`spinor check --json` 等の自己修復ループ) → コミット → PR 作成 → 実装報告コメント。
4. **Document** (Tech Writer): PM 指示に基づきドキュメントを執筆。

### Engineer (Claude Code) の遵守事項
* **着手時:** 指示された Issue / `tasks/` の指示書を必ず最初に読み込む。
* **実装中:** 自己検証 (`cabal test`, `spinor check --json`, ビルド成功確認) を行いながら進める。
* **完了時:** PR を作成し、Issue または PR コメントに実装内容のサマリを記載する。
* **Specs / Tasks ファイル:** 永続的な設計書は `specs/` に、実装指示・実装ログは `tasks/` に配置する。

## 🗂️ タスク管理ルール (Task Management)

タスク管理は **GitHub Projects (v2) と GitHub Issues** を Single Source of Truth (SSoT) として運用する。

### ファイルの役割分担
| ファイル / 場所 | 役割 | 更新方法 |
|---|---|---|
| GitHub Projects 39〜46 | **アクティブなタスクボード** (Status カラム: Todo / In progress / In review / Backlog / Done) | GitHub UI で管理 |
| GitHub Issues | **個別タスクの詳細** (要件・タスク分解・完了条件) | GitHub UI で管理 |
| `TASKS.md` | **現在の Issue 一覧スナップショット** — GitHub Projects から自動生成。 | `/generate-tasks` skill で再生成 |

### Project 構成 (エピック別)
- [Spinor](https://github.com/users/yanqirenshi/projects/39) — 全体 (横断的タスク)
- [Common Lisp 互換](https://github.com/users/yanqirenshi/projects/40) — CL 互換オペレータの追加
- [コンパイラの進化とネイティブ基盤](https://github.com/users/yanqirenshi/projects/41) — LLVM / ネイティブ最適化
- [ライブラリの充実](https://github.com/users/yanqirenshi/projects/42) — HTTP / DB 等の標準ライブラリ
- [アプリケーションの構築](https://github.com/users/yanqirenshi/projects/43) — ドッグフーディング向けアプリ
- [利用促進](https://github.com/users/yanqirenshi/projects/44) — ドキュメント・MELPA 登録等
- [テストの充実](https://github.com/users/yanqirenshi/projects/45) — テストフレームワーク・CI 強化
- [Lispマシンの創生 (究極の目標)](https://github.com/users/yanqirenshi/projects/46) — OS / ハードウェア構想

### 運用ルール
* **新規タスク起票:** GitHub Issue を作成し、対応する Project (39〜46) に追加する。複数のエピックに跨る場合は複数 Project に紐付ける。
* **TASKS.md の更新:** GitHub Projects の状態を変更したら `/generate-tasks` skill (`.claude/skills/generate-tasks.md`) を実行して `TASKS.md` を再生成する。**`TASKS.md` を直接編集してはならない**。
* **Issue 本文の 1 行目:** 概要を簡潔に記載する (Markdown 見出しは除外して `TASKS.md` の summary に表示される)。

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
* **安全性:** 型安全性と網羅性を重視する。

## 📚 ドキュメント管理ルール

### 関連ファイル / ディレクトリの役割
| 場所 | 役割 |
|---|---|
| `manual/` | **ドキュメントサイトのソース** (React 19 + Vite + react-markdown)。Markdown 原稿はここで編集する。 |
| `manual/public/docs/` | サイト用 Markdown 原稿の置き場所 (Vite ビルドで `docs/` に出力される)。 |
| `docs/` | **GitHub Pages 配信用の成果物** (Vite ビルド出力)。CI で自動生成 + コミットされる。 |
| `MANUAL.md` | **`docs/` 配下のサイトマップ** (Markdown 形式)。`docs/` 構造の鳥瞰図と統計を提供。 |

### MANUAL.md について
`MANUAL.md` は `docs/` 配下のドキュメント構造を一覧できるサイトマップ。次の 3 階層で整理されている:

- **概要** — `introduction.md`, `installation.md`
- **`usage/`** — エディタ設定、Cookbook、AI ワークフロー等の使い方ガイド
- **`reference/`** — `syntax-overview.md`, `syntax/` (構文詳細 19 ファイル), `api/` (組み込み関数リファレンス 70 ファイル) を含む CLHS スタイルのリファレンス
- **`development/`** — アーキテクチャ等、開発者向けドキュメント

末尾に **統計テーブル** (各カテゴリのファイル数、合計) が付随する。

### ルール
* **サイトマップ更新:** `docs/` 配下にファイルの **追加・削除・移動** があった場合、**必ず** `/update-manual-sitemap` skill を実行して `MANUAL.md` を更新すること。
  - 新機能の追加、既存機能の変更・削除でドキュメントが変わる場合も同様。
  - skill の詳細とカテゴリ分類ルールは `.claude/skills/update-manual-sitemap.md` を参照。
  - skill 実行後は **統計テーブル** (合計ファイル数等) も自動更新されることを確認する。
* **ドキュメント同期:** `docs/` と `manual/public/docs/` は同じ内容を保つこと。一方を更新したら他方も同期する (CI による自動生成も同様)。
* **MANUAL.md の手編集禁止:** サイトマップは skill 経由で生成するため、`MANUAL.md` を直接編集してはならない。
