# Spinor Development Workflow

このプロジェクトは、複数のAIエージェントと人間の協調作業によって進行します。
以下の役割分担とフローに従って開発を進めてください。

## 🏛️ Roles & Responsibilities

| Role | Agent | Context | Responsibilities |
| :--- | :--- | :--- | :--- |
| **Product Manager (PM)** | **User & Gemini Chat** | Web Chat | **意思決定・管理**<br>- `TODO.md` の管理<br>- プロジェクトの方針決定<br>- タスクの優先順位付け |
| **Architect** | **Gemini CLI** | Terminal | **設計・仕様策定**<br>- ソースコード全量を読み込む<br>- 機能要件から技術仕様 (`specs/`) を作成<br>- 実装指示書 (`tasks/`) を作成 |
| **Engineer** | **Claude Code** | Terminal | **実装・テスト**<br>- `specs/` と `tasks/` に基づきコーディング (`src/`, `twister/`)<br>- テストの作成と実行 (`test/`)<br>- 実装完了の報告 |
| **Tech Writer** | **Gemini CLI** | Terminal | **文書化**<br>- 完成したコードを読み込む<br>- ユーザー向けドキュメント (`docs/`) の作成・更新<br>- リファレンスマニュアルの整備 |

## 📂 Directory Structure

* `TODO.md`: 全体のロードマップ (PM管理)
* `workflow.md`: この開発フロー定義書
* `specs/`: **機能仕様書 (Specifications)**
    * Architect が作成する永続的なドキュメント。
    * 実装の詳細、型定義、構文ルールなどを記述。
    * 例: `specs/01_syntax.md`, `specs/17_data_types.md`
* `tasks/`: **実装指示書 (Tasks)**
    * Architect が Engineer に渡す作業チケット。
    * 具体的な変更ファイル名や手順を記述。
    * 実装完了後はアーカイブしてもよい。
* `docs/`: **ユーザーマニュアル (Documentation)**
    * Tech Writer が作成する利用者向け文書。
    * CLHS (Common Lisp HyperSpec) スタイルを目指す。
* `src/`, `twister/`: ソースコード (Engineer 領域)
* `test/`: テストコード (Engineer 領域)

## 🔄 Development Cycle

1.  **Plan (PM):** `TODO.md` で次のタスクを決定する。
2.  **Design (Architect):**
    * Gemini CLI に全コードを読ませる。
    * `specs/new_feature.md` (仕様) と `tasks/step_XX.md` (指示書) を生成させる。
3.  **Implement (Engineer):**
    * Claude Code に `tasks/step_XX.md` を渡し、実装とテストを行わせる。
    * テストが通ったら完了報告を行う。
4.  **Document (Tech Writer):**
    * 実装完了後、Gemini CLI に変更点を読ませ、`docs/` を更新させる。
