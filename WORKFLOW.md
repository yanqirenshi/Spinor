# Spinor Development Workflow (Issue-Driven AI Workflow)

このプロジェクトは、複数のAIエージェントと人間の高度な協調作業（ハイブリッド・ワークフロー）によって進行します。
プロジェクトのタスク管理は GitHub Projects V2 および Issues を中心（Single Source of Truth）として行われます。

## 🏛️ Roles & Responsibilities

| Role | Agent | Context | Responsibilities |
| :--- | :--- | :--- | :--- |
| **Product Manager (PM)** | **User & Gemini Chat** | Web Chat | **意思決定・要件定義**<br>- プロジェクトの方針決定と壁打ち<br>- Issue 用の要件定義テンプレート（概要・完了条件）の作成<br>- User による GitHub Issue の手動起票と Project ボード管理 |
| **Architect** | **Gemini CLI** (Local `gh` or GitHub Actions) | Terminal / GitHub | **アーキテクチャ設計・仕様策定**<br>- ユーザーの指示（またはメンション）に基づき、Issue に詳細設計 (Specs/Tasks) を作成して追記する<br>- 実装に向けた技術的な道筋を定義する |
| **Engineer** | **Claude Code** | Terminal | **自律実装・テスト**<br>- GitHub Issue の詳細設計 (Specs/Tasks) を読み込み、コーディングとテストを実行<br>- 実装完了後、PR (Pull Request) の作成や Issue への実装内容報告を行う |
| **Tech Writer** | **Gemini CLI** or **Claude Code** | Terminal / GitHub | **ドキュメント執筆**<br>- **概念・ビジョン系 (Gemini CLI):** ホワイトペーパーや高度なアーキテクチャ概念の執筆<br>- **コード依存系 (Claude Code):** API リファレンスや Cookbook など、ソースコードに密結合するドキュメントの執筆<br>- PM の指示に基づきドキュメントを生成し、PR/Issue で報告 |

## 📂 Directory Structure

* `workflow.md`: この開発フロー定義書
* `TODO.md`: 大枠のロードマップ（※詳細なタスク管理は GitHub Projects へ移行済み）
* `specs/`: **機能仕様書 (Specifications)**
    * Issue で議論・策定された設計を、永続的なドキュメントとして残す場所。
* `tasks/`: **実装指示書 (Tasks) / 実装ログ**
    * Engineer への指示内容と、実装後の「実装方針」「実装内容」のログを記録する場所。
* `docs/`, `manual/`: **ユーザーマニュアル (Documentation)**
    * Tech Writer が作成する利用者向け文書（React マニュアルサイト用データなど）。
* `src/`, `twister/`: ソースコード (Engineer 領域)
* `test/`: テストコード (Engineer 領域)
* `.agents/`: **Agent Teams ワークフロー定義** (複数 AI エージェントの自律協調用)

## 🔄 Development Cycle (Issue 駆動型開発)

プロジェクトは以下のサイクルで進行します。

1. **Plan & Issue Creation (PM & User):**
   * Web チャット上で PM (Gemini) と壁打ちを行い、次の一手を決定する。
   * PM が「Issue 起票用の Markdown (概要と完了条件)」を生成する。
   * User が生成されたテキストを GitHub にコピー＆ペーストし、Issue を作成して Project ボードに登録する。
2. **Design (Architect):**
   * User が `gh` コマンドを経由して Gemini CLI に Issue を読み込ませる（または GitHub Actions 上で `@gemini-cli` をメンションする）。
   * Architect が詳細設計（Specs/Tasks）を作成し、Issue のコメント欄等に Markdown で記載する。
   * PM (Gemini Chat) が設計内容をレビュー・確認する。
3. **Implement (Engineer):**
   * User がターミナルで Claude Code に「Issue #XX の詳細設計を読んで実装して」と指示を出す。
   * Claude Code が自律的にコードを変更、テスト (`spinor check --json` 等の自己修復ループを含む) を実行し、完了したらコミットして PR を作成する。
   * Claude Code が実装内容の報告を Issue または PR のコメントに記載する。
4. **Document (Tech Writer):**
   * PM (Gemini Chat) が Issue でドキュメントの構成案 (Specs) を指定する。
   * 対象ドキュメントの性質（概念系かコード依存系か）に応じて Gemini CLI または Claude Code が執筆を行う。
   * PM が内容のトーン＆マナーや正確性をレビューする。
