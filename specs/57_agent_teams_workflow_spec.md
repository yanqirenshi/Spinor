# 57: Agent Teams Workflow Specification

## 1. 概要
Claude Code の「Agent Teams」機能を最大限に活用するため、Spinor プロジェクト内に複数の AI エージェント（Team Lead と Teammate）が自律的かつ並行して開発を進められるファイルベースのワークフロー基盤を構築する。

## 2. アーキテクチャ (File-based IPC)
エージェント間の通信、タスクの割り当て、および状態管理を `.agents/` ディレクトリ内のファイルシステムを通じて行う。

### 2.1 ディレクトリ構成
```
.agents/
  TEAMS.md              # エージェント向けの行動規範とプロトコル定義
  tasks/
    todo/               # 未割り当てのタスク (.md)
    in-progress/        # 現在作業中のタスク (.md)
    review/             # 実装完了、レビュー待ちのタスク (.md)
    done/               # 完了したタスク (.md)
  mailboxes/            # エージェント間のメッセージパッシング用
    lead/
    teammate-1/
```

## 3. エージェントの役割とルール (`TEAMS.md`)

### 3.1 Team Lead
- **責務:** プロジェクトの全体目標を個別のタスクファイルに分割し、`.agents/tasks/todo/` に配置する。
- **レビュー:** `review/` に移動されたタスクを確認。提出されたコードに対して `spinor check --json` や `spinor test --json` を実行し、問題がなければ `done/` へ移動。リジェクト時はコメントを追記して `todo/` または `in-progress/` に戻す。

### 3.2 Teammate (Developer)
- **取得:** `todo/` からタスクファイルを一つ選び、自身の名前をファイル内に明記した上で `in-progress/` へ移動（ファイルムーブによるアトミックなロック）。
- **実装:** Spinor コードを記述し、自身のスキル (`spinor check --json`) を用いて Self-Healing Loop を回す。
- **提出:** 実装とテストが完了したら、タスクファイルを `review/` へ移動し、Lead にメールボックス等で通知する（または単に配置するだけでも可）。

## 4. プロトコル
- **ロック競合の回避:** ファイルの移動 (mv) 操作をアトミックな操作とみなし、タスクの重複アサインを防ぐ。
- **状態遷移:** `todo` -> `in-progress` -> `review` -> `done` の単方向フローを基本とする。

## 5. 統合
- `spinor init` コマンド拡張により、このディレクトリ構造と `TEAMS.md` が自動生成されるようにする。
