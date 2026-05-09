---
description: GitHub Projects (39〜46) から TASKS.md を再生成する
allowed-tools: Read, Write, Edit, Bash
---

`.claude/skills/generate-tasks.md` を読み込み、そこに記載された手順に厳密に従って `TASKS.md` を再生成してください。

実行手順の概要:
1. `gh` CLI を使って GitHub Projects 39〜46 から Issue 一覧 (Status / Title / Issue 番号 / リポジトリ) を取得する。
2. skill ファイルで指定されたフォーマットに従い `TASKS.md` を再生成する。
3. 完了後、Project ごとの件数サマリを報告する。

注意:
- `TASKS.md` は GitHub Projects の **スナップショット** であり、直接手編集してはならない (CLAUDE.md ルール)。本コマンド経由でのみ更新する。
- Issue 本文の 1 行目を summary として使用する (Markdown 見出しは除外)。
