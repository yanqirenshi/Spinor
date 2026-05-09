---
description: docs/ 配下のサイトマップを MANUAL.md に反映する
allowed-tools: Read, Write, Edit, Glob, Bash
---

`.claude/skills/update-manual-sitemap.md` を読み込み、そこに記載された手順 (トリガー条件・実行手順・カテゴリ分類ルール) に厳密に従って `MANUAL.md` のサイトマップを更新してください。

実行手順の概要:
1. `docs/` 配下の現在のファイル構造を `Glob: docs/**/*` で取得する。
2. `MANUAL.md` を読み込み、現在のサイトマップとの差分 (追加・削除・移動) を特定する。
3. `MANUAL.md` のサイトマップツリーを更新し、末尾の統計テーブル (各カテゴリのファイル数、合計) も整合させる。
4. 完了後、`{追加|削除|移動}` のサマリと統計の前後値を 1 ブロックで報告する。

注意:
- `MANUAL.md` を直接手編集する運用は禁止 (CLAUDE.md ルール)。本コマンド経由でのみ更新する。
- `docs/` と `manual/public/docs/` の同期確認も併せて行い、片側にしか存在しないファイルがあれば報告する。
