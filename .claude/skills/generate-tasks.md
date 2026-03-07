# Skill: generate-tasks

GitHub Projects (v2) からタスクリストを取得し、`TASKS.md` に出力するスキルです。

## トリガー条件

以下の場合にこのスキルを実行してください：

1. ユーザーが `/generate-tasks` を実行した場合
2. ユーザーが「タスクリストを更新して」等を依頼した場合

## 対象プロジェクト

以下の GitHub Projects からタスクを取得します：

| No. | Project URL | 説明 |
|-----|-------------|------|
| 39 | https://github.com/users/yanqirenshi/projects/39 | Spinor |
| 40 | https://github.com/users/yanqirenshi/projects/40 | Common Lisp 互換 |
| 41 | https://github.com/users/yanqirenshi/projects/41 | コンパイラの進化とネイティブ基盤 |
| 42 | https://github.com/users/yanqirenshi/projects/42 | ライブラリの充実 |
| 43 | https://github.com/users/yanqirenshi/projects/43 | アプリケーションの構築 |
| 44 | https://github.com/users/yanqirenshi/projects/44 | 利用促進 |
| 45 | https://github.com/users/yanqirenshi/projects/45 | テストの充実 |
| 46 | https://github.com/users/yanqirenshi/projects/46 | Lispマシンの創生 |

## 実行手順

1. 各プロジェクトの情報を取得する
   ```bash
   gh project view {NUMBER} --owner yanqirenshi --format json
   ```

2. 各プロジェクトのアイテム（Issue）を取得する
   ```bash
   gh project item-list {NUMBER} --owner yanqirenshi --format json
   ```

3. 取得したデータを整理し、プロジェクトごと・ステータスごとにグループ化する

4. `TASKS.md` に出力する

## 出力フォーマット

```markdown
# TASK LIST

## [{project name}]({project url})

### {status column name}
#### [{issue name} #{issue number}]({issue url})
#### [{issue name} #{issue number}]({issue url})

### {status column name}
#### [{issue name} #{issue number}]({issue url})
```

### フォーマットルール

- `## [{project name}]({project url})` : プロジェクト名とURLのリンク
- `### {status column name}` : GitHub Project v2 の Status カラム名（例: Todo, In progress, Done, Backlog, Ready, In review）
- `#### [{issue name} #{issue number}]({issue url})` : Issue 名、番号、URLのリンク
- 同一 Issue が複数プロジェクトに存在する場合は、それぞれのプロジェクトに表示する

## 権限エラー時の対応

`gh` コマンドで `read:project` スコープが不足している場合：

```bash
gh auth refresh -s read:project
```

を実行するようユーザーに案内してください。

## 出力例

変更完了後、以下の形式で報告：

```
TASKS.md を更新しました:
- 全 8 プロジェクトから 15 件の Issue を取得
- In progress: 2 件
- Backlog: 3 件
- Done: 10 件
```
