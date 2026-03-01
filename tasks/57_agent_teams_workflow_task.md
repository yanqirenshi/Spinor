# 57: Agent Teams Workflow の実装指示書

## 1. 目的
`spinor init` によって自動生成されるプロジェクト雛形を拡張し、AI チームが並行開発を行うためのファイルベースのワークフロー基盤（`.agents/` 構造と `TEAMS.md`）を導入する。

## 2. 実装手順

### Step 1: テンプレート定数の追加
- `src/Spinor/Template.hs` を改修し、以下の定数を追加する。
    - `teamsMd`: エージェント向けの行動規範・役割定義（英語で記述）。
    - `sampleTaskMd`: 初期デモ用タスクの雛形（例: `Implement a simple map function in twister/list.spin`）。

### Step 2: `CLAUDE.md` の更新
- 既存の `claudeMd` 定数に、「Agent Teams として稼働する場合は `.agents/TEAMS.md` のルールに従うこと」というディレクティブを追記する。

### Step 3: CLI `init` ロジックの拡張 (`app/Main.hs`)
- `initMode` 関数を改修し、プロジェクト作成時に以下のディレクトリを生成する処理を追加。
    - `.agents/`
    - `.agents/tasks/todo/`
    - `.agents/tasks/in-progress/`
    - `.agents/tasks/review/`
    - `.agents/tasks/done/`
    - `.agents/mailboxes/`
- 生成したディレクトリ内に `TEAMS.md` と `task-001.md`（`todo/` 内）を書き出す。

### Step 4: 検証
- `spinor init team-app` を実行する。
- 期待されるディレクトリツリーとファイルが作成されていることを確認。
- `TEAMS.md` および `CLAUDE.md` の内容が正しく連携していることを目視（または AI パース）で確認。

## 3. 完了条件
- 新規プロジェクト生成時に、AI チーム開発用のディレクトリとルールファイルが適切に配置されること。

---
## 実装報告

### 実装方針

`spinor init` コマンドによるプロジェクト生成時に、AI エージェントチームが並行開発を行うためのファイルベースワークフロー基盤を自動構築する。ディレクトリ構造と規範文書（TEAMS.md）を通じて、Team Lead と Teammate の役割分担、タスクのライフサイクル管理、アトミックなロック機構を実現する。

### 実装内容

#### 1. テンプレート定数の追加 (`src/Spinor/Template.hs`)

**エクスポートリストの拡張:**
```haskell
module Spinor.Template
  ( mainSpin, testSpin, gitignore, claudeMd
  , teamsMd, sampleTaskMd  -- 新規追加
  ) where
```

**`teamsMd` 定数:**
Agent Teams プロトコルを定義する英語のドキュメント。以下のセクションを含む:
- Roles (Team Lead / Teammate)
- Task Lifecycle (`todo` → `in-progress` → `review` → `done`)
- Task File Format
- Lock Protocol (ファイル移動によるアトミックロック)
- Communication (メールボックス)
- Verification Commands (`spinor check --json`)
- Best Practices

**`sampleTaskMd` 定数:**
`map` 関数の実装を例題とするサンプルタスクファイル。

#### 2. `claudeMd` への指示追記

既存の `claudeMd` 定数末尾に以下のセクションを追加:
```markdown
## Agent Teams Mode

If operating as part of an Agent Teams workflow (multiple AI agents working in parallel),
you MUST follow the rules and protocols defined in `.agents/TEAMS.md`.
```

#### 3. CLI `init` ロジックの拡張 (`app/Main.hs`)

**`initMode` 関数の改修:**
```haskell
initMode :: String -> IO ()
initMode projectName = do
  let agentsDir       = projectDir ++ "/.agents"
      todoDir         = agentsDir ++ "/tasks/todo"
      inProgressDir   = agentsDir ++ "/tasks/in-progress"
      reviewDir       = agentsDir ++ "/tasks/review"
      doneDir         = agentsDir ++ "/tasks/done"
      mailboxesDir    = agentsDir ++ "/mailboxes"

  -- ディレクトリ作成
  mapM_ (createDirectoryIfMissing True)
    [ srcDir, testDir, todoDir, inProgressDir, reviewDir, doneDir, mailboxesDir ]

  -- ファイル書き出し
  writeFile (agentsDir ++ "/TEAMS.md") teamsMd
  writeFile (todoDir ++ "/task-001.md") sampleTaskMd
```

#### 4. 動作確認

**生成されるディレクトリ構造:**
```
team-app/
  .agents/
    TEAMS.md
    tasks/
      todo/
        task-001.md
      in-progress/
      review/
      done/
    mailboxes/
  src/
    main.spin
  test/
    test.spin
  CLAUDE.md
  .gitignore
```

**テスト結果:**
```
229 examples, 0 failures
```

### 完了日
2026-03-01
