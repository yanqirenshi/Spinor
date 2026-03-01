# 54: Project Template & AI Context の実装指示書

## 1. 目的
`spinor init` コマンドを実装し、AI 協調開発に最適化されたプロジェクト雛形（特に `CLAUDE.md`）を生成可能にする。

## 2. 実装手順

### Step 1: テンプレート定義モジュールの作成
- `src/Spinor/Template.hs` を新規作成する。
- 以下の定数を定義（`Data.Text` 型）:
    - `mainSpin`: シンプルなコード例。
    - `testSpin`: テストの例。
    - `gitignore`: 標準的な除外設定。
    - `claudeMd`: AI 向けの指示書（英語推奨）。
        - 言語の性質、コマンド、高度な機能の紹介。

### Step 2: CLI コマンドの追加 (`app/Main.hs`)
- `Main.hs` に `spinor init <name>` コマンドを実装。
- ディレクトリ作成とファイル書き出しのロジックを実装。

### Step 3: `CLAUDE.md` の内容執筆
- `claudeMd` 定数に、AI がプロジェクトを理解するのに十分なメタ情報を記述する。

### Step 4: 検証
- `spinor init sample-app` を実行し、生成物を確認。
- 生成されたコードが `spinor` コマンドで実行可能か確認。

## 3. 完了条件
- `spinor init` が正常に動作し、`CLAUDE.md` を含むプロジェクトが生成されること。
- `CLAUDE.md` が AI エージェントにとって有用な情報を提供していること。

---
## 実装報告

### 実装方針

AI エージェント (Claude Code 等) が Spinor プロジェクトを自律的に理解できるよう、包括的な `CLAUDE.md` テンプレートを設計。`spinor init` コマンドでプロジェクト雛形を生成し、即座に開発を開始できる環境を提供する。

### 実装内容

#### 1. テンプレートモジュールの作成 (`src/Spinor/Template.hs`)

4つの Text 定数を定義:

**`mainSpin`**: エントリポイントテンプレート
- `defpackage` / `in-package` によるモジュール構成
- シンプルな `greet` 関数と `main` 関数の例

**`testSpin`**: テストスイートテンプレート
- `:use` によるパッケージインポート
- `test` / `assert` を使ったテスト例

**`gitignore`**: 標準的な除外設定
- ビルド出力 (`dist/`, `*.o`, `*.c`)
- 実行ファイル、WASM 出力
- エディタ・OS ファイル

**`claudeMd`**: AI エージェント向けコンテキスト (英語)
- Quick Reference: 全 CLI コマンド
- Language Overview: Lisp 構文 + Haskell セマンティクス
- Core Syntax: 変数定義、関数、let、if、match
- Type System: HM 型推論の説明
- ADT: data 定義とパターンマッチ
- Package System: `defpackage`, `in-package`, `:use`, `:export`
- Experimental Features: Ownership, Region
- Common Patterns: テスト、エラーハンドリング、リスト処理
- Style Guidelines / Troubleshooting

#### 2. CLI コマンドの追加 (`app/Main.hs`)

```haskell
-- コマンドパターンを追加
["init", name] -> initMode name

-- initMode 関数
initMode :: String -> IO ()
initMode projectName = do
  -- ディレクトリ作成
  createDirectoryIfMissing True (projectDir ++ "/src")
  createDirectoryIfMissing True (projectDir ++ "/test")
  -- ファイル書き出し
  TIO.writeFile (srcDir ++ "/main.spin") mainSpin
  TIO.writeFile (testDir ++ "/test.spin") testSpin
  TIO.writeFile (projectDir ++ "/.gitignore") gitignore
  TIO.writeFile (projectDir ++ "/CLAUDE.md") claudeMd
```

ヘルプメッセージにも `init <name>` コマンドを追加。

#### 3. 動作確認

```bash
$ spinor init sample-app
Creating project: sample-app
  Created: sample-app/src/main.spin
  Created: sample-app/test/test.spin
  Created: sample-app/.gitignore
  Created: sample-app/CLAUDE.md

Project created successfully!

Next steps:
  cd sample-app
  spinor src/main.spin    # Run the application
  spinor test/test.spin   # Run tests
```

生成されたファイル構造:
```
sample-app/
  src/
    main.spin      (371 bytes)
  test/
    test.spin      (479 bytes)
  .gitignore       (181 bytes)
  CLAUDE.md        (4967 bytes)
```

#### テスト結果

```
229 examples, 0 failures
```

### 完了日
2026-02-26
