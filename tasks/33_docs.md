# Step 33: Documentation & Polish - 実装指示書

## 概要

このタスクは、Spinor プロジェクトの最終仕上げとして、ドキュメントを拡充し、コマンドラインインターフェースを洗練させることを目的とします。ユーザーがプロジェクトの全体像を把握し、快適に利用開始できるように整備してください。

仕様書 `specs/33_docs.md` に基づき、以下の手順で実装を進めてください。

## Steps (実装手順)

### 1. `README.md` の全面的なリファクタリング

プロジェクトの最新の状況を反映するよう `README.md` を書き換えます。

-   **Features セクションの更新:** 仕様書にリストされている最新の機能（Native Compilation, TCO, WASM, SLY Integration 等）を追記してください。
-   **Quick Start の拡充:** `spinor build` コマンドを使って "Hello, World" プログラムをネイティブバイナリとしてコンパイル・実行する手順を追加してください。
-   **Live Demo の追加:** `[Try Spinor in your browser!](path/to/your/browser/repl.html)` のような、ブラウザ REPL へのリンク（現時点ではプレースホルダーで可）を追加してください。

### 2. ドキュメントの新規作成

`docs/` ディレクトリに、言語リファレンスと開発環境ガイドを新たに追加します。

1.  **`docs/reference.md` の作成:**
    -   Spinor の主要な特殊形式 (`defun`, `if`, `let` 等) と、基本的な組み込み関数 (`+`, `cons`, `print` 等) の一覧を作成してください。
    -   それぞれの項目について、簡単な説明と使用例を記述してください。（例: `(if cond then-expr else-expr) - cond が #f または nil でなければ then-expr を、さもなくば else-expr を評価する。`）

2.  **`docs/emacs.md` の作成:**
    -   Emacs と SLY を使った開発環境のセットアップ手順を記述してください。
    -   `spinor-mode.el` を `~/.emacs.d/lisp/` などに配置し、`init.el` にロードするための設定例を記載します。
    -   `spinor server` を起動し、`M-x sly-connect` で接続する具体的な手順を記述します。

### 3. CLI の洗練 (`app/Main.hs`)

`optparse-applicative` ライブラリを導入して、コマンドラインインターフェースを堅牢かつユーザーフレンドリーにします。

1.  **依存関係の追加:**
    -   `package.yaml` の `dependencies` に `optparse-applicative` を追加してください。

2.  **引数パーサの再実装:**
    -   `app/Main.hs` の `main` 関数を修正し、現在の手動での引数解析を `optparse-applicative` を使った実装に置き換えます。
    -   `repl`, `build <file>`, `compile <file>`, `server [--port <p>]` のサブコマンドを定義してください。
    -   `--help` と `--version` オプションを実装してください。
    -   **ヒント:** `execParser` を使い、各サブコマンドに対応するデータ型とパーサを定義します。

    ```haskell
    -- データ型定義の例
    data Command
      = Repl
      | Build FilePath
      | Compile FilePath
      | Server Int

    -- optparse-applicative のパーサ定義例
    opts :: ParserInfo Command
    opts = info (commandParser <**> helper) (fullDesc <> progDesc "The Spinor Lisp Interpreter and Compiler")

    commandParser :: Parser Command
    commandParser = subparser (
         command "repl" (info (pure Repl) (progDesc "Start the interactive REPL"))
      <> command "build" (info (Build <$> argument str (metavar "FILE")) (progDesc "Build a native executable"))
      <> ...
      )

    -- main 関数内
    main = do
      cmd <- execParser opts
      case cmd of
        Repl -> replMode
        Build file -> buildMode file
        ...
    ```

3.  **バージョン表示機能:**
    -   Cabal が自動生成する `Paths_spinor` モジュールをインポートします。
    -   `Data.Version` の `showVersion` を使って、`package.yaml` 由来のバージョン番号を表示するロジックを `--version` オプションに組み込んでください。

    ```haskell
    import Data.Version (showVersion)
    import Paths_spinor (version) // `package.yaml` の name に合わせる

    -- バージョン表示用の option パーサ
    versionOption :: Parser (a -> a)
    versionOption = infoOption (showVersion version) (long "version" <> short 'v' <> help "Show version")
    ```

---

### 実装報告

**実装完了後、この Markdown ファイルを直接編集し、以下の2つのセクションを追記して実装内容を報告してください。**

#### Implementation Policy (実装方針)

*(ここに、実装する上で考慮した点や設計判断、採用したアプローチなどを記述してください)*

#### Implementation Details (実装内容)

*(ここに、具体的なコードの変更点や追加した関数の役割、苦労した点などを記述してください)*
