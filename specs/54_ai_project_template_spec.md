# 54: Project Template & AI Context (`CLAUDE.md`) Specification

## 1. 概要
Spinor プロジェクトの標準的な雛形を自動生成する機能を追加し、同時に AI エージェント（Claude Code 等）がプロジェクトを自律的に理解するための設定ファイル `CLAUDE.md` を提供する。

## 2. コマンド仕様
### `spinor init <project-name>`
- **機能:** 指定された名前のディレクトリを作成し、標準的なファイル構成を書き出す。
- **生成される構成:**
    - `<project-name>/`
        - `src/main.spin`: エントリポイント。基本の Hello World と関数定義。
        - `test/test.spin`: テストコードの例。
        - `CLAUDE.md`: AI エージェント向けのガイドライン。
        - `.gitignore`: `dist/` やバイナリを除外。

## 3. `CLAUDE.md` の定義
AI エージェントがこのディレクトリを開いた際に、以下の情報を即座に把握できるようにする。

- **Project Type:** "Spinor (Static Lisp with Haskell Semantics)"
- **Build/Run Commands:** 
    - 実行: `spinor src/main.spin`
    - ビルド: `spinor build src/main.spin`
    - テスト: `spinor test/test.spin`
- **Language Rules:**
    - 構文は Lisp だが、セマンティクスは Haskell (静的型付け、HM型推論)。
    - モジュール・パッケージシステム (`defpackage`, `in-package`) の利用推奨。
    - 実験的機能 (Ownership, Regions) のアノテーション記法の存在。
- **Style Guide:**
    - トップレベル定義は `def` または `defun`。
    - 型エラーは実行前に報告されるため、型不整合に注意する。

## 4. テンプレートコンテンツの管理
- テンプレートの各ファイル内容は `src/Spinor/Template.hs` にハードコードされた Text 定数として保持する。
- ユーザーが `init` を実行した際、これらの定数が実ファイルとして展開される。
