# Task 39: Reference Generator (docgen) の実装

`src/Spinor/Lsp/Docs.hs` のデータを元に Markdown リファレンスを生成する機能を実装してください。

## ステップ

### 1. データモデルの更新
- `src/Spinor/Lsp/Docs.hs` を開き、`DocEntry` に `docSlug :: Text` フィールドを追加する。
- `primitiveDocs` 内の全エントリに `docSlug` を定義する。
    - 例: `("+", DocEntry "(Int, Int) -> Int" "..." CompletionItemKind_Function "add")`
    - slug は `docs/ref/` 内のファイル名として使用される。

### 2. ジェネレーターモジュールの作成
- `src/Spinor/DocGen.hs` を新規作成する。
- 以下の機能を実装する：
    - `generateDocs :: IO ()`: エントリポイント。
    - `System.Directory.createDirectoryIfMissing True "docs/ref"` を実行。
    - 各 `DocEntry` を `specs/39_docgen.md` のフォーマットに従って Markdown 化し、`docs/ref/<slug>.md` に書き込む。
    - 全エントリをまとめた `docs/reference.md` を作成する。リンクは `doc.html?src=ref/<slug>.md` 形式とする。

### 3. CLI への統合
- `app/Main.hs` を修正する。
- `helpMessage` に `docgen` コマンドの説明を追加。
- `main` 関数の `case args of` に `["docgen"] -> generateDocs` を追加。

### 4. 動作確認
- `cabal run spinor -- docgen` を実行する。
- `docs/ref/` に大量の Markdown ファイルが生成されていることを確認。
- `docs/reference.md` が更新されていることを確認。
- ローカル HTTP サーバー（例: `python -m http.server`）を `docs/` で起動し、ブラウザでリンクが正しく機能するか確認する。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針
(どのような設計判断を行ったか、slug の命名で工夫した点など)

### 実装内容
(変更したファイルの一覧、追加した主要な関数など)

---

## 実装報告

### 実装方針
- **slug の命名規則:** spec に従い、記号を含む関数名は英単語に変換した。
  - 演算子: `+` → `add`, `-` → `sub`, `*` → `mul`, `%` → `mod`
  - 比較演算子: `=` → `eq-op` (既存の `eq` 関数との衝突を避けるため), `<` → `lt`, `>` → `gt`
  - 述語関数: `null?` → `null-p`, `empty?` → `empty-p`, `string=?` → `string-eq`, `file-exists?` → `file-exists-p`
  - 変換関数: `string->list` → `string-to-list`, `list->string` → `list-to-string`
- **カテゴリ分類:** インデックスファイルではエントリを以下の6カテゴリに分類して出力:
  - Special Forms, Arithmetic, Comparison, List Operations, String Operations, I/O, Concurrency

### 実装内容

**変更ファイル:**
1. `src/Spinor/Lsp/Docs.hs`
   - `DocEntry` 型に `docSlug :: Text` フィールドを追加
   - `primitiveDocs` の全43エントリに適切な slug を定義

2. `src/Spinor/DocGen.hs` (新規作成)
   - `generateDocs :: IO ()` — エントリポイント
   - `generateEntryFile` — 個別 Markdown ファイル生成
   - `renderEntry` — エントリを Markdown 形式に変換
   - `generateIndexFile` — インデックス生成
   - `renderIndex` — カテゴリ別インデックスを Markdown 形式に変換
   - `renderCategory` — カテゴリ内リストをリンク付きで生成

3. `app/Main.hs`
   - `Spinor.DocGen` のインポート追加
   - `helpMessage` に `docgen` コマンドの説明を追加
   - `main` 関数に `["docgen"] -> generateDocs` ケースを追加

4. `spinor.cabal`
   - `exposed-modules` に `Spinor.DocGen` を追加

**生成結果:**
- `docs/ref/` ディレクトリに43個の個別 Markdown ファイルを生成
- `docs/reference.md` にカテゴリ別インデックスを生成
- リンク形式は `doc.html?src=ref/<slug>.md` (既存の JS ローダー互換)
