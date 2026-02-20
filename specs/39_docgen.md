# Spec 39: Reference Generator (docgen)

## 概要
Spinor の組み込み関数や特殊形式の定義データ (`src/Spinor/Lsp/Docs.hs`) から、ユーザー向けの Markdown リファレンスを自動生成する。
これにより、ソースコード内のドキュメント、LSP、および静的ドキュメントの同期を保証する。

## アーキテクチャ
- **Source of Truth:** `src/Spinor/Lsp/Docs.hs` の `primitiveDocs` マップ。
- **CLI Command:** `spinor docgen`
- **Output:**
    - 個別詳細ファイル: `docs/ref/<slug>.md`
    - 総合インデックス: `docs/reference.md` (既存ファイルを置換)

## データモデルの変更
`DocEntry` 型に `docSlug` フィールドを追加する。
```haskell
data DocEntry = DocEntry
  { docSignature   :: Text
  , docDescription :: Text
  , docKind        :: CompletionItemKind
  , docSlug        :: Text  -- 追加: ファイル名として安全な識別子
  }
```

### Slug の命名規則
記号を含む関数名については、以下のルールに従って英単語の slug を割り当てる：
- `+` -> `add`
- `-` -> `sub`
- `*` -> `mul`
- `/` -> `div`
- `%` -> `mod`
- `=` -> `eq`
- `<` -> `lt`
- `>` -> `gt`
- `null?` -> `null-p`
- `string=?` -> `string-eq`
- `file-exists?` -> `file-exists-p`
- その他、記号は `-` に置換するか、意味の通じる英単語にする。

## 出力形式

### 1. 個別ファイル (`docs/ref/<slug>.md`)
```markdown
# <関数名>

**Kind:** <Function | Special Form | Keyword>
**Signature:** `<シグネチャ>`

<説明文>
```

### 2. 総合インデックス (`docs/reference.md`)
- カテゴリ別（またはアルファベット順）のリスト。
- 各項目は個別ファイルへのリンクを持つ。
- リンク形式: `[name](doc.html?src=ref/<slug>.md)`
  - ※既存の `docs/index.html` の JS ローダーが `src` パラメータを解釈することを前提とする。

## 考慮事項
- `docs/ref/` ディレクトリが存在しない場合は自動生成する。
- 生成前に `docs/ref/` 内の古いファイルを削除するか、上書きする。
