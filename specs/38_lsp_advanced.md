# Step 38: LSP Advanced Features (Hover & Completion) - 技術仕様

## 1. 概要
Step 37 で構築した LSP サーバー (`spinor lsp`) に、組み込み関数のホバー表示と入力補完機能を追加する。これによりエディタ上でのコーディング体験を大幅に向上させる。

## 2. Hover 機能 (`textDocument/hover`)

### 2.1. 機能概要
ユーザーがエディタ上でシンボル（単語）にカーソルを合わせると、そのシンボルが Spinor の組み込み関数・特殊形式であれば、シグネチャと説明を Markdown 形式で表示する。

### 2.2. リクエスト/レスポンス

**リクエスト:**
```json
{
  "method": "textDocument/hover",
  "params": {
    "textDocument": { "uri": "file:///path/to/file.spin" },
    "position": { "line": 5, "character": 10 }
  }
}
```

**レスポンス (成功時):**
```json
{
  "contents": {
    "kind": "markdown",
    "value": "```lisp\n(+ a b) :: Int -> Int -> Int\n```\n\n2つの整数を加算します。"
  },
  "range": { "start": { "line": 5, "character": 8 }, "end": { "line": 5, "character": 9 } }
}
```

**レスポンス (対象外シンボル):**
```json
null
```

### 2.3. 単語抽出アルゴリズム

Hover リクエストには座標 (line, character) のみが送られるため、VFS からファイル内容を取得し、カーソル位置の単語を抽出する必要がある。

**Lisp シンボル構成文字:**
```
[a-zA-Z0-9_+\-*/=<>?!]
```

**アルゴリズム:**
1. VFS から対象ファイルの内容を取得
2. 指定行のテキストを取得
3. カーソル位置から左右に走査し、シンボル構成文字の連続を抽出
4. 抽出した単語を返す

```haskell
extractWordAt :: Text -> UInt -> Maybe Text
extractWordAt line col = ...
```

## 3. Completion 機能 (`textDocument/completion`)

### 3.1. 機能概要
ユーザーが文字を入力している際、組み込み関数・特殊形式の名前を補完候補として提供する。

### 3.2. リクエスト/レスポンス

**リクエスト:**
```json
{
  "method": "textDocument/completion",
  "params": {
    "textDocument": { "uri": "file:///path/to/file.spin" },
    "position": { "line": 5, "character": 3 }
  }
}
```

**レスポンス:**
```json
{
  "isIncomplete": false,
  "items": [
    {
      "label": "string-append",
      "kind": 3,
      "detail": "(String...) -> String",
      "documentation": {
        "kind": "markdown",
        "value": "複数の文字列を連結します。"
      }
    },
    ...
  ]
}
```

### 3.3. CompletionItem 構造

| フィールド | 値 |
|-----------|-----|
| `label` | 関数名 (例: `string-append`) |
| `kind` | `CompletionItemKind_Function` (3) または `CompletionItemKind_Keyword` (14) |
| `detail` | シグネチャ (例: `(String...) -> String`) |
| `documentation` | Markdown 形式の説明 |

## 4. ドキュメント辞書

### 4.1. データ構造

```haskell
data DocEntry = DocEntry
  { docSignature   :: Text  -- シグネチャ (例: "(a, b) -> Int")
  , docDescription :: Text  -- 説明文
  , docKind        :: CompletionItemKind  -- Function or Keyword
  }

primitiveDocs :: Map Text DocEntry
```

### 4.2. 登録対象

#### 特殊形式 (Special Forms)
| 名前 | シグネチャ | 説明 |
|------|-----------|------|
| `def` / `define` | `(Symbol, Expr) -> Val` | 変数を定義する |
| `fn` | `(Params, Body) -> Function` | 無名関数を作成する |
| `mac` | `(Params, Body) -> Macro` | マクロを作成する |
| `if` | `(Bool, Expr, Expr) -> Val` | 条件分岐 |
| `let` | `(Bindings, Body) -> Val` | ローカル変数を束縛する |
| `match` | `(Expr, Branches...) -> Val` | パターンマッチ |
| `quote` | `(Expr) -> Val` | 式を評価せずに値として返す |
| `begin` / `progn` | `(Expr...) -> Val` | 順次評価 |
| `setq` | `(Symbol, Expr) -> Val` | 変数に代入する |
| `data` | `(TypeName, Constructors...) -> ()` | ADT を定義する |

#### プリミティブ関数 (Primitives)
| 名前 | シグネチャ | 説明 |
|------|-----------|------|
| `+` | `(Int, Int) -> Int` | 加算 |
| `-` | `(Int, Int) -> Int` | 減算 |
| `*` | `(Int, Int) -> Int` | 乗算 |
| `%` | `(Int, Int) -> Int` | 剰余 |
| `=` | `(a, a) -> Bool` | 等値比較 |
| `<` | `(Int, Int) -> Bool` | 小なり比較 |
| `>` | `(Int, Int) -> Bool` | 大なり比較 |
| `cons` | `(a, [a]) -> [a]` | リストの先頭に追加 |
| `car` | `([a]) -> a` | リストの先頭要素 |
| `cdr` | `([a]) -> [a]` | リストの残り |
| `list` | `(a...) -> [a]` | 引数をリストにまとめる |
| `null?` | `(a) -> Bool` | 空リスト判定 |
| `eq` | `(a, a) -> Bool` | 同一性比較 |
| `equal` | `(a, a) -> Bool` | 構造的等価性比較 |

#### 文字列操作
| 名前 | シグネチャ | 説明 |
|------|-----------|------|
| `string-append` | `(String...) -> String` | 文字列を連結 |
| `string-length` | `(String) -> Int` | 文字列の長さ |
| `substring` | `(String, Int, Int) -> String` | 部分文字列 |
| `string=?` | `(String, String) -> Bool` | 文字列比較 |
| `string->list` | `(String) -> [String]` | 文字列をリストに変換 |
| `list->string` | `([String]) -> String` | リストを文字列に変換 |

#### I/O & システム
| 名前 | シグネチャ | 説明 |
|------|-----------|------|
| `print` | `(a) -> a` | 値を表示して返す |
| `read-file` | `(String) -> String` | ファイルを読み込む |
| `write-file` | `(String, String) -> Bool` | ファイルに書き込む |
| `spawn` | `(Expr) -> Bool` | 新しいスレッドで評価 |
| `sleep` | `(Int) -> Bool` | 指定ミリ秒スリープ |

## 5. モジュール構成

```
src/Spinor/Lsp/
├── Server.hs      -- メインサーバー (既存)
└── Docs.hs        -- ドキュメント辞書 (新規)
```

### 5.1. Docs.hs の公開 API

```haskell
module Spinor.Lsp.Docs
  ( DocEntry(..)
  , primitiveDocs
  , lookupDoc
  , allCompletionItems
  ) where
```

## 6. Server Capabilities の更新

`serverDef` の `options` を更新し、Hover と Completion をサポートすることを宣言する。

```haskell
options = defaultOptions
  { optTextDocumentSync = Just syncOptions
  , optHoverProvider = Just True  -- 追加
  , optCompletionProvider = Just completionOptions  -- 追加
  }

completionOptions :: CompletionOptions
completionOptions = CompletionOptions
  { _triggerCharacters = Just ["(", " "]
  , _resolveProvider = Just False
  , ...
  }
```

## 7. エラーハンドリング

| 状況 | 対応 |
|------|------|
| VFS にファイルがない | `Nothing` (Hover) / 空リスト (Completion) を返す |
| カーソル位置に単語がない | `Nothing` を返す |
| 辞書に単語がない | `Nothing` を返す |

## 8. パフォーマンス考慮

- ドキュメント辞書は起動時に構築し、メモリに保持
- Completion は全候補を返す（フィルタリングはクライアント側）
- 将来的にはプレフィックスフィルタリングを検討
