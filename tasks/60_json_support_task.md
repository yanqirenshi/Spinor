# Task 60: JSON Support の実装

`aeson` パッケージを利用して、JSON パースおよび文字列化機能を実装してください。

## ステップ

### 1. 依存関係の追加
- `spinor.cabal` の `library` セクションの `build-depends` に以下を追加してください。
    - `aeson`
    - `bytestring`
    - `scientific`

### 2. 変換ロジックの実装
- `src/Spinor/Library/Json.hs` を新規作成し、以下の関数を実装してください。
    - `spinorToAeson :: Val -> Either Text Aeson.Value`
    - `aesonToSpinor :: Aeson.Value -> Val`
- オブジェクトの変換では、キーを `VStr` とし、ペアを `VList [key, val]` とする Alist 形式を使用してください。

### 3. 組み込み関数の登録
- `src/Spinor/Primitive.hs` の `primitiveBindings` に以下を追加してください。
    - `"json-parse"`: `VStr` を受け取り JSON をパース。
    - `"json-stringify"`: `Val` を受け取り JSON 文字列を生成。

### 4. ドキュメントの追加
- `src/Spinor/Lsp/Docs.hs` に `json-parse` と `json-stringify` のドキュメントを追加してください。

### 5. ユニットテストの作成
- `test/Spinor/JsonSpec.hs` を作成し、各種データ型の変換テストを記述してください。

### 6. 動作確認
- `cabal test` が通ることを確認してください。
- REPL で `json-parse` と `json-stringify` が動作することを確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**JSON Object と Spinor List の変換:**
- JSON Object は Spinor の連想リスト (Alist) として表現
- Alist の形式: `(("key1" value1) ("key2" value2) ...)`
- Alist 判定: リストの全要素が `(VStr key, value)` の2要素リストであるか検出
- Alist → Object、通常リスト → Array として変換

**数値型の精度維持:**
- `aeson` の `Number` は `Scientific` 型で表現されている
- `Sci.floatingOrInteger` を使用して、整数として表現可能な場合は `VInt`、そうでなければ `VFloat` に変換
- これにより `42` → `VInt 42`、`3.14` → `VFloat 3.14` と適切に変換

**変換不可能な型のエラーハンドリング:**
- `VFunc`, `VMacro`, `VPrim`, `VMVar`, `VData`, `VMatrix`, `VCLContext`, `VCLBuffer`, `VCLKernel`, `VWindow` は JSON に変換不可
- 各型ごとに明確なエラーメッセージを返す

### 実装内容

**新規作成ファイル:**

| ファイル | 内容 |
|---------|------|
| `src/Spinor/Library/Json.hs` | JSON 変換ロジック |
| `test/Spinor/JsonSpec.hs` | ユニットテスト (35 テストケース) |

**変更ファイル:**

| ファイル | 変更内容 |
|---------|---------|
| `spinor.cabal` | `aeson`, `scientific` を build-depends に追加、`Spinor.Library.Json` と `Spinor.JsonSpec` を追加 |
| `src/Spinor/Primitive.hs` | `json-parse`, `json-stringify` プリミティブを登録 |
| `src/Spinor/Infer.hs` | `baseTypeEnv` に JSON 関数の型を追加 |
| `src/Spinor/Lsp/Docs.hs` | `json-parse`, `json-stringify` のドキュメントを追加 |

**新規定義関数 (Json.hs):**

```haskell
-- | Spinor の Val を Aeson の Value に変換
spinorToAeson :: Val -> Either Text Aeson.Value

-- | Aeson の Value を Spinor の Val に変換
aesonToSpinor :: Aeson.Value -> Val

-- | JSON 文字列をパースして Spinor の Val を返す
jsonParse :: Text -> Either Text Val

-- | Spinor の Val を JSON 文字列に変換
jsonStringify :: Val -> Either Text Text
```

**テスト結果:**
```
187 examples, 0 failures
```
(既存 152 テスト + 新規 35 テスト)

**REPL 動作確認:**
```lisp
spinor> (json-parse "{\"name\": \"Alice\", \"age\": 30}")
:: t1
(("age" 30) ("name" "Alice"))

spinor> (json-stringify '((\"name\" \"Alice\") (\"age\" 30)))
:: Str
"{\"age\":30,\"name\":\"Alice\"}"

spinor> (json-stringify '(1 2 3))
:: Str
"[1,2,3]"
```
