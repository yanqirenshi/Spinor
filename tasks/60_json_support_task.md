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
(JSON Object と Spinor List の変換で工夫した点や、数値型の精度維持について)

### 実装内容
(変更したファイルの一覧、新しく定義した関数の説明など)
