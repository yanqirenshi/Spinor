# Spec 60: JSON Support

## 概要
Spinor に JSON のパースおよび文字列化機能を追加する。
Haskell の `aeson` パッケージをバックエンドとして利用し、JSON データを Spinor のネイティブなデータ構造（リスト、文字列、数値等）にマッピングする。

## アーキテクチャ
- **依存ライブラリ**: `aeson`, `bytestring`, `scientific`
- **組み込み関数**: 
    - `(json-parse string)`: JSON 文字列を Spinor の値に変換。
    - `(json-stringify val)`: Spinor の値を JSON 文字列に変換。

## データマッピング規則

| JSON 型 | Spinor 型 | 変換例 |
| :--- | :--- | :--- |
| Number (整数) | `VInt` | `123` -> `123` |
| Number (浮動小数) | `VFloat` | `1.23` -> `1.23` |
| String | `VStr` | `"hello"` -> `"hello"` |
| Boolean | `VBool` | `true` -> `#t`, `false` -> `#f` |
| Null | `VNil` | `null` -> `nil` |
| Array | `VList` | `[1, 2]` -> `(1 2)` |
| Object | `VList` (Alist) | `{"a": 1}` -> `(("a" 1))` |

### Object の詳細 (Association List)
JSON オブジェクトは、キー（String）と値のペアを持つリストとして表現する。
例: `{"name": "Alice", "age": 30}` 
-> `(("name" "Alice") ("age" 30))`

## エラーハンドリング
JSON のパースに失敗した場合は、`json-parse` 関数内でエラーをキャッチし、Step 59 で導入した位置情報付きエラー (`SpinorError`) を送出する。
※パース関数に渡された文字列自体の位置情報をエラーに付与する。

## 考慮事項
- **循環参照**: `json-stringify` に循環参照を含むリストを渡した場合、Haskell 側でスタックオーバーフローや無限ループにならないよう留意する。
- **型の変換**: `VData` や `VFunc` などの JSON に対応しない Spinor の型を `json-stringify` しようとした場合はエラーとする。
