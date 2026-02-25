# Spec 59: Error Reporting & Source Tracking

## 概要
AST (抽象構文木) の各ノードにソースコード上の位置情報 (ファイル名、行、列) を付与し、パーサーから型チェッカー、評価器まで一貫して位置情報を追跡する。これにより、エラー発生時に詳細なコンテキスト情報を開発者に提供する。

## データモデルの拡張

### 1. 位置情報の定義 (`src/Spinor/Syntax.hs`)
```haskell
data SourcePos = SourcePos
  { posFile   :: FilePath
  , posLine   :: Int
  , posColumn :: Int
  } deriving (Show, Eq)

data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Show, Eq)
```

### 2. AST ノードの拡張
`Expr` 型の各コンストラクタの第1引数に `SourceSpan` を追加する。
```haskell
data Expr
  = EInt  SourceSpan Integer
  | EBool SourceSpan Bool
  | ESym  SourceSpan Text
  | EStr  SourceSpan Text
  | EList SourceSpan [Expr]
  | ELet  SourceSpan [(Text, Expr)] Expr
  -- ... 他のコンストラクタも同様
```

## パーサーの改修 (`src/Spinor/Syntax.hs`)
Megaparsec の `getSourcePos` を利用して、各式をパースする直前と直後の位置を取得し、`SourceSpan` を構築して AST に埋め込む。

## エラー報告の構造化
`Either Text a` で返していたエラーを、位置情報を含む構造体に変更する。

```haskell
data SpinorError = SpinorError
  { errSpan    :: SourceSpan
  , errMessage :: Text
  } deriving (Show)
```

## Pretty Printing
エラーを以下の標準的な形式で出力するヘルパーを実装する。
`ファイル名:行:列: エラー内容`
例: `test.spin:10:5: 未定義のシンボル 'x' です。`

## 考慮事項
- **マクロ展開:** マクロ展開後のコードの位置情報をどう扱うか。初期実装では、展開元のマクロ呼び出しの位置を保持することを基本とする。
- **リファクタリングの規模:** `Expr` をパターンマッチしている箇所が全てコンパイラエラーになるため、機械的な修正が必要となる。
