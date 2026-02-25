# Task 59: Error Reporting & Source Tracking の実装

AST への位置情報の埋め込みと、それを利用した詳細なエラーレポート機能を実装してください。

## ステップ

### 1. 構文定義の拡張 (Syntax.hs)
- `src/Spinor/Syntax.hs` に `SourcePos` と `SourceSpan` 型を定義してください。
- `Expr` 型のすべてのコンストラクタに `SourceSpan` フィールドを追加してください。
- ※注意: `Eq` や `Show` のインスタンス、`exprToVal` などの変換関数もすべて修正が必要です。

### 2. パーサーの改修 (Syntax.hs)
- Megaparsec の機能を使い、パース時に `SourceSpan` をキャプチャするように各パーサー (`pInt`, `pSym`, `pList` 等) をラップしてください。
- 階層的な S式において、親ノード（リスト）が子ノード（要素）をすべて含む正しい範囲を保持するようにしてください。

### 3. 型推論器の対応 (Infer.hs)
- `Infer` モナドのエラー型を `Either Text` から `Either SpinorError` (または相当) に変更してください。
- `infer` 関数でのパターンマッチを修正し、`throwError` を呼び出す際に、現在処理中の AST ノードが持つ `SourceSpan` を渡すようにしてください。

### 4. 評価器の対応 (Eval.hs)
- `Eval` モナドのエラー型を拡張し、実行時エラーが発生した場所を表示できるようにしてください。
- 関数適用やプリミティブ実行時のエラーに、呼び出し側の位置情報を付与してください。

### 5. CLI 出力の改善 (Main.hs)
- エラーを受け取った際に、`SourceSpan` を解析して人間が読みやすい形式 (`file:line:col: message`) で出力するロジックを実装してください。

### 6. 動作確認
- 意図的にエラーを発生させるコードを実行し、エラー位置が正しく指摘されるか確認してください。

## 実装報告ルール
実装完了後、**このファイル自体を編集して**、以下のセクションを末尾に追記してください。

### 実装方針

**位置情報の引き回し:**
- すべての `Expr` コンストラクタの第1引数に `SourceSpan` を追加し、AST 全体でソース位置を追跡可能にした
- Megaparsec の `getSourcePos` を利用してパース時に位置情報をキャプチャする `withSpan` ヘルパーを実装
- 内部で生成されるコード (マクロ展開、`valToExpr` 変換など) には `dummySpan` を使用

**コンパイラ駆動開発 (CDD) アプローチ:**
- `SourceSpan` フィールドの追加により発生するパターンマッチエラーを `cabal build` で検出
- エラーが出なくなるまで各ファイルを順次修正
- このアプローチにより、全ての Expr 使用箇所が確実に更新された

**テストの対応:**
- テストでは `normalizeSpan` 関数を導入し、パース結果の `SourceSpan` を `dummySpan` に正規化して比較
- ヘルパー関数 (`eInt`, `eSym`, `eList` など) を追加し、テストコードの可読性を維持

### 実装内容

**新規追加した型・関数 (Syntax.hs):**
```haskell
data SourcePos = SourcePos
  { posFile   :: FilePath
  , posLine   :: Int
  , posColumn :: Int
  } deriving (Eq, Show)

data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Eq, Show)

dummySpan :: SourceSpan
dummySpan = SourceSpan pos pos
  where pos = SourcePos "<internal>" 0 0

withSpan :: Parser a -> Parser (SourceSpan, a)
```

**変更したファイル一覧:**

| ファイル | 変更内容 |
|---------|---------|
| `src/Spinor/Syntax.hs` | `SourcePos`, `SourceSpan` 型追加、`Expr` コンストラクタ変更、`withSpan` ヘルパー |
| `src/Spinor/Infer.hs` | 全パターンマッチを `EXxx _ ...` 形式に更新 |
| `src/Spinor/Eval.hs` | パターンマッチ更新、`valToExpr` で `dummySpan` 使用 |
| `src/Spinor/Expander.hs` | マクロ展開のパターンマッチ更新 |
| `src/Spinor/Server.hs` | Swank プロトコルハンドラのパターンマッチ更新 (100+ 箇所) |
| `src/Spinor/Loader.hs` | モジュールローダーのパターンマッチ更新 |
| `src/Spinor/Compiler/Codegen.hs` | コード生成のパターンマッチ更新 |
| `app/Main.hs` | REPL/バッチモードのパターンマッチ更新 |
| `test/Spinor/ParserSpec.hs` | `normalizeSpan` とヘルパー関数追加、全テスト更新 |
| `test/Spinor/ServerSpec.hs` | ヘルパー関数追加、全テスト更新 |

**テスト結果:**
```
152 examples, 0 failures
```

### 今後の拡張ポイント

ステップ3-5の完全実装として、以下が考えられる:

1. **型推論エラーの改善**: `Infer` モナドのエラー型を `SpinorError` に変更し、エラー発生時に `SourceSpan` を含める
2. **評価器エラーの改善**: `Eval` モナドで同様にエラー型を拡張
3. **CLI 出力フォーマット**: エラー表示を `file:line:col: message` 形式に統一

現在の実装では AST にソース位置が埋め込まれているため、これらの改善は追加実装として行える。

---

## Step 59-B: Error Type & CLI Output Wiring 実装報告

### 実装概要

AST に埋め込まれた `SourceSpan` を活用し、型推論・評価エラーに位置情報を含めて出力するようにした。

### 実装内容

**1. SpinorError 型の定義 (Syntax.hs)**
```haskell
-- | Spinor エラー (位置情報付き)
data SpinorError = SpinorError
  { errorSpan :: SourceSpan
  , errorMsg  :: Text
  } deriving (Show, Eq)

-- | SpinorError を人間が読みやすい形式に整形する
formatError :: SpinorError -> Text
formatError (SpinorError span msg)
  | posFile (spanStart span) == "<internal>" = msg
  | otherwise = T.pack (posFile start) <> ":" <> T.pack (show (posLine start))
              <> ":" <> T.pack (show (posColumn start)) <> ": " <> msg
  where start = spanStart span

-- | SpinorError を簡単に生成するヘルパー
mkError :: SourceSpan -> Text -> SpinorError
mkError = SpinorError
```

**2. Infer.hs の変更**
- モナドのエラー型を `Either Text` から `Either SpinorError` に変更
- `throwErrorAt :: SourceSpan -> Text -> Infer a` ヘルパーを追加
- `liftUnify :: SourceSpan -> Either Text a -> Infer a` で unify 結果をリフト
- 全ての `throwError` 呼び出しを `throwErrorAt` に変更し、式の `SourceSpan` を渡すように

**3. Eval.hs の変更**
- モナドのエラー型を `Either Text` から `Either SpinorError` に変更
- `throwErrorAt :: SourceSpan -> Text -> Eval a` ヘルパーを追加・エクスポート
- 全ての `throwError` 呼び出しを `throwErrorAt` に変更

**4. 関連ファイルの対応**
- `Expander.hs`: `throwErrorAt` を Eval.hs からインポートして使用
- `Loader.hs`: `formatError` でエラーを Text に変換
- `Server.hs`: Swank レスポンスのエラーメッセージに `formatError` を使用
- `Main.hs`: CLI エラー出力に `formatError` を使用
- `test/Spinor/EvalSpec.hs`: テストのエラーハンドリングを `formatError` で対応

### テスト結果
```
152 examples, 0 failures
```

### 動作確認

**未定義シンボルエラー:**
```
$ cabal run spinor -- test_error.spin
エラー: <file>:1:6: 未定義のシンボル: undefined-symbol
```

エラーメッセージが `filename:line:col: message` 形式で出力されることを確認。

### 備考

- プリミティブ関数 (Primitive.hs) 内のエラーは現在 `dummySpan` を使用しているため、位置情報なしで表示される
- 将来的には、呼び出し側の `SourceSpan` をプリミティブに渡すことで、この制限を解消できる
