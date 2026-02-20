# xref 静的解析機能の実装

## 概要

SLY xref (クロスリファレンス) の完全な実装。現在は RPC ハンドラのスタブのみで、実際の静的解析機能は未実装。

## 現状

- SLY からの RPC リクエストには応答できる
- `swank:xref :callers "func"` → 空リスト
- `swank:xref :callees "func"` → 空リスト
- 実際の呼び出し関係の解析は行われない

## xref の種類

| タイプ | 説明 |
|--------|------|
| `:callers` | この関数を呼び出している関数 |
| `:callees` | この関数が呼び出している関数 |
| `:calls` | `:callees` と同じ |
| `:references` | この変数を参照しているコード |
| `:binds` | この変数を束縛しているコード |
| `:sets` | この変数に代入しているコード |
| `:macroexpands` | このマクロを展開しているコード |

## 必要な作業

### 1. CallGraph データ構造の実装

```haskell
-- src/Spinor/Analysis.hs (新規)
module Spinor.Analysis where

import Data.Map.Strict (Map)
import Data.Text (Text)

data CallGraph = CallGraph
    { callers  :: Map Text [Text]  -- 関数名 -> 呼び出し元リスト
    , callees  :: Map Text [Text]  -- 関数名 -> 呼び出し先リスト
    , references :: Map Text [SourceLocation]  -- 変数名 -> 参照位置リスト
    }

data SourceLocation = SourceLocation
    { slFile   :: Text
    , slLine   :: Int
    , slColumn :: Int
    }
```

### 2. AST 解析機能の実装

```haskell
-- 関数定義を解析して呼び出し関係を抽出
analyzeDefinition :: Text -> Expr -> CallGraph -> CallGraph
analyzeDefinition funcName body graph = ...

-- AST を走査して関数呼び出しを収集
collectCalls :: Expr -> [Text]
collectCalls (EList (ESym name : _)) = [name]
collectCalls (EList exprs) = concatMap collectCalls exprs
collectCalls _ = []
```

### 3. Server への統合

```haskell
-- Server.hs
data ServerState = ServerState
    { ssEnv       :: Env
    , ssCallGraph :: CallGraph
    , ssTracedFns :: TracedFunctions
    }

-- define 時に CallGraph を更新
handleDefine :: Text -> Expr -> ServerState -> ServerState
```

### 4. xrefSearch の実装

```haskell
xrefSearch :: CallGraph -> Text -> Text -> Expr
xrefSearch graph xrefType name = case xrefType of
    ":callers" -> formatResults $ Map.lookup name (callers graph)
    ":callees" -> formatResults $ Map.lookup name (callees graph)
    ...
```

## 実装の課題

1. **ソース位置の追跡**: 現在のパーサーはソース位置を保持していない
2. **ファイル単位の管理**: 複数ファイルにまたがる解析
3. **インクリメンタル更新**: 再定義時の効率的な更新

## 参考資料

- `slynk-xref.lisp`: https://github.com/joaotavora/sly/blob/master/slynk/slynk-xref.lisp
- SLIME の xref 実装: https://github.com/slime/slime/blob/master/swank/swank-source-path-parser.lisp

## 優先度

低（基本的な SLY 統合は完了しており、本機能は発展的な機能）

## 関連ファイル

- `src/Spinor/Server.hs` - RPC ハンドラ（スタブ実装済み）
- `src/Spinor/Syntax.hs` - パーサー（ソース位置追跡の追加が必要）
- `src/Spinor/Eval.hs` - 評価器（CallGraph 更新の統合が必要）
