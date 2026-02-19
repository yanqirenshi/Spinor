# Step 38: LSP Advanced Features - 実装指示書

## 概要
LSP サーバーに Hover (ホバー表示) と Completion (入力補完) 機能を追加してください。
組み込み関数・特殊形式のドキュメントをエディタ上で参照できるようになります。

## Steps

### 1. ドキュメント辞書モジュールの作成

`src/Spinor/Lsp/Docs.hs` を新規作成してください。

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Spinor.Lsp.Docs
  ( DocEntry(..)
  , primitiveDocs
  , lookupDoc
  , allDocEntries
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Language.LSP.Protocol.Types (CompletionItemKind(..))

data DocEntry = DocEntry
  { docSignature   :: Text
  , docDescription :: Text
  , docKind        :: CompletionItemKind
  }

primitiveDocs :: Map Text DocEntry
primitiveDocs = Map.fromList
  -- 特殊形式
  [ ("def", DocEntry "(Symbol, Expr) -> Val" "変数を定義します。" CompletionItemKind_Keyword)
  , ("define", DocEntry "(Symbol, Expr) -> Val" "変数を定義します。(def のエイリアス)" CompletionItemKind_Keyword)
  , ("fn", DocEntry "(Params, Body) -> Function" "無名関数 (ラムダ) を作成します。" CompletionItemKind_Keyword)
  , ("mac", DocEntry "(Params, Body) -> Macro" "マクロを作成します。" CompletionItemKind_Keyword)
  , ("if", DocEntry "(Bool, Then, Else) -> Val" "条件分岐。条件が真なら Then を、偽なら Else を評価します。" CompletionItemKind_Keyword)
  , ("let", DocEntry "(Bindings, Body) -> Val" "ローカル変数を束縛して Body を評価します。" CompletionItemKind_Keyword)
  , ("match", DocEntry "(Expr, Branches...) -> Val" "パターンマッチを行います。" CompletionItemKind_Keyword)
  , ("quote", DocEntry "(Expr) -> Val" "式を評価せずに値として返します。" CompletionItemKind_Keyword)
  , ("begin", DocEntry "(Expr...) -> Val" "複数の式を順次評価し、最後の値を返します。" CompletionItemKind_Keyword)
  , ("progn", DocEntry "(Expr...) -> Val" "begin のエイリアス。" CompletionItemKind_Keyword)
  , ("setq", DocEntry "(Symbol, Expr) -> Val" "既存の変数に新しい値を代入します。" CompletionItemKind_Keyword)
  , ("data", DocEntry "(TypeName, Constructors...) -> ()" "代数的データ型 (ADT) を定義します。" CompletionItemKind_Keyword)
  , ("print", DocEntry "(a) -> a" "値を標準出力に表示し、その値を返します。" CompletionItemKind_Function)

  -- 算術演算
  , ("+", DocEntry "(Int, Int) -> Int" "2つの整数を加算します。" CompletionItemKind_Function)
  , ("-", DocEntry "(Int, Int) -> Int" "2つの整数を減算します。" CompletionItemKind_Function)
  , ("*", DocEntry "(Int, Int) -> Int" "2つの整数を乗算します。" CompletionItemKind_Function)
  , ("%", DocEntry "(Int, Int) -> Int" "整数の剰余を計算します。" CompletionItemKind_Function)

  -- 比較演算
  , ("=", DocEntry "(a, a) -> Bool" "2つの値が等しいかを判定します。" CompletionItemKind_Function)
  , ("<", DocEntry "(Int, Int) -> Bool" "左辺が右辺より小さいかを判定します。" CompletionItemKind_Function)
  , (">", DocEntry "(Int, Int) -> Bool" "左辺が右辺より大きいかを判定します。" CompletionItemKind_Function)

  -- リスト操作
  , ("cons", DocEntry "(a, [a]) -> [a]" "値をリストの先頭に追加します。" CompletionItemKind_Function)
  , ("car", DocEntry "([a]) -> a" "リストの先頭要素を返します。" CompletionItemKind_Function)
  , ("cdr", DocEntry "([a]) -> [a]" "リストの先頭以外を返します。" CompletionItemKind_Function)
  , ("list", DocEntry "(a...) -> [a]" "引数をリストにまとめます。" CompletionItemKind_Function)
  , ("null?", DocEntry "(a) -> Bool" "リストが空かどうかを判定します。" CompletionItemKind_Function)
  , ("empty?", DocEntry "(a) -> Bool" "null? のエイリアス。" CompletionItemKind_Function)
  , ("eq", DocEntry "(a, a) -> Bool" "2つの値が同一かを判定します (アトムのみ)。" CompletionItemKind_Function)
  , ("equal", DocEntry "(a, a) -> Bool" "2つの値が構造的に等しいかを判定します。" CompletionItemKind_Function)

  -- 文字列操作
  , ("string-append", DocEntry "(String...) -> String" "複数の文字列を連結します。" CompletionItemKind_Function)
  , ("string-length", DocEntry "(String) -> Int" "文字列の長さ (文字数) を返します。" CompletionItemKind_Function)
  , ("substring", DocEntry "(String, Int, Int) -> String" "部分文字列を取得します。(start, end) は 0-indexed。" CompletionItemKind_Function)
  , ("string=?", DocEntry "(String, String) -> Bool" "2つの文字列が等しいかを判定します。" CompletionItemKind_Function)
  , ("string->list", DocEntry "(String) -> [String]" "文字列を1文字ずつのリストに変換します。" CompletionItemKind_Function)
  , ("list->string", DocEntry "([String]) -> String" "文字列のリストを連結して1つの文字列にします。" CompletionItemKind_Function)

  -- I/O
  , ("read-file", DocEntry "(String) -> String" "ファイルを読み込み、内容を文字列として返します。" CompletionItemKind_Function)
  , ("write-file", DocEntry "(String, String) -> Bool" "ファイルに文字列を書き込みます (上書き)。" CompletionItemKind_Function)
  , ("append-file", DocEntry "(String, String) -> Bool" "ファイルに文字列を追記します。" CompletionItemKind_Function)
  , ("file-exists?", DocEntry "(String) -> Bool" "ファイルが存在するかを判定します。" CompletionItemKind_Function)

  -- 並行処理
  , ("spawn", DocEntry "(Expr) -> Bool" "新しいスレッドで式を評価します。" CompletionItemKind_Function)
  , ("sleep", DocEntry "(Int) -> Bool" "指定ミリ秒だけスレッドを停止します。" CompletionItemKind_Function)
  , ("new-mvar", DocEntry "() -> MVar" "新しい MVar を作成します。" CompletionItemKind_Function)
  , ("take-mvar", DocEntry "(MVar) -> Val" "MVar から値を取り出します (ブロッキング)。" CompletionItemKind_Function)
  , ("put-mvar", DocEntry "(MVar, Val) -> Bool" "MVar に値を格納します (ブロッキング)。" CompletionItemKind_Function)
  ]

lookupDoc :: Text -> Maybe DocEntry
lookupDoc = flip Map.lookup primitiveDocs

allDocEntries :: [(Text, DocEntry)]
allDocEntries = Map.toList primitiveDocs
```

### 2. Server.hs に Hover ハンドラを追加

`src/Spinor/Lsp/Server.hs` を更新してください。

#### 2.1. インポートの追加

```haskell
import Data.Text qualified as T
import Data.Char (isAlphaNum)
import Language.LSP.Protocol.Types qualified as J

import Spinor.Lsp.Docs (lookupDoc, DocEntry(..), allDocEntries)
```

#### 2.2. 単語抽出ヘルパー関数

```haskell
-- | Lisp シンボル構成文字かどうか
isSymbolChar :: Char -> Bool
isSymbolChar c = isAlphaNum c || c `elem` ("_+-*/%=<>?!" :: String)

-- | カーソル位置の単語を抽出
extractWordAt :: Text -> UInt -> Maybe Text
extractWordAt line col
  | col' >= T.length line = Nothing
  | not (isSymbolChar (T.index line col')) = Nothing
  | otherwise =
      let before = T.takeWhileEnd isSymbolChar (T.take (col' + 1) line)
          after  = T.takeWhile isSymbolChar (T.drop (col' + 1) line)
      in Just (before <> after)
  where
    col' = fromIntegral col
```

#### 2.3. Hover ハンドラ

```haskell
, requestHandler SMethod_TextDocumentHover $ \req responder -> do
    let uri = req ^. L.params . L.textDocument . L.uri
        pos = req ^. L.params . L.position
        line = pos ^. L.line
        col  = pos ^. L.character

    mVirtualFile <- getVirtualFile (toNormalizedUri uri)
    case mVirtualFile of
      Nothing -> responder $ Right $ InR Null
      Just vf -> do
        let content = virtualFileText vf
            lines = T.lines content
        if fromIntegral line >= length lines
          then responder $ Right $ InR Null
          else do
            let lineText = lines !! fromIntegral line
            case extractWordAt lineText col of
              Nothing -> responder $ Right $ InR Null
              Just word -> case lookupDoc word of
                Nothing -> responder $ Right $ InR Null
                Just doc -> do
                  let md = "```lisp\n(" <> word <> ") :: " <> docSignature doc <> "\n```\n\n" <> docDescription doc
                      hover = Hover
                        { _contents = InL $ MarkupContent MarkupKind_Markdown md
                        , _range = Nothing
                        }
                  responder $ Right $ InL hover
```

### 3. Server.hs に Completion ハンドラを追加

```haskell
, requestHandler SMethod_TextDocumentCompletion $ \_req responder -> do
    let items = map toCompletionItem allDocEntries
    responder $ Right $ InL $ InL items
  where
    toCompletionItem (name, doc) = CompletionItem
      { _label = name
      , _labelDetails = Nothing
      , _kind = Just (docKind doc)
      , _tags = Nothing
      , _detail = Just (docSignature doc)
      , _documentation = Just $ InR $ MarkupContent MarkupKind_Markdown (docDescription doc)
      , _deprecated = Nothing
      , _preselect = Nothing
      , _sortText = Nothing
      , _filterText = Nothing
      , _insertText = Nothing
      , _insertTextFormat = Nothing
      , _insertTextMode = Nothing
      , _textEdit = Nothing
      , _textEditText = Nothing
      , _additionalTextEdits = Nothing
      , _commitCharacters = Nothing
      , _command = Nothing
      , _data_ = Nothing
      }
```

### 4. Server Capabilities の更新

`serverDef` の `options` を更新してください。

```haskell
options = defaultOptions
  { optTextDocumentSync = Just syncOptions
  }
```

を以下に変更:

```haskell
options = defaultOptions
  { optTextDocumentSync = Just syncOptions
  , optCompletionTriggerCharacters = Just ['(', ' ']
  }
```

**注意:** lsp パッケージのバージョンによって API が異なる場合があります。`ServerCapabilities` を直接設定する方法も検討してください。

### 5. spinor.cabal の更新

`Spinor.Lsp.Docs` モジュールを追加してください。

```cabal
  exposed-modules:
    ...
    Spinor.Lsp.Docs
    Spinor.Lsp.Server
```

### 6. 検証手順

#### 6.1. ビルド確認
```bash
cabal build
```

#### 6.2. VS Code での動作確認 (spinor-lsp 拡張機能がある場合)
1. `.spin` ファイルを開く
2. `string-append` などの組み込み関数にカーソルを合わせ、ホバー表示を確認
3. `(` を入力した後、補完候補が表示されることを確認

#### 6.3. チェックリスト
- [ ] `cabal build` が成功する
- [ ] Hover: 組み込み関数にカーソルを合わせるとドキュメントが表示される
- [ ] Hover: 未知のシンボルでは何も表示されない
- [ ] Completion: `(` の後に補完候補が表示される
- [ ] Completion: 各候補にシグネチャと説明が含まれている

---

## 実装報告

### Implementation Policy (実装方針)
*(実装完了後、ここに記述してください)*

### Implementation Details (実装内容)
*(具体的な実装の工夫点、直面した問題の解決策などを記述してください)*
