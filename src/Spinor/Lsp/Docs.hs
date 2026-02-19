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
