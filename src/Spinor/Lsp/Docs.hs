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
  , docSlug        :: Text  -- ファイル名として安全な識別子
  }

primitiveDocs :: Map Text DocEntry
primitiveDocs = Map.fromList
  -- 特殊形式
  [ ("def", DocEntry "(Symbol, Expr) -> Val" "変数を定義します。" CompletionItemKind_Keyword "def")
  , ("define", DocEntry "(Symbol, Expr) -> Val" "変数を定義します。(def のエイリアス)" CompletionItemKind_Keyword "define")
  , ("fn", DocEntry "(Params, Body) -> Function" "無名関数 (ラムダ) を作成します。" CompletionItemKind_Keyword "fn")
  , ("mac", DocEntry "(Params, Body) -> Macro" "マクロを作成します。" CompletionItemKind_Keyword "mac")
  , ("if", DocEntry "(Bool, Then, Else) -> Val" "条件分岐。条件が真なら Then を、偽なら Else を評価します。" CompletionItemKind_Keyword "if")
  , ("let", DocEntry "(Bindings, Body) -> Val" "ローカル変数を束縛して Body を評価します。" CompletionItemKind_Keyword "let")
  , ("match", DocEntry "(Expr, Branches...) -> Val" "パターンマッチを行います。" CompletionItemKind_Keyword "match")
  , ("quote", DocEntry "(Expr) -> Val" "式を評価せずに値として返します。" CompletionItemKind_Keyword "quote")
  , ("begin", DocEntry "(Expr...) -> Val" "複数の式を順次評価し、最後の値を返します。" CompletionItemKind_Keyword "begin")
  , ("progn", DocEntry "(Expr...) -> Val" "begin のエイリアス。" CompletionItemKind_Keyword "progn")
  , ("setq", DocEntry "(Symbol, Expr) -> Val" "既存の変数に新しい値を代入します。" CompletionItemKind_Keyword "setq")
  , ("data", DocEntry "(TypeName, Constructors...) -> ()" "代数的データ型 (ADT) を定義します。" CompletionItemKind_Keyword "data")
  , ("print", DocEntry "(a) -> a" "値を標準出力に表示し、その値を返します。" CompletionItemKind_Function "print")

  -- 算術演算
  , ("+", DocEntry "(Int, Int) -> Int" "2つの整数を加算します。" CompletionItemKind_Function "add")
  , ("-", DocEntry "(Int, Int) -> Int" "2つの整数を減算します。" CompletionItemKind_Function "sub")
  , ("*", DocEntry "(Int, Int) -> Int" "2つの整数を乗算します。" CompletionItemKind_Function "mul")
  , ("%", DocEntry "(Int, Int) -> Int" "整数の剰余を計算します。" CompletionItemKind_Function "mod")

  -- 比較演算
  , ("=", DocEntry "(a, a) -> Bool" "2つの値が等しいかを判定します。" CompletionItemKind_Function "eq-op")
  , ("<", DocEntry "(Int, Int) -> Bool" "左辺が右辺より小さいかを判定します。" CompletionItemKind_Function "lt")
  , (">", DocEntry "(Int, Int) -> Bool" "左辺が右辺より大きいかを判定します。" CompletionItemKind_Function "gt")

  -- リスト操作
  , ("cons", DocEntry "(a, [a]) -> [a]" "値をリストの先頭に追加します。" CompletionItemKind_Function "cons")
  , ("car", DocEntry "([a]) -> a" "リストの先頭要素を返します。" CompletionItemKind_Function "car")
  , ("cdr", DocEntry "([a]) -> [a]" "リストの先頭以外を返します。" CompletionItemKind_Function "cdr")
  , ("list", DocEntry "(a...) -> [a]" "引数をリストにまとめます。" CompletionItemKind_Function "list")
  , ("null?", DocEntry "(a) -> Bool" "リストが空かどうかを判定します。" CompletionItemKind_Function "null-p")
  , ("empty?", DocEntry "(a) -> Bool" "null? のエイリアス。" CompletionItemKind_Function "empty-p")
  , ("eq", DocEntry "(a, a) -> Bool" "2つの値が同一かを判定します (アトムのみ)。" CompletionItemKind_Function "eq")
  , ("equal", DocEntry "(a, a) -> Bool" "2つの値が構造的に等しいかを判定します。" CompletionItemKind_Function "equal")

  -- 文字列操作
  , ("string-append", DocEntry "(String...) -> String" "複数の文字列を連結します。" CompletionItemKind_Function "string-append")
  , ("string-length", DocEntry "(String) -> Int" "文字列の長さ (文字数) を返します。" CompletionItemKind_Function "string-length")
  , ("substring", DocEntry "(String, Int, Int) -> String" "部分文字列を取得します。(start, end) は 0-indexed。" CompletionItemKind_Function "substring")
  , ("string=?", DocEntry "(String, String) -> Bool" "2つの文字列が等しいかを判定します。" CompletionItemKind_Function "string-eq")
  , ("string->list", DocEntry "(String) -> [String]" "文字列を1文字ずつのリストに変換します。" CompletionItemKind_Function "string-to-list")
  , ("list->string", DocEntry "([String]) -> String" "文字列のリストを連結して1つの文字列にします。" CompletionItemKind_Function "list-to-string")

  -- I/O
  , ("read-file", DocEntry "(String) -> String" "ファイルを読み込み、内容を文字列として返します。" CompletionItemKind_Function "read-file")
  , ("write-file", DocEntry "(String, String) -> Bool" "ファイルに文字列を書き込みます (上書き)。" CompletionItemKind_Function "write-file")
  , ("append-file", DocEntry "(String, String) -> Bool" "ファイルに文字列を追記します。" CompletionItemKind_Function "append-file")
  , ("file-exists?", DocEntry "(String) -> Bool" "ファイルが存在するかを判定します。" CompletionItemKind_Function "file-exists-p")

  -- 並行処理
  , ("spawn", DocEntry "(Expr) -> Bool" "新しいスレッドで式を評価します。" CompletionItemKind_Function "spawn")
  , ("sleep", DocEntry "(Int) -> Bool" "指定ミリ秒だけスレッドを停止します。" CompletionItemKind_Function "sleep")
  , ("new-mvar", DocEntry "() -> MVar" "新しい MVar を作成します。" CompletionItemKind_Function "new-mvar")
  , ("take-mvar", DocEntry "(MVar) -> Val" "MVar から値を取り出します (ブロッキング)。" CompletionItemKind_Function "take-mvar")
  , ("put-mvar", DocEntry "(MVar, Val) -> Bool" "MVar に値を格納します (ブロッキング)。" CompletionItemKind_Function "put-mvar")
  ]

lookupDoc :: Text -> Maybe DocEntry
lookupDoc = flip Map.lookup primitiveDocs

allDocEntries :: [(Text, DocEntry)]
allDocEntries = Map.toList primitiveDocs
