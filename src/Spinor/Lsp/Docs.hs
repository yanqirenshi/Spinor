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
  { docSignature            :: Text
  , docDescription          :: Text
  , docKind                 :: CompletionItemKind
  , docSlug                 :: Text
  , docSyntax               :: Text
  , docArgumentsAndValues   :: Text
  , docExamples             :: Text
  , docSideEffects          :: Text
  , docAffectedBy           :: Text
  , docExceptionalSituations :: Text
  , docSeeAlso              :: [Text]
  , docNotes                :: Text
  }

-- | ヘルパー: デフォルト値を持つ DocEntry を作成
mkDoc :: Text -> Text -> CompletionItemKind -> Text -> Text -> Text -> Text -> [Text] -> DocEntry
mkDoc sig desc kind slug syntax argsVals examples seeAlso = DocEntry
  { docSignature            = sig
  , docDescription          = desc
  , docKind                 = kind
  , docSlug                 = slug
  , docSyntax               = syntax
  , docArgumentsAndValues   = argsVals
  , docExamples             = examples
  , docSideEffects          = "None."
  , docAffectedBy           = "None."
  , docExceptionalSituations = "None."
  , docSeeAlso              = seeAlso
  , docNotes                = ""
  }

-- | ヘルパー: 最小限の DocEntry (TBD)
mkDocTBD :: Text -> Text -> CompletionItemKind -> Text -> DocEntry
mkDocTBD sig desc kind slug = DocEntry
  { docSignature            = sig
  , docDescription          = desc
  , docKind                 = kind
  , docSlug                 = slug
  , docSyntax               = "TBD"
  , docArgumentsAndValues   = "TBD"
  , docExamples             = "TBD"
  , docSideEffects          = "None."
  , docAffectedBy           = "None."
  , docExceptionalSituations = "None."
  , docSeeAlso              = []
  , docNotes                = ""
  }

primitiveDocs :: Map Text DocEntry
primitiveDocs = Map.fromList
  -- ========================================
  -- 特殊形式 (Special Forms)
  -- ========================================
  [ ("def", DocEntry
      { docSignature = "(Symbol, Expr) -> Val"
      , docDescription = "トップレベルに変数を定義します。シンボル `name` に式 `expr` の評価結果を束縛します。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "def"
      , docSyntax = "(def name expr)"
      , docArgumentsAndValues = unlines
          [ "- `name` -- 定義する変数名 (シンボル)"
          , "- `expr` -- 束縛する値を生成する式"
          , "- 戻り値: 束縛された値"
          ]
      , docExamples = unlines
          [ "```lisp"
          , ";; 数値の定義"
          , "(def x 42)"
          , "x  ; => 42"
          , ""
          , ";; 関数の定義"
          , "(def square (fn (n) (* n n)))"
          , "(square 5)  ; => 25"
          , "```"
          ]
      , docSideEffects = "グローバル環境に新しい束縛を追加します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["fn", "let", "setq"]
      , docNotes = "`define` は `def` のエイリアスです。Scheme スタイルの構文を好む場合に使用できます。"
      })

  , ("define", mkDoc
      "(Symbol, Expr) -> Val"
      "変数を定義します。`def` のエイリアス。"
      CompletionItemKind_Keyword
      "define"
      "(define name expr)"
      "- `name` -- 変数名\n- `expr` -- 束縛する式\n- 戻り値: 束縛された値"
      "```lisp\n(define pi 3.14159)\n```"
      ["def"])

  , ("fn", DocEntry
      { docSignature = "(Params, Body) -> Function"
      , docDescription = "無名関数 (ラムダ式) を作成します。`params` は仮引数リスト、`body` は関数本体です。クロージャとして定義時の環境を捕捉します。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "fn"
      , docSyntax = "(fn (param1 param2 ...) body)"
      , docArgumentsAndValues = unlines
          [ "- `params` -- 仮引数のリスト (シンボルのリスト)"
          , "- `body` -- 関数本体の式"
          , "- 戻り値: 関数オブジェクト"
          ]
      , docExamples = unlines
          [ "```lisp"
          , ";; 基本的なラムダ"
          , "((fn (x) (* x 2)) 5)  ; => 10"
          , ""
          , ";; 複数引数"
          , "(def add (fn (a b) (+ a b)))"
          , "(add 3 4)  ; => 7"
          , ""
          , ";; クロージャ"
          , "(def make-adder (fn (n) (fn (x) (+ x n))))"
          , "(def add10 (make-adder 10))"
          , "(add10 5)  ; => 15"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "引数の数が一致しない場合、実行時エラーが発生します。"
      , docSeeAlso = ["def", "mac", "let"]
      , docNotes = "Spinor の関数は正格評価 (call-by-value) です。"
      })

  , ("mac", mkDoc
      "(Params, Body) -> Macro"
      "マクロを作成します。引数は評価されずにそのまま渡され、返り値が評価されます。"
      CompletionItemKind_Keyword
      "mac"
      "(mac (param1 param2 ...) body)"
      "- `params` -- 仮引数のリスト\n- `body` -- マクロ本体\n- 戻り値: マクロオブジェクト"
      "```lisp\n(def when (mac (cond body)\n  (list 'if cond body nil)))\n(when (> 5 3) (print \"yes\"))  ; => \"yes\"\n```"
      ["fn", "quote"])

  , ("if", DocEntry
      { docSignature = "(Bool, Then, Else) -> Val"
      , docDescription = "条件分岐を行います。`condition` が真 (nil 以外) なら `then-expr` を、偽 (nil) なら `else-expr` を評価します。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "if"
      , docSyntax = "(if condition then-expr else-expr)"
      , docArgumentsAndValues = unlines
          [ "- `condition` -- 条件式"
          , "- `then-expr` -- 条件が真の場合に評価される式"
          , "- `else-expr` -- 条件が偽の場合に評価される式"
          , "- 戻り値: 評価された分岐の結果"
          ]
      , docExamples = unlines
          [ "```lisp"
          , "(if (> 5 3) \"yes\" \"no\")  ; => \"yes\""
          , ""
          , "(def abs (fn (n)"
          , "  (if (< n 0) (- 0 n) n)))"
          , "(abs -5)  ; => 5"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["match", "begin"]
      , docNotes = "Spinor では `nil` のみが偽として扱われます。`0` や空リストは真です。"
      })

  , ("let", DocEntry
      { docSignature = "(Bindings, Body) -> Val"
      , docDescription = "ローカル変数を導入し、その環境で `body` を評価します。束縛は逐次的に行われます (let*)。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "let"
      , docSyntax = "(let ((var1 val1) (var2 val2) ...) body)"
      , docArgumentsAndValues = unlines
          [ "- `bindings` -- `(変数名 値)` のペアのリスト"
          , "- `body` -- 束縛のスコープ内で評価される式"
          , "- 戻り値: body の評価結果"
          ]
      , docExamples = unlines
          [ "```lisp"
          , "(let ((x 10)"
          , "      (y 20))"
          , "  (+ x y))  ; => 30"
          , ""
          , ";; 逐次束縛 (前の変数を参照可能)"
          , "(let ((x 5)"
          , "      (y (* x 2)))"
          , "  y)  ; => 10"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["def", "fn"]
      , docNotes = "Spinor の `let` は Common Lisp の `let*` と同じ意味論です。"
      })

  , ("match", mkDoc
      "(Expr, Branches...) -> Val"
      "パターンマッチを行います。式の値に最初にマッチするパターンの本体を評価します。"
      CompletionItemKind_Keyword
      "match"
      "(match expr (pattern1 body1) (pattern2 body2) ...)"
      "- `expr` -- マッチ対象の式\n- `patterns` -- パターンと本体のペア\n- 戻り値: マッチした分岐の本体の評価結果"
      "```lisp\n(data Maybe (Just val) Nothing)\n\n(def safe-head (fn (lst)\n  (match lst\n    ((cons x _) (Just x))\n    (nil Nothing))))\n```"
      ["if", "data"])

  , ("quote", mkDoc
      "(Expr) -> Val"
      "式を評価せずにそのままデータとして返します。"
      CompletionItemKind_Keyword
      "quote"
      "(quote expr)  ; または 'expr"
      "- `expr` -- クオートする式\n- 戻り値: 評価されていない式そのもの"
      "```lisp\n(quote (+ 1 2))  ; => (+ 1 2)\n'(a b c)         ; => (a b c)\n```"
      ["list", "mac"])

  , ("begin", mkDoc
      "(Expr...) -> Val"
      "複数の式を順次評価し、最後の式の値を返します。"
      CompletionItemKind_Keyword
      "begin"
      "(begin expr1 expr2 ... exprN)"
      "- `exprs` -- 評価する式のシーケンス\n- 戻り値: 最後の式の評価結果"
      "```lisp\n(begin\n  (print \"first\")\n  (print \"second\")\n  42)  ; => 42 (\"first\" \"second\" が出力される)\n```"
      ["progn", "let"])

  , ("progn", mkDoc
      "(Expr...) -> Val"
      "`begin` のエイリアス。Common Lisp スタイル。"
      CompletionItemKind_Keyword
      "progn"
      "(progn expr1 expr2 ... exprN)"
      "- `exprs` -- 評価する式のシーケンス\n- 戻り値: 最後の式の評価結果"
      "```lisp\n(progn (setq x 1) (setq x (+ x 1)) x)  ; => 2\n```"
      ["begin"])

  , ("setq", mkDoc
      "(Symbol, Expr) -> Val"
      "既存の変数に新しい値を代入します。変数が存在しない場合はエラーになります。"
      CompletionItemKind_Keyword
      "setq"
      "(setq name expr)"
      "- `name` -- 代入先の変数名\n- `expr` -- 新しい値\n- 戻り値: 代入された値"
      "```lisp\n(def counter 0)\n(setq counter (+ counter 1))\ncounter  ; => 1\n```"
      ["def"])

  , ("data", mkDoc
      "(TypeName, Constructors...) -> ()"
      "代数的データ型 (ADT) を定義します。"
      CompletionItemKind_Keyword
      "data"
      "(data TypeName (Ctor1 field1 ...) (Ctor2 field2 ...) ...)"
      "- `TypeName` -- 型の名前\n- `Ctors` -- コンストラクタ定義のリスト\n- 戻り値: ()"
      "```lisp\n(data Maybe (Just val) Nothing)\n(data Tree (Leaf val) (Node left right))\n```"
      ["match"])

  , ("print", DocEntry
      { docSignature = "(a) -> a"
      , docDescription = "値を標準出力に表示し、その値をそのまま返します。デバッグに便利です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "print"
      , docSyntax = "(print expr)"
      , docArgumentsAndValues = "- `expr` -- 表示する値\n- 戻り値: 入力と同じ値"
      , docExamples = "```lisp\n(print \"Hello, World!\")  ; 出力: Hello, World!\n(+ 1 (print 2))          ; 出力: 2, 戻り値: 3\n```"
      , docSideEffects = "標準出力にテキストを出力します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["write-file"]
      , docNotes = ""
      })

  -- ========================================
  -- 算術演算 (Arithmetic)
  -- ========================================
  , ("+", DocEntry
      { docSignature = "(Int, Int) -> Int"
      , docDescription = "2つの整数を加算し、その和を返します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "add"
      , docSyntax = "(+ a b)"
      , docArgumentsAndValues = unlines
          [ "- `a` -- 第一オペランド (整数)"
          , "- `b` -- 第二オペランド (整数)"
          , "- 戻り値: `a + b` の結果 (整数)"
          ]
      , docExamples = unlines
          [ "```lisp"
          , "(+ 1 2)      ; => 3"
          , "(+ 10 -3)    ; => 7"
          , "(+ 0 0)      ; => 0"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "引数が整数でない場合、型エラーが発生します。"
      , docSeeAlso = ["sub", "mul", "mod"]
      , docNotes = ""
      })

  , ("-", mkDoc
      "(Int, Int) -> Int"
      "2つの整数を減算します。"
      CompletionItemKind_Function
      "sub"
      "(- a b)"
      "- `a` -- 被減数\n- `b` -- 減数\n- 戻り値: `a - b`"
      "```lisp\n(- 10 3)  ; => 7\n(- 5 8)   ; => -3\n```"
      ["add", "mul"])

  , ("*", mkDoc
      "(Int, Int) -> Int"
      "2つの整数を乗算します。"
      CompletionItemKind_Function
      "mul"
      "(* a b)"
      "- `a` -- 第一オペランド\n- `b` -- 第二オペランド\n- 戻り値: `a * b`"
      "```lisp\n(* 3 4)   ; => 12\n(* 7 0)   ; => 0\n```"
      ["add", "sub"])

  , ("%", mkDoc
      "(Int, Int) -> Int"
      "整数の剰余を計算します。"
      CompletionItemKind_Function
      "mod"
      "(% a b)"
      "- `a` -- 被除数\n- `b` -- 除数\n- 戻り値: `a mod b`"
      "```lisp\n(% 10 3)  ; => 1\n(% 15 5)  ; => 0\n```"
      ["add", "sub", "mul"])

  -- ========================================
  -- 比較演算 (Comparison)
  -- ========================================
  , ("=", mkDoc
      "(a, a) -> Bool"
      "2つの値が等しいかを判定します。"
      CompletionItemKind_Function
      "eq-op"
      "(= a b)"
      "- `a`, `b` -- 比較する値\n- 戻り値: 等しければ `t`、そうでなければ `nil`"
      "```lisp\n(= 1 1)      ; => t\n(= \"a\" \"b\")  ; => nil\n```"
      ["lt", "gt", "eq", "equal"])

  , ("<", mkDoc
      "(Int, Int) -> Bool"
      "左辺が右辺より小さいかを判定します。"
      CompletionItemKind_Function
      "lt"
      "(< a b)"
      "- `a`, `b` -- 比較する整数\n- 戻り値: `a < b` なら `t`"
      "```lisp\n(< 1 2)  ; => t\n(< 5 3)  ; => nil\n```"
      ["gt", "eq-op"])

  , (">", mkDoc
      "(Int, Int) -> Bool"
      "左辺が右辺より大きいかを判定します。"
      CompletionItemKind_Function
      "gt"
      "(> a b)"
      "- `a`, `b` -- 比較する整数\n- 戻り値: `a > b` なら `t`"
      "```lisp\n(> 5 3)  ; => t\n(> 1 2)  ; => nil\n```"
      ["lt", "eq-op"])

  -- ========================================
  -- リスト操作 (List Operations)
  -- ========================================
  , ("cons", DocEntry
      { docSignature = "(a, [a]) -> [a]"
      , docDescription = "値をリストの先頭に追加し、新しいリストを返します。Lisp の基本的なリスト構築関数です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "cons"
      , docSyntax = "(cons x lst)"
      , docArgumentsAndValues = unlines
          [ "- `x` -- リストの先頭に追加する値"
          , "- `lst` -- 既存のリスト"
          , "- 戻り値: `x` を先頭に持つ新しいリスト"
          ]
      , docExamples = unlines
          [ "```lisp"
          , "(cons 1 '(2 3))     ; => (1 2 3)"
          , "(cons 'a nil)       ; => (a)"
          , "(cons 1 (cons 2 (cons 3 nil)))  ; => (1 2 3)"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["car", "cdr", "list"]
      , docNotes = "`cons` は \"construct\" の略です。"
      })

  , ("car", mkDoc
      "([a]) -> a"
      "リストの先頭要素 (head) を返します。"
      CompletionItemKind_Function
      "car"
      "(car lst)"
      "- `lst` -- 対象のリスト\n- 戻り値: リストの最初の要素"
      "```lisp\n(car '(1 2 3))  ; => 1\n(car '(a))      ; => a\n```"
      ["cdr", "cons"])

  , ("cdr", mkDoc
      "([a]) -> [a]"
      "リストの先頭以外 (tail) を返します。"
      CompletionItemKind_Function
      "cdr"
      "(cdr lst)"
      "- `lst` -- 対象のリスト\n- 戻り値: 先頭を除いたリスト"
      "```lisp\n(cdr '(1 2 3))  ; => (2 3)\n(cdr '(a))      ; => nil\n```"
      ["car", "cons"])

  , ("list", mkDoc
      "(a...) -> [a]"
      "引数をリストにまとめます。"
      CompletionItemKind_Function
      "list"
      "(list elem1 elem2 ...)"
      "- `elems` -- リストの要素\n- 戻り値: 引数を含むリスト"
      "```lisp\n(list 1 2 3)      ; => (1 2 3)\n(list 'a 'b)      ; => (a b)\n(list)            ; => nil\n```"
      ["cons", "quote"])

  , ("null?", mkDoc
      "(a) -> Bool"
      "引数が空リスト (nil) かどうかを判定します。"
      CompletionItemKind_Function
      "null-p"
      "(null? x)"
      "- `x` -- 判定する値\n- 戻り値: `nil` なら `t`、それ以外は `nil`"
      "```lisp\n(null? nil)       ; => t\n(null? '())       ; => t\n(null? '(1 2))    ; => nil\n```"
      ["empty-p", "cons"])

  , ("empty?", mkDoc
      "(a) -> Bool"
      "`null?` のエイリアス。"
      CompletionItemKind_Function
      "empty-p"
      "(empty? x)"
      "- `x` -- 判定する値\n- 戻り値: 空なら `t`"
      "```lisp\n(empty? nil)  ; => t\n```"
      ["null-p"])

  , ("eq", mkDoc
      "(a, a) -> Bool"
      "2つの値がポインタレベルで同一かを判定します (アトムのみ)。"
      CompletionItemKind_Function
      "eq"
      "(eq a b)"
      "- `a`, `b` -- 比較する値\n- 戻り値: 同一なら `t`"
      "```lisp\n(eq 'a 'a)    ; => t\n(eq 1 1)      ; => t\n```"
      ["equal", "eq-op"])

  , ("equal", mkDoc
      "(a, a) -> Bool"
      "2つの値が構造的に等しいかを判定します。リストも再帰的に比較します。"
      CompletionItemKind_Function
      "equal"
      "(equal a b)"
      "- `a`, `b` -- 比較する値\n- 戻り値: 構造的に等しければ `t`"
      "```lisp\n(equal '(1 2) '(1 2))  ; => t\n(equal '(1 2) '(1 3))  ; => nil\n```"
      ["eq", "eq-op"])

  -- ========================================
  -- 文字列操作 (String Operations)
  -- ========================================
  , ("string-append", mkDocTBD "(String...) -> String" "複数の文字列を連結します。" CompletionItemKind_Function "string-append")
  , ("string-length", mkDocTBD "(String) -> Int" "文字列の長さ (文字数) を返します。" CompletionItemKind_Function "string-length")
  , ("substring", mkDocTBD "(String, Int, Int) -> String" "部分文字列を取得します。(start, end) は 0-indexed。" CompletionItemKind_Function "substring")
  , ("string=?", mkDocTBD "(String, String) -> Bool" "2つの文字列が等しいかを判定します。" CompletionItemKind_Function "string-eq")
  , ("string->list", mkDocTBD "(String) -> [String]" "文字列を1文字ずつのリストに変換します。" CompletionItemKind_Function "string-to-list")
  , ("list->string", mkDocTBD "([String]) -> String" "文字列のリストを連結して1つの文字列にします。" CompletionItemKind_Function "list-to-string")

  -- ========================================
  -- I/O
  -- ========================================
  , ("read-file", mkDocTBD "(String) -> String" "ファイルを読み込み、内容を文字列として返します。" CompletionItemKind_Function "read-file")
  , ("write-file", mkDocTBD "(String, String) -> Bool" "ファイルに文字列を書き込みます (上書き)。" CompletionItemKind_Function "write-file")
  , ("append-file", mkDocTBD "(String, String) -> Bool" "ファイルに文字列を追記します。" CompletionItemKind_Function "append-file")
  , ("file-exists?", mkDocTBD "(String) -> Bool" "ファイルが存在するかを判定します。" CompletionItemKind_Function "file-exists-p")

  -- ========================================
  -- 並行処理 (Concurrency)
  -- ========================================
  , ("spawn", mkDocTBD "(Expr) -> Bool" "新しいスレッドで式を評価します。" CompletionItemKind_Function "spawn")
  , ("sleep", mkDocTBD "(Int) -> Bool" "指定ミリ秒だけスレッドを停止します。" CompletionItemKind_Function "sleep")
  , ("new-mvar", mkDocTBD "() -> MVar" "新しい MVar を作成します。" CompletionItemKind_Function "new-mvar")
  , ("take-mvar", mkDocTBD "(MVar) -> Val" "MVar から値を取り出します (ブロッキング)。" CompletionItemKind_Function "take-mvar")
  , ("put-mvar", mkDocTBD "(MVar, Val) -> Bool" "MVar に値を格納します (ブロッキング)。" CompletionItemKind_Function "put-mvar")
  ]

lookupDoc :: Text -> Maybe DocEntry
lookupDoc = flip Map.lookup primitiveDocs

allDocEntries :: [(Text, DocEntry)]
allDocEntries = Map.toList primitiveDocs
