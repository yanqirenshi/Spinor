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
import qualified Data.Text as T
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
      , docArgumentsAndValues = T.unlines
          [ "- `name` -- 定義する変数名 (シンボル)"
          , "- `expr` -- 束縛する値を生成する式"
          , "- 戻り値: 束縛された値"
          ]
      , docExamples = T.unlines
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
      , docArgumentsAndValues = T.unlines
          [ "- `params` -- 仮引数のリスト (シンボルのリスト)"
          , "- `body` -- 関数本体の式"
          , "- 戻り値: 関数オブジェクト"
          ]
      , docExamples = T.unlines
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
      , docArgumentsAndValues = T.unlines
          [ "- `condition` -- 条件式"
          , "- `then-expr` -- 条件が真の場合に評価される式"
          , "- `else-expr` -- 条件が偽の場合に評価される式"
          , "- 戻り値: 評価された分岐の結果"
          ]
      , docExamples = T.unlines
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
      , docArgumentsAndValues = T.unlines
          [ "- `bindings` -- `(変数名 値)` のペアのリスト"
          , "- `body` -- 束縛のスコープ内で評価される式"
          , "- 戻り値: body の評価結果"
          ]
      , docExamples = T.unlines
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

  -- ========================================
  -- パッケージ操作 (Package Operations)
  -- ========================================
  , ("defpackage", DocEntry
      { docSignature = "(String, Options...) -> String"
      , docDescription = "新しいパッケージを定義します。パッケージは名前空間を提供し、シンボルの衝突を防ぎます。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "defpackage"
      , docSyntax = "(defpackage \"pkg-name\" (:use \"base-pkg\") (:export \"sym1\" \"sym2\"))"
      , docArgumentsAndValues = T.unlines
          [ "- `pkg-name` -- 定義するパッケージ名 (文字列)"
          , "- `:use` -- (オプション) インポートするパッケージのリスト"
          , "- `:export` -- (オプション) 外部に公開するシンボル名のリスト"
          , "- 戻り値: 作成されたパッケージ名"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; シンプルなパッケージ定義"
          , "(defpackage \"mylib\")"
          , ""
          , ";; エクスポートを指定"
          , "(defpackage \"math-utils\" (:export \"square\" \"cube\"))"
          , ""
          , ";; 他のパッケージを use"
          , "(defpackage \"my-app\" (:use \"math-utils\") (:export \"main\"))"
          , "```"
          ]
      , docSideEffects = "新しいパッケージをパッケージレジストリに登録します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "パッケージ名が文字列でない場合、エラーを返します。"
      , docSeeAlso = ["in-package", "use-package", "export", "current-package"]
      , docNotes = "すべてのパッケージは暗黙的に \"spinor\" パッケージを use します (コアプリミティブへのアクセス)。"
      })

  , ("in-package", DocEntry
      { docSignature = "(String) -> String"
      , docDescription = "現在の評価コンテキストを指定したパッケージに切り替えます。以降の `def` はこのパッケージに定義されます。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "in-package"
      , docSyntax = "(in-package \"pkg-name\")"
      , docArgumentsAndValues = T.unlines
          [ "- `pkg-name` -- 切り替え先のパッケージ名 (文字列)"
          , "- 戻り値: 切り替え先のパッケージ名"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(defpackage \"mylib\")"
          , "(in-package \"mylib\")"
          , "(def x 42)  ; mylib パッケージに定義"
          , ""
          , "(in-package \"user\")"
          , "(def y 100)  ; user パッケージに定義"
          , "```"
          ]
      , docSideEffects = "現在のパッケージコンテキストを変更します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "指定されたパッケージが存在しない場合、エラーを返します。"
      , docSeeAlso = ["defpackage", "current-package", "use-package"]
      , docNotes = "REPL での実験的な開発に便利です。"
      })

  , ("use-package", DocEntry
      { docSignature = "(String) -> Bool"
      , docDescription = "指定したパッケージの公開シンボルを現在のパッケージから参照可能にします。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "use-package"
      , docSyntax = "(use-package \"pkg-name\")"
      , docArgumentsAndValues = T.unlines
          [ "- `pkg-name` -- インポートするパッケージ名 (文字列)"
          , "- 戻り値: `#t`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; math-lib の公開シンボルを使えるようにする"
          , "(defpackage \"math-lib\" (:export \"square\"))"
          , "(in-package \"math-lib\")"
          , "(def square (fn (x) (* x x)))"
          , "(export \"square\")"
          , ""
          , "(defpackage \"my-app\")"
          , "(in-package \"my-app\")"
          , "(use-package \"math-lib\")"
          , "(square 5)  ; => 25"
          , "```"
          ]
      , docSideEffects = "現在のパッケージの使用パッケージリストを更新します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "指定されたパッケージが存在しない場合、エラーを返します。"
      , docSeeAlso = ["defpackage", "in-package", "export"]
      , docNotes = "公開 (export) されたシンボルのみがインポートされます。"
      })

  , ("current-package", DocEntry
      { docSignature = "() -> String"
      , docDescription = "現在のパッケージ名を返します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "current-package"
      , docSyntax = "(current-package)"
      , docArgumentsAndValues = T.unlines
          [ "- 引数なし"
          , "- 戻り値: 現在のパッケージ名 (文字列)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(current-package)  ; => \"user\" (デフォルト)"
          , ""
          , "(defpackage \"mylib\")"
          , "(in-package \"mylib\")"
          , "(current-package)  ; => \"mylib\""
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "`in-package` の呼び出しに影響されます。"
      , docExceptionalSituations = "None."
      , docSeeAlso = ["in-package", "defpackage"]
      , docNotes = "初期状態では \"user\" パッケージが現在のパッケージです。"
      })

  , ("export", DocEntry
      { docSignature = "(String...) -> Bool"
      , docDescription = "現在のパッケージから指定したシンボルを公開 (エクスポート) します。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "export"
      , docSyntax = "(export \"sym1\" \"sym2\" ...)"
      , docArgumentsAndValues = T.unlines
          [ "- `syms` -- 公開するシンボル名 (文字列、可変長)"
          , "- 戻り値: `#t`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(defpackage \"mylib\")"
          , "(in-package \"mylib\")"
          , "(def public-fn (fn (x) (* x 2)))"
          , "(def private-fn (fn (x) (+ x 1)))"
          , "(export \"public-fn\")  ; public-fn のみ公開"
          , ""
          , ";; 別パッケージから"
          , "(defpackage \"app\")"
          , "(in-package \"app\")"
          , "(use-package \"mylib\")"
          , "(public-fn 5)   ; => 10 (アクセス可)"
          , ";; private-fn は見えない"
          , "```"
          ]
      , docSideEffects = "現在のパッケージのエクスポートリストを更新します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "シンボル名が文字列でない場合、エラーを返します。"
      , docSeeAlso = ["defpackage", "use-package", "in-package"]
      , docNotes = "`defpackage` の `:export` オプションでも定義時にエクスポートを指定できます。"
      })

  -- ========================================
  -- エラーハンドリング (Error Handling)
  -- ========================================
  , ("ignore-errors", DocEntry
      { docSignature = "(Expr...) -> Val | nil"
      , docDescription = "本体の評価中にエラーが発生した場合、そのエラーを無視して `nil` を返します。エラーが発生しなければ最後の式の結果を返します。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "ignore-errors"
      , docSyntax = "(ignore-errors body...)"
      , docArgumentsAndValues = T.unlines
          [ "- `body` -- 評価する式 (0個以上)"
          , "- 戻り値: 成功時は最後の式の値、エラー時は `nil`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; ゼロ除算エラーを無視"
          , "(ignore-errors (/ 1 0))  ; => nil"
          , ""
          , ";; 正常時は値を返す"
          , "(ignore-errors (+ 1 2))  ; => 3"
          , ""
          , ";; 複数の式"
          , "(ignore-errors"
          , "  (print \"start\")"
          , "  (error \"fail\")"
          , "  (print \"end\"))  ; => nil (\"start\" のみ出力)"
          , "```"
          ]
      , docSideEffects = "本体の副作用はエラー発生前まで実行されます。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "None. (全てのエラーを捕捉します)"
      , docSeeAlso = ["handler-case", "unwind-protect"]
      , docNotes = "Common Lisp の `ignore-errors` に相当します。詳細なエラー処理が必要な場合は `handler-case` を使用してください。"
      })

  , ("handler-case", DocEntry
      { docSignature = "(Expr, Handler) -> Val"
      , docDescription = "式を評価し、エラーが発生した場合は指定されたハンドラを実行します。エラーメッセージは指定した変数に束縛されます。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "handler-case"
      , docSyntax = "(handler-case expression (error (var) handler-body...))"
      , docArgumentsAndValues = T.unlines
          [ "- `expression` -- 評価する式"
          , "- `error` -- エラーハンドラのキーワード (現在は全エラーを捕捉)"
          , "- `var` -- エラーメッセージが束縛される変数名"
          , "- `handler-body` -- エラー時に評価される式"
          , "- 戻り値: 成功時は `expression` の値、エラー時はハンドラの結果"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; エラーメッセージを取得"
          , "(handler-case"
          , "  (error \"something went wrong\")"
          , "  (error (msg)"
          , "    (string-append \"Caught: \" msg)))"
          , "; => \"Caught: something went wrong\""
          , ""
          , ";; 正常時はそのまま値を返す"
          , "(handler-case"
          , "  (+ 1 2)"
          , "  (error (e) \"error occurred\"))"
          , "; => 3"
          , ""
          , ";; デフォルト値を返す"
          , "(handler-case"
          , "  (/ 10 0)"
          , "  (error (_) 0))  ; => 0"
          , "```"
          ]
      , docSideEffects = "ハンドラ内の副作用が実行される可能性があります。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "ハンドラ形式が不正な場合、エラーを返します。"
      , docSeeAlso = ["ignore-errors", "unwind-protect"]
      , docNotes = "Common Lisp の `handler-case` の簡易版です。現時点では全てのエラーを `error` キーワードで捕捉します。将来的にエラー型による分岐をサポート予定です。"
      })

  , ("unwind-protect", DocEntry
      { docSignature = "(Expr, Expr...) -> Val"
      , docDescription = "保護フォームを評価し、その成否に関わらずクリーンアップフォームを必ず実行します。ファイル入出力やリソース解放に必須の構文です。"
      , docKind = CompletionItemKind_Keyword
      , docSlug = "unwind-protect"
      , docSyntax = "(unwind-protect protected-form cleanup-form...)"
      , docArgumentsAndValues = T.unlines
          [ "- `protected-form` -- 保護される式 (1つ)"
          , "- `cleanup-form` -- 必ず実行されるクリーンアップ式 (0個以上)"
          , "- 戻り値: `protected-form` の結果 (エラー時は再送出)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 必ずクリーンアップが実行される"
          , "(def cleaned nil)"
          , "(ignore-errors"
          , "  (unwind-protect"
          , "    (error \"fail\")"
          , "    (setq cleaned t)))"
          , "cleaned  ; => t"
          , ""
          , ";; ファイル操作の典型例"
          , "(unwind-protect"
          , "  (begin"
          , "    (def data (read-file \"input.txt\"))"
          , "    (process data))"
          , "  (print \"cleanup completed\"))"
          , ""
          , ";; 正常終了時も実行"
          , "(unwind-protect"
          , "  (+ 1 2)"
          , "  (print \"done\"))  ; \"done\" が出力され、3 を返す"
          , "```"
          ]
      , docSideEffects = "クリーンアップフォームの副作用が必ず実行されます。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "クリーンアップ中のエラーは無視されます。保護フォームでエラーが発生した場合、クリーンアップ後にエラーが再送出されます。"
      , docSeeAlso = ["ignore-errors", "handler-case"]
      , docNotes = "Common Lisp の `unwind-protect` と同様の動作をします。リソースリークを防ぐために、ファイルハンドルやロックの解放には必ずこの構文を使用してください。"
      })

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
      , docArgumentsAndValues = T.unlines
          [ "- `a` -- 第一オペランド (整数)"
          , "- `b` -- 第二オペランド (整数)"
          , "- 戻り値: `a + b` の結果 (整数)"
          ]
      , docExamples = T.unlines
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
      , docArgumentsAndValues = T.unlines
          [ "- `x` -- リストの先頭に追加する値"
          , "- `lst` -- 既存のリスト"
          , "- 戻り値: `x` を先頭に持つ新しいリスト"
          ]
      , docExamples = T.unlines
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

  -- ========================================
  -- 行列操作 (Matrix Operations)
  -- ========================================
  , ("matrix", DocEntry
      { docSignature = "(Int, Int, [Number]) -> Matrix"
      , docDescription = "行列を生成します。行数・列数と要素のフラットリストから行優先 (row-major) で構築します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "matrix"
      , docSyntax = "(matrix rows cols elements)"
      , docArgumentsAndValues = T.unlines
          [ "- `rows` -- 行数 (正の整数)"
          , "- `cols` -- 列数 (正の整数)"
          , "- `elements` -- 要素のリスト (`Int` または `Float`)"
          , "- 戻り値: `rows × cols` の行列"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 2x3 行列を生成"
          , "(matrix 2 3 '(1 2 3 4 5 6))"
          , "; => #m((1.0 2.0 3.0) (4.0 5.0 6.0))"
          , ""
          , ";; 単位行列"
          , "(matrix 2 2 '(1 0 0 1))"
          , "; => #m((1.0 0.0) (0.0 1.0))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- `rows * cols` と要素数が一致しない場合、エラーを返します。"
          , "- 要素に数値以外が含まれる場合、エラーを返します。"
          ]
      , docSeeAlso = ["mdim", "mref"]
      , docNotes = "内部的には `Data.Vector.Storable` を使用し、将来的な BLAS/LAPACK 連携を想定しています。"
      })

  , ("mdim", DocEntry
      { docSignature = "(Matrix) -> (Int Int)"
      , docDescription = "行列の次元 (行数, 列数) をリストとして返します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "mdim"
      , docSyntax = "(mdim m)"
      , docArgumentsAndValues = T.unlines
          [ "- `m` -- 行列"
          , "- 戻り値: `(rows cols)` の形式のリスト"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def m (matrix 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12)))"
          , "(mdim m)  ; => (3 4)"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "引数が行列でない場合、エラーを返します。"
      , docSeeAlso = ["matrix", "mref"]
      , docNotes = ""
      })

  , ("mref", DocEntry
      { docSignature = "(Matrix, Int, Int) -> Float"
      , docDescription = "行列の指定位置の要素を取得します。インデックスは 0-indexed です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "mref"
      , docSyntax = "(mref m row col)"
      , docArgumentsAndValues = T.unlines
          [ "- `m` -- 行列"
          , "- `row` -- 行インデックス (0-indexed)"
          , "- `col` -- 列インデックス (0-indexed)"
          , "- 戻り値: 指定位置の要素 (浮動小数点数)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def m (matrix 2 3 '(1 2 3 4 5 6)))"
          , "(mref m 0 0)  ; => 1.0  ; 左上"
          , "(mref m 0 2)  ; => 3.0  ; 1行目の最後"
          , "(mref m 1 1)  ; => 5.0  ; 2行目の中央"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "インデックスが範囲外の場合、エラーを返します。"
      , docSeeAlso = ["matrix", "mdim"]
      , docNotes = ""
      })

  -- ========================================
  -- BLAS/LAPACK 行列演算 (Matrix Arithmetic)
  -- ========================================
  , ("m+", DocEntry
      { docSignature = "(Matrix, Matrix) -> Matrix"
      , docDescription = "2つの行列の要素ごとの加算を行います。BLAS を利用した高速演算です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "m-add"
      , docSyntax = "(m+ a b)"
      , docArgumentsAndValues = T.unlines
          [ "- `a` -- 第一行列"
          , "- `b` -- 第二行列"
          , "- 戻り値: 要素ごとの和を持つ新しい行列"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def a (matrix 2 2 '(1 2 3 4)))"
          , "(def b (matrix 2 2 '(5 6 7 8)))"
          , "(m+ a b)  ; => #m((6.0 8.0) (10.0 12.0))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 引数が行列でない場合、エラーを返します。"
          , "- 2つの行列の次元が一致しない場合、エラーを返します。"
          ]
      , docSeeAlso = ["m*", "matrix"]
      , docNotes = "内部的に hmatrix (BLAS) を使用しています。"
      })

  , ("m*", DocEntry
      { docSignature = "(Matrix, Matrix) -> Matrix"
      , docDescription = "行列積を計算します。BLAS を利用した高速演算です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "m-mul"
      , docSyntax = "(m* a b)"
      , docArgumentsAndValues = T.unlines
          [ "- `a` -- 左行列 (m×k)"
          , "- `b` -- 右行列 (k×n)"
          , "- 戻り値: 行列積 (m×n)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def a (matrix 2 3 '(1 2 3 4 5 6)))"
          , "(def b (matrix 3 2 '(7 8 9 10 11 12)))"
          , "(m* a b)  ; => #m((58.0 64.0) (139.0 154.0))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 引数が行列でない場合、エラーを返します。"
          , "- 左行列の列数と右行列の行数が一致しない場合、エラーを返します。"
          ]
      , docSeeAlso = ["m+", "transpose", "inverse"]
      , docNotes = "内部的に hmatrix (BLAS/dgemm) を使用しています。"
      })

  , ("transpose", DocEntry
      { docSignature = "(Matrix) -> Matrix"
      , docDescription = "行列の転置を返します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "transpose"
      , docSyntax = "(transpose m)"
      , docArgumentsAndValues = T.unlines
          [ "- `m` -- 行列 (m×n)"
          , "- 戻り値: 転置行列 (n×m)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def m (matrix 2 3 '(1 2 3 4 5 6)))"
          , "(transpose m)  ; => #m((1.0 4.0) (2.0 5.0) (3.0 6.0))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = "引数が行列でない場合、エラーを返します。"
      , docSeeAlso = ["m*", "inverse", "matrix"]
      , docNotes = ""
      })

  , ("inverse", DocEntry
      { docSignature = "(Matrix) -> Matrix"
      , docDescription = "正方行列の逆行列を計算します。LAPACK を利用した高速演算です。"
      , docKind = CompletionItemKind_Function
      , docSlug = "inverse"
      , docSyntax = "(inverse m)"
      , docArgumentsAndValues = T.unlines
          [ "- `m` -- 正方行列 (n×n)"
          , "- 戻り値: 逆行列 (n×n)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 単位行列の逆行列は単位行列"
          , "(inverse (matrix 2 2 '(1 0 0 1)))  ; => #m((1.0 0.0) (0.0 1.0))"
          , ""
          , "(inverse (matrix 2 2 '(1 2 3 4)))"
          , "; => #m((-2.0 1.0) (1.5 -0.5))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 引数が行列でない場合、エラーを返します。"
          , "- 行列が正方でない場合、エラーを返します。"
          , "- 行列が特異 (singular) な場合、エラーを返します。"
          ]
      , docSeeAlso = ["m*", "transpose", "matrix"]
      , docNotes = "内部的に hmatrix (LAPACK) を使用しています。"
      })

  -- ========================================
  -- OpenCL / GPGPU
  -- ========================================
  , ("cl-init", DocEntry
      { docSignature = "() -> CLContext"
      , docDescription = "OpenCL プラットフォームとデバイスをスキャンし、コンテキストとコマンドキューを初期化します。GPU を優先し、なければ CPU にフォールバックします。"
      , docKind = CompletionItemKind_Function
      , docSlug = "cl-init"
      , docSyntax = "(cl-init)"
      , docArgumentsAndValues = T.unlines
          [ "- 引数なし"
          , "- 戻り値: OpenCL コンテキスト (`CLContext`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def ctx (cl-init))"
          , "ctx  ; => <CLContext>"
          , "```"
          ]
      , docSideEffects = "OpenCL プラットフォームの初期化を行います。"
      , docAffectedBy = "システムにインストールされた OpenCL ドライバとデバイスに依存します。"
      , docExceptionalSituations = T.unlines
          [ "- OpenCL プラットフォームが見つからない場合、エラーを返します。"
          , "- 利用可能なデバイス (GPU/CPU) が見つからない場合、エラーを返します。"
          ]
      , docSeeAlso = ["to-device", "to-host", "cl-compile"]
      , docNotes = "Haskell の OpenCL パッケージ (`Control.Parallel.OpenCL`) を使用しています。"
      })

  , ("to-device", DocEntry
      { docSignature = "(CLContext, Matrix) -> CLBuffer"
      , docDescription = "行列データを GPU 側のバッファに転送します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "to-device"
      , docSyntax = "(to-device ctx matrix)"
      , docArgumentsAndValues = T.unlines
          [ "- `ctx` -- OpenCL コンテキスト"
          , "- `matrix` -- CPU 側の行列データ (`VMatrix`)"
          , "- 戻り値: GPU バッファ (`CLBuffer`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def ctx (cl-init))"
          , "(def m (matrix 2 2 '(1 2 3 4)))"
          , "(def buf (to-device ctx m))  ; => <CLBuffer:size=4>"
          , "```"
          ]
      , docSideEffects = "GPU メモリを確保し、データを転送します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 第1引数が CLContext でない場合、エラーを返します。"
          , "- 第2引数が Matrix でない場合、エラーを返します。"
          ]
      , docSeeAlso = ["to-host", "cl-init", "matrix"]
      , docNotes = ""
      })

  , ("to-host", DocEntry
      { docSignature = "(CLContext, CLBuffer, Int, Int) -> Matrix"
      , docDescription = "GPU バッファのデータを CPU に読み戻し、指定された次元の行列を生成します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "to-host"
      , docSyntax = "(to-host ctx buffer rows cols)"
      , docArgumentsAndValues = T.unlines
          [ "- `ctx` -- OpenCL コンテキスト"
          , "- `buffer` -- GPU バッファ (`CLBuffer`)"
          , "- `rows` -- 行数"
          , "- `cols` -- 列数"
          , "- 戻り値: CPU 側の行列 (`Matrix`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def ctx (cl-init))"
          , "(def m (matrix 2 2 '(1 2 3 4)))"
          , "(def buf (to-device ctx m))"
          , "(to-host ctx buf 2 2)  ; => #m((1.0 2.0) (3.0 4.0))"
          , "```"
          ]
      , docSideEffects = "GPU からデータを読み戻します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- バッファサイズと指定された次元 (`rows * cols`) が不一致の場合、エラーを返します。"
          , "- 第1引数が CLContext でない場合、エラーを返します。"
          , "- 第2引数が CLBuffer でない場合、エラーを返します。"
          ]
      , docSeeAlso = ["to-device", "cl-init", "matrix"]
      , docNotes = ""
      })

  , ("cl-compile", DocEntry
      { docSignature = "(CLContext, String, String) -> CLKernel"
      , docDescription = "OpenCL C のソースコードをコンパイルし、指定されたカーネル関数へのハンドルを取得します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "cl-compile"
      , docSyntax = "(cl-compile ctx source kernel-name)"
      , docArgumentsAndValues = T.unlines
          [ "- `ctx` -- OpenCL コンテキスト"
          , "- `source` -- OpenCL C ソースコード (文字列)"
          , "- `kernel-name` -- エントリポイントとなるカーネル関数名 (文字列)"
          , "- 戻り値: コンパイル済みカーネル (`CLKernel`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def ctx (cl-init))"
          , "(def k (cl-compile ctx"
          , "  \"__kernel void add(__global double* a, __global double* b, __global double* c, int n) { int i = get_global_id(0); if (i < n) c[i] = a[i] + b[i]; }\""
          , "  \"add\"))"
          , "k  ; => <CLKernel:add>"
          , "```"
          ]
      , docSideEffects = "OpenCL プログラムのコンパイルを行います。"
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- ソースコードのコンパイルに失敗した場合、ビルドログ付きのエラーを返します。"
          , "- 指定されたカーネル名が見つからない場合、エラーを返します。"
          , "- 第1引数が CLContext でない場合、エラーを返します。"
          ]
      , docSeeAlso = ["cl-init", "to-device", "to-host", "cl-enqueue"]
      , docNotes = "コンパイルエラー時はビルドログが含まれます。"
      })

  , ("cl-enqueue", DocEntry
      { docSignature = "(CLContext, CLKernel, [Int], [Int], Args...) -> Bool"
      , docDescription = "コンパイル済みカーネルに引数を設定し、GPU 上で実行します。完了まで同期的に待機します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "cl-enqueue"
      , docSyntax = "(cl-enqueue ctx kernel global-work-size local-work-size arg1 arg2 ...)"
      , docArgumentsAndValues = T.unlines
          [ "- `ctx` -- OpenCL コンテキスト (`CLContext`)"
          , "- `kernel` -- コンパイル済みカーネル (`CLKernel`)"
          , "- `global-work-size` -- 全スレッド数のリスト (例: `'(1024)`)。1D/2D/3D に対応。"
          , "- `local-work-size` -- ワークグループサイズのリスト (例: `'(64)`)。空リスト `'()` の場合はドライバ任せ。"
          , "- `argN` -- カーネルに渡す引数 (可変長):"
          , "    - `CLBuffer` → GPU メモリポインタとして渡される"
          , "    - `Int` → `cl_int` (4バイト) のスカラー値として渡される"
          , "    - `Float` → `cl_double` (8バイト) のスカラー値として渡される"
          , "- 戻り値: 成功時に `#t`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; ベクトル加算の例"
          , "(def ctx (cl-init))"
          , "(def m1 (matrix 1 1024 (list-of-range 1024)))"
          , "(def m2 (matrix 1 1024 (list-of-range 1024)))"
          , "(def b1 (to-device ctx m1))"
          , "(def b2 (to-device ctx m2))"
          , "(def b3 (to-device ctx m1))  ; 出力用バッファ"
          , ""
          , "(def src \"__kernel void add(__global double* a, __global double* b, __global double* c) {"
          , "    int i = get_global_id(0);"
          , "    c[i] = a[i] + b[i];"
          , "}\")"
          , "(def knl (cl-compile ctx src \"add\"))"
          , ""
          , "(cl-enqueue ctx knl '(1024) '() b1 b2 b3)  ; => #t"
          , "(def res (to-host ctx b3 1 1024))"
          , "```"
          ]
      , docSideEffects = "GPU 上でカーネルを実行し、完了を待機します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 第1引数が CLContext でない場合、エラーを返します。"
          , "- 第2引数が CLKernel でない場合、エラーを返します。"
          , "- global-work-size が空の場合、エラーを返します。"
          , "- カーネル引数の型が不正 (CLBuffer/Int/Float 以外) の場合、エラーを返します。"
          ]
      , docSeeAlso = ["cl-init", "cl-compile", "to-device", "to-host"]
      , docNotes = "現在はブロッキング実行のみ対応しています。"
      })

  -- ========================================
  -- OpenGL / GLFW (Visualization)
  -- ========================================
  , ("gl-init", DocEntry
      { docSignature = "(Int, Int, String) -> Window"
      , docDescription = "GLFW を初期化し、指定されたサイズとタイトルでウィンドウを作成します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "gl-init"
      , docSyntax = "(gl-init width height title)"
      , docArgumentsAndValues = T.unlines
          [ "- `width` -- ウィンドウの幅 (ピクセル)"
          , "- `height` -- ウィンドウの高さ (ピクセル)"
          , "- `title` -- ウィンドウのタイトル (文字列)"
          , "- 戻り値: GLFW ウィンドウハンドル (`Window`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(def win (gl-init 640 480 \"My Window\"))"
          , "win  ; => <Window>"
          , "```"
          ]
      , docSideEffects = "GLFW を初期化し、OpenGL コンテキスト付きのウィンドウを作成します。"
      , docAffectedBy = "ディスプレイ環境に依存します。"
      , docExceptionalSituations = T.unlines
          [ "- GLFW の初期化に失敗した場合、エラーを返します。"
          , "- ウィンドウの作成に失敗した場合、エラーを返します。"
          ]
      , docSeeAlso = ["gl-window-should-close", "gl-swap-buffers", "gl-clear", "gl-draw-points"]
      , docNotes = ""
      })

  , ("gl-window-should-close", DocEntry
      { docSignature = "(Window) -> Bool"
      , docDescription = "ウィンドウの閉じるボタンが押されたか、または終了フラグがセットされているかを確認します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "gl-window-should-close"
      , docSyntax = "(gl-window-should-close win)"
      , docArgumentsAndValues = T.unlines
          [ "- `win` -- GLFW ウィンドウ"
          , "- 戻り値: 閉じるべきなら `#t`、そうでなければ `#f`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(if (not (gl-window-should-close win))"
          , "    (begin (gl-clear) (gl-swap-buffers win))"
          , "    (print \"done\"))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "ユーザーのウィンドウ操作 (閉じるボタン) に依存します。"
      , docExceptionalSituations = "引数が Window でない場合、エラーを返します。"
      , docSeeAlso = ["gl-init", "gl-swap-buffers"]
      , docNotes = ""
      })

  , ("gl-swap-buffers", DocEntry
      { docSignature = "(Window) -> Nil"
      , docDescription = "フロントバッファとバックバッファを入れ替え、描画内容を画面に反映させます。同時に GLFW のイベントをポーリングします。"
      , docKind = CompletionItemKind_Function
      , docSlug = "gl-swap-buffers"
      , docSyntax = "(gl-swap-buffers win)"
      , docArgumentsAndValues = T.unlines
          [ "- `win` -- GLFW ウィンドウ"
          , "- 戻り値: `nil`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(gl-clear)"
          , "(gl-draw-points pts)"
          , "(gl-swap-buffers win)"
          , "```"
          ]
      , docSideEffects = "バッファスワップとイベントポーリングを行います。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "引数が Window でない場合、エラーを返します。"
      , docSeeAlso = ["gl-init", "gl-clear", "gl-draw-points"]
      , docNotes = ""
      })

  , ("gl-clear", DocEntry
      { docSignature = "() -> Nil"
      , docDescription = "カラーバッファをクリアします (デフォルトは黒)。"
      , docKind = CompletionItemKind_Function
      , docSlug = "gl-clear"
      , docSyntax = "(gl-clear)"
      , docArgumentsAndValues = T.unlines
          [ "- 引数なし"
          , "- 戻り値: `nil`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , "(gl-clear)  ; 画面を黒でクリア"
          , "```"
          ]
      , docSideEffects = "OpenGL のカラーバッファをクリアします。"
      , docAffectedBy = "None."
      , docExceptionalSituations = "None."
      , docSeeAlso = ["gl-draw-points", "gl-swap-buffers"]
      , docNotes = ""
      })

  , ("gl-draw-points", DocEntry
      { docSignature = "(Matrix) -> Nil"
      , docDescription = "Nx2 または Nx3 の行列を受け取り、各行を頂点座標として画面上に点 (GL_POINTS) を描画します。"
      , docKind = CompletionItemKind_Function
      , docSlug = "gl-draw-points"
      , docSyntax = "(gl-draw-points matrix)"
      , docArgumentsAndValues = T.unlines
          [ "- `matrix` -- 頂点座標の行列 (Nx2 または Nx3)"
          , "    - Nx2: 各行が `(x y)` の 2D 座標"
          , "    - Nx3: 各行が `(x y z)` の 3D 座標"
          , "- 戻り値: `nil`"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 3点を描画"
          , "(def pts (matrix 3 2 '(0.0 0.0  0.5 0.5  -0.5 -0.5)))"
          , "(gl-draw-points pts)"
          , "```"
          ]
      , docSideEffects = "OpenGL の固定機能パイプラインで点を描画します。"
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 引数が Matrix でない場合、エラーを返します。"
          , "- 列数が 2 または 3 でない場合、エラーを返します。"
          ]
      , docSeeAlso = ["gl-clear", "gl-swap-buffers", "matrix"]
      , docNotes = "座標系は OpenGL 標準の正規化デバイス座標系 (-1.0 ～ 1.0) です。"
      })

  -- ========================================
  -- JSON 操作 (JSON Operations)
  -- ========================================
  , ("json-parse", DocEntry
      { docSignature = "(String) -> Val"
      , docDescription = "JSON 文字列をパースして Spinor の値に変換します。オブジェクトは連想リスト (Alist) として表現されます。"
      , docKind = CompletionItemKind_Function
      , docSlug = "json-parse"
      , docSyntax = "(json-parse json-string)"
      , docArgumentsAndValues = T.unlines
          [ "- `json-string` -- パースする JSON 文字列"
          , "- 戻り値: Spinor の値"
          , "    - JSON Number (整数) → `VInt`"
          , "    - JSON Number (浮動小数) → `VFloat`"
          , "    - JSON String → `VStr`"
          , "    - JSON Boolean → `VBool` (`true` → `#t`, `false` → `#f`)"
          , "    - JSON Null → `VNil` (`nil`)"
          , "    - JSON Array → `VList`"
          , "    - JSON Object → `VList` (Alist: `((\"key1\" val1) (\"key2\" val2) ...)`)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 基本的なパース"
          , "(json-parse \"42\")           ; => 42"
          , "(json-parse \"3.14\")         ; => 3.14"
          , "(json-parse \"\\\"hello\\\"\")    ; => \"hello\""
          , "(json-parse \"true\")         ; => #t"
          , "(json-parse \"null\")         ; => nil"
          , ""
          , ";; 配列のパース"
          , "(json-parse \"[1, 2, 3]\")    ; => (1 2 3)"
          , ""
          , ";; オブジェクトのパース (Alist として表現)"
          , "(json-parse \"{\\\"name\\\": \\\"Alice\\\", \\\"age\\\": 30}\")"
          , "; => ((\"name\" \"Alice\") (\"age\" 30))"
          , ""
          , ";; ネストしたデータ"
          , "(json-parse \"{\\\"items\\\": [1, 2, 3]}\")"
          , "; => ((\"items\" (1 2 3)))"
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 不正な JSON 文字列の場合、パースエラーを返します。"
          , "- 引数が文字列でない場合、エラーを返します。"
          ]
      , docSeeAlso = ["json-stringify"]
      , docNotes = "内部的に aeson パッケージを使用しています。"
      })

  , ("json-stringify", DocEntry
      { docSignature = "(Val) -> String"
      , docDescription = "Spinor の値を JSON 文字列に変換します。連想リスト (Alist) は JSON オブジェクトとして出力されます。"
      , docKind = CompletionItemKind_Function
      , docSlug = "json-stringify"
      , docSyntax = "(json-stringify value)"
      , docArgumentsAndValues = T.unlines
          [ "- `value` -- JSON に変換する値"
          , "- 戻り値: JSON 文字列"
          , ""
          , "変換規則:"
          , "    - `VInt` → JSON Number (整数)"
          , "    - `VFloat` → JSON Number (浮動小数)"
          , "    - `VStr` → JSON String"
          , "    - `VBool` → JSON Boolean (`#t` → `true`, `#f` → `false`)"
          , "    - `VNil` → JSON Null (`null`)"
          , "    - `VList` → JSON Array (通常のリスト) または JSON Object (Alist)"
          , "    - `VSym` → JSON String (シンボルは文字列として変換)"
          ]
      , docExamples = T.unlines
          [ "```lisp"
          , ";; 基本的な変換"
          , "(json-stringify 42)          ; => \"42\""
          , "(json-stringify 3.14)        ; => \"3.14\""
          , "(json-stringify \"hello\")    ; => \"\\\"hello\\\"\""
          , "(json-stringify #t)          ; => \"true\""
          , "(json-stringify nil)         ; => \"null\""
          , ""
          , ";; 配列の変換"
          , "(json-stringify (list 1 2 3))  ; => \"[1,2,3]\""
          , ""
          , ";; オブジェクトの変換 (Alist から)"
          , "(json-stringify '((\"name\" \"Alice\") (\"age\" 30)))"
          , "; => \"{\\\"name\\\":\\\"Alice\\\",\\\"age\\\":30}\""
          , "```"
          ]
      , docSideEffects = "None."
      , docAffectedBy = "None."
      , docExceptionalSituations = T.unlines
          [ "- 関数、マクロ、MVar など JSON に変換できない型が含まれる場合、エラーを返します。"
          , "- 行列 (VMatrix)、CLContext、CLBuffer、Window も変換不可です。"
          ]
      , docSeeAlso = ["json-parse"]
      , docNotes = "Alist として認識されるには、リストの全要素が `(\"key\" value)` の形式である必要があります。"
      })
  ]

lookupDoc :: Text -> Maybe DocEntry
lookupDoc = flip Map.lookup primitiveDocs

allDocEntries :: [(Text, DocEntry)]
allDocEntries = Map.toList primitiveDocs
