{-# LANGUAGE OverloadedStrings #-}

module Spinor.ParserSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Spinor.Syntax (Expr(..), Pattern(..), TypeExpr(..), ConstructorDef(..), ImportOption(..), readExpr, dummySpan)

-- | SourceSpan を無視して Expr を比較するためのヘルパー
-- パース結果の SourceSpan を dummySpan に正規化する
normalizeSpan :: Expr -> Expr
normalizeSpan (EInt _ n) = EInt dummySpan n
normalizeSpan (EBool _ b) = EBool dummySpan b
normalizeSpan (EStr _ s) = EStr dummySpan s
normalizeSpan (ESym _ s) = ESym dummySpan s
normalizeSpan (EList _ xs) = EList dummySpan (map normalizeSpan xs)
normalizeSpan (ELet _ bindings body) = ELet dummySpan [(n, normalizeSpan e) | (n, e) <- bindings] (normalizeSpan body)
normalizeSpan (EData _ name ctors) = EData dummySpan name ctors
normalizeSpan (EMatch _ target branches) = EMatch dummySpan (normalizeSpan target) [(normalizePat p, normalizeSpan e) | (p, e) <- branches]
normalizeSpan (EModule _ name exports) = EModule dummySpan name exports
normalizeSpan (EImport _ name opts) = EImport dummySpan name opts

-- | パターン内の PLit の Expr も正規化
normalizePat :: Pattern -> Pattern
normalizePat (PVar s) = PVar s
normalizePat (PCon c ps) = PCon c (map normalizePat ps)
normalizePat (PLit e) = PLit (normalizeSpan e)
normalizePat PWild = PWild

-- | パース結果を正規化して比較
parseShouldBe :: String -> Expr -> Expectation
parseShouldBe input expected =
  fmap normalizeSpan (readExpr (T.pack input)) `shouldBe` Right expected

-- | 短縮版の Expr コンストラクタ (テスト用)
eInt :: Integer -> Expr
eInt = EInt dummySpan

eBool :: Bool -> Expr
eBool = EBool dummySpan

eStr :: T.Text -> Expr
eStr = EStr dummySpan

eSym :: T.Text -> Expr
eSym = ESym dummySpan

eList :: [Expr] -> Expr
eList = EList dummySpan

eLet :: [(T.Text, Expr)] -> Expr -> Expr
eLet = ELet dummySpan

eData :: T.Text -> [ConstructorDef] -> Expr
eData = EData dummySpan

eMatch :: Expr -> [(Pattern, Expr)] -> Expr
eMatch = EMatch dummySpan

eModule :: T.Text -> [T.Text] -> Expr
eModule = EModule dummySpan

eImport :: T.Text -> [ImportOption] -> Expr
eImport = EImport dummySpan

spec :: Spec
spec = describe "Spinor.Syntax (Parser)" $ do

  describe "整数リテラル" $ do
    it "正の整数" $
      "42" `parseShouldBe` eInt 42
    it "負の整数" $
      "-7" `parseShouldBe` eInt (-7)
    it "ゼロ" $
      "0" `parseShouldBe` eInt 0

  describe "真偽値" $ do
    it "#t → EBool True" $
      "#t" `parseShouldBe` eBool True
    it "#f → EBool False" $
      "#f" `parseShouldBe` eBool False

  describe "文字列リテラル" $ do
    it "二重引用符で囲まれた文字列" $
      "\"hello\"" `parseShouldBe` eStr "hello"

  describe "シンボル" $ do
    it "英字シンボル" $
      "foo" `parseShouldBe` eSym "foo"
    it "記号を含むシンボル" $
      "null?" `parseShouldBe` eSym "null?"
    it "演算子シンボル" $
      "+" `parseShouldBe` eSym "+"

  describe "リスト" $ do
    it "空リスト" $
      "()" `parseShouldBe` eList []
    it "整数のリスト" $
      "(1 2 3)" `parseShouldBe` eList [eInt 1, eInt 2, eInt 3]
    it "関数適用式" $
      "(+ 1 2)" `parseShouldBe` eList [eSym "+", eInt 1, eInt 2]
    it "ネストしたリスト" $
      "(+ (* 2 3) 4)" `parseShouldBe`
        eList [eSym "+", eList [eSym "*", eInt 2, eInt 3], eInt 4]

  describe "quote" $ do
    it "quote 略記" $
      "'(1 2 3)" `parseShouldBe`
        eList [eSym "quote", eList [eInt 1, eInt 2, eInt 3]]
    it "quote 正式形式" $
      "(quote foo)" `parseShouldBe`
        eList [eSym "quote", eSym "foo"]

  describe "let 式" $ do
    it "(let x 1 x) → ELet (旧形式)" $
      "(let x 1 x)" `parseShouldBe`
        eLet [("x", eInt 1)] (eSym "x")
    it "(let ((x 1)) x) → ELet (新形式)" $
      "(let ((x 1)) x)" `parseShouldBe`
        eLet [("x", eInt 1)] (eSym "x")
    it "(let ((x 1) (y 2)) (+ x y)) → ELet 複数束縛" $
      "(let ((x 1) (y 2)) (+ x y))" `parseShouldBe`
        eLet [("x", eInt 1), ("y", eInt 2)] (eList [eSym "+", eSym "x", eSym "y"])

  describe "define 式" $ do
    it "(define x 42)" $
      "(define x 42)" `parseShouldBe`
        eList [eSym "define", eSym "x", eInt 42]

  describe "data 式 (ADT)" $ do
    it "(data Maybe (Just a) (Nothing))" $
      "(data Maybe (Just a) (Nothing))" `parseShouldBe`
        eData "Maybe"
          [ ConstructorDef "Just" [TEVar "a"]
          , ConstructorDef "Nothing" []
          ]
    it "(data MyList (MyCons a (MyList a)) (MyNil))" $
      "(data MyList (MyCons a (MyList a)) (MyNil))" `parseShouldBe`
        eData "MyList"
          [ ConstructorDef "MyCons" [TEVar "a", TEApp "MyList" [TEVar "a"]]
          , ConstructorDef "MyNil" []
          ]

  describe "match 式 (パターンマッチ)" $ do
    it "(match x ((Just v) (+ v 1)) (Nothing 0)) のパース" $
      "(match x ((Just v) (+ v 1)) (Nothing 0))" `parseShouldBe`
        eMatch (eSym "x")
          [ (PCon "Just" [PVar "v"], eList [eSym "+", eSym "v", eInt 1])
          , (PCon "Nothing" [], eInt 0)
          ]
    it "ワイルドカードパターン _ のパース" $
      "(match x (_ 0))" `parseShouldBe`
        eMatch (eSym "x")
          [ (PWild, eInt 0)
          ]
    it "リテラルパターンのパース" $
      "(match x (0 \"zero\") (_ \"other\"))" `parseShouldBe`
        eMatch (eSym "x")
          [ (PLit (eInt 0), eStr "zero")
          , (PWild, eStr "other")
          ]

  describe "module 宣言" $ do
    it "(module my-mod (export a b))" $
      "(module my-mod (export a b))" `parseShouldBe`
        eModule "my-mod" ["a", "b"]
    it "(module my-mod (export)) - 空エクスポート" $
      "(module my-mod (export))" `parseShouldBe`
        eModule "my-mod" []
    it "(module my-mod) - エクスポート省略" $
      "(module my-mod)" `parseShouldBe`
        eModule "my-mod" []

  describe "import 宣言" $ do
    it "(import lib) - オプションなし" $
      "(import lib)" `parseShouldBe`
        eImport "lib" []
    it "(import lib (only a b))" $
      "(import lib (only a b))" `parseShouldBe`
        eImport "lib" [Only ["a", "b"]]
    it "(import lib (except x))" $
      "(import lib (except x))" `parseShouldBe`
        eImport "lib" [Except ["x"]]
    it "(import lib (prefix m-))" $
      "(import lib (prefix m-))" `parseShouldBe`
        eImport "lib" [Prefix "m-"]
    it "(import lib (alias M))" $
      "(import lib (alias M))" `parseShouldBe`
        eImport "lib" [Alias "M"]
    it "(import 'twister/core) - quote 形式" $
      "(import 'twister/core)" `parseShouldBe`
        eImport "twister/core" []
