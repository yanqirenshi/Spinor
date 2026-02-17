{-# LANGUAGE OverloadedStrings #-}

module Spinor.ParserSpec (spec) where

import Test.Hspec
import Spinor.Syntax (Expr(..), Pattern(..), TypeExpr(..), ConstructorDef(..), readExpr)

spec :: Spec
spec = describe "Spinor.Syntax (Parser)" $ do

  describe "整数リテラル" $ do
    it "正の整数" $
      readExpr "42" `shouldBe` Right (EInt 42)
    it "負の整数" $
      readExpr "-7" `shouldBe` Right (EInt (-7))
    it "ゼロ" $
      readExpr "0" `shouldBe` Right (EInt 0)

  describe "真偽値" $ do
    it "#t → EBool True" $
      readExpr "#t" `shouldBe` Right (EBool True)
    it "#f → EBool False" $
      readExpr "#f" `shouldBe` Right (EBool False)

  describe "文字列リテラル" $ do
    it "二重引用符で囲まれた文字列" $
      readExpr "\"hello\"" `shouldBe` Right (EStr "hello")

  describe "シンボル" $ do
    it "英字シンボル" $
      readExpr "foo" `shouldBe` Right (ESym "foo")
    it "記号を含むシンボル" $
      readExpr "null?" `shouldBe` Right (ESym "null?")
    it "演算子シンボル" $
      readExpr "+" `shouldBe` Right (ESym "+")

  describe "リスト" $ do
    it "空リスト" $
      readExpr "()" `shouldBe` Right (EList [])
    it "整数のリスト" $
      readExpr "(1 2 3)" `shouldBe` Right (EList [EInt 1, EInt 2, EInt 3])
    it "関数適用式" $
      readExpr "(+ 1 2)" `shouldBe` Right (EList [ESym "+", EInt 1, EInt 2])
    it "ネストしたリスト" $
      readExpr "(+ (* 2 3) 4)" `shouldBe`
        Right (EList [ESym "+", EList [ESym "*", EInt 2, EInt 3], EInt 4])

  describe "quote" $ do
    it "quote 略記" $
      readExpr "'(1 2 3)" `shouldBe`
        Right (EList [ESym "quote", EList [EInt 1, EInt 2, EInt 3]])
    it "quote 正式形式" $
      readExpr "(quote foo)" `shouldBe`
        Right (EList [ESym "quote", ESym "foo"])

  describe "let 式" $ do
    it "(let x 1 x) → ELet" $
      readExpr "(let x 1 x)" `shouldBe`
        Right (ELet "x" (EInt 1) (ESym "x"))

  describe "define 式" $ do
    it "(define x 42)" $
      readExpr "(define x 42)" `shouldBe`
        Right (EList [ESym "define", ESym "x", EInt 42])

  describe "data 式 (ADT)" $ do
    it "(data Maybe (Just a) (Nothing))" $
      readExpr "(data Maybe (Just a) (Nothing))" `shouldBe`
        Right (EData "Maybe"
          [ ConstructorDef "Just" [TEVar "a"]
          , ConstructorDef "Nothing" []
          ])
    it "(data MyList (MyCons a (MyList a)) (MyNil))" $
      readExpr "(data MyList (MyCons a (MyList a)) (MyNil))" `shouldBe`
        Right (EData "MyList"
          [ ConstructorDef "MyCons" [TEVar "a", TEApp "MyList" [TEVar "a"]]
          , ConstructorDef "MyNil" []
          ])

  describe "match 式 (パターンマッチ)" $ do
    it "(match x ((Just v) (+ v 1)) (Nothing 0)) のパース" $
      readExpr "(match x ((Just v) (+ v 1)) (Nothing 0))" `shouldBe`
        Right (EMatch (ESym "x")
          [ (PCon "Just" [PVar "v"], EList [ESym "+", ESym "v", EInt 1])
          , (PCon "Nothing" [], EInt 0)
          ])
    it "ワイルドカードパターン _ のパース" $
      readExpr "(match x (_ 0))" `shouldBe`
        Right (EMatch (ESym "x")
          [ (PWild, EInt 0)
          ])
    it "リテラルパターンのパース" $
      readExpr "(match x (0 \"zero\") (_ \"other\"))" `shouldBe`
        Right (EMatch (ESym "x")
          [ (PLit (EInt 0), EStr "zero")
          , (PWild, EStr "other")
          ])
