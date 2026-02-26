{-# LANGUAGE OverloadedStrings #-}

module Spinor.BorrowCheckSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map

import Spinor.Syntax (Expr(..), readExpr, dummySpan)
import Spinor.BorrowCheck

-- | テスト用に文字列から Expr をパースするヘルパー
parseExpr :: Text -> Expr
parseExpr input = case readExpr input of
  Right expr -> expr
  Left err   -> error $ "Parse error: " ++ err

-- | 複数の式をパースするヘルパー
parseExprs :: [Text] -> [Expr]
parseExprs = map parseExpr

spec :: Spec
spec = do
  describe "BorrowCheck" $ do
    describe "基本的な所有権チェック" $ do
      it "通常の変数使用ではエラーなし" $ do
        let exprs = parseExprs ["(let ((x 1)) x)"]
            result = checkBorrow exprs
        brErrors result `shouldBe` []

      it "未使用の let 変数はエラーなし (非線形)" $ do
        let exprs = parseExprs ["(let ((x 1)) 42)"]
            result = checkBorrow exprs
        brErrors result `shouldBe` []

      it "複数回の変数参照もエラーなし (非線形)" $ do
        let exprs = parseExprs ["(let ((x 1)) (+ x x))"]
            result = checkBorrow exprs
        brErrors result `shouldBe` []

    describe "線形型の検証" $ do
      it "linear で宣言した変数が未使用ならエラー" $ do
        let exprs = parseExprs ["(linear y 42)"]
            result = checkBorrow exprs
        length (brErrors result) `shouldBe` 1
        case head (brErrors result) of
          Unconsumed name _ -> name `shouldBe` "y"
          _ -> expectationFailure "Expected Unconsumed error"

      it "linear 変数を1回使用すればエラーなし" $ do
        -- linear で宣言して即座に参照
        let expr = EList dummySpan
              [ ESym dummySpan "begin"
              , EList dummySpan
                  [ ESym dummySpan "linear"
                  , ESym dummySpan "z"
                  , EInt dummySpan 100
                  ]
              , ESym dummySpan "z"
              ]
            result = checkBorrow [expr]
        -- 線形変数 z は1回使用されているのでエラーなし
        brErrors result `shouldBe` []

    describe "drop ポイントの追跡" $ do
      it "let 束縛の変数は drop ポイントが記録される" $ do
        let exprs = parseExprs ["(let ((resource 1)) resource)"]
            result = checkBorrow exprs
        Map.member "resource" (brDropPoints result) `shouldBe` True

    describe "エラーメッセージのフォーマット" $ do
      it "DoubleUse エラーが正しくフォーマットされる" $ do
        let err = DoubleUse "x" dummySpan dummySpan
        formatBorrowError err `shouldBe` "Linear variable 'x' used more than once"

      it "Unconsumed エラーが正しくフォーマットされる" $ do
        let err = Unconsumed "y" dummySpan
        formatBorrowError err `shouldBe` "Linear variable 'y' must be consumed before scope ends"

      it "UseAfterMove エラーが正しくフォーマットされる" $ do
        let err = UseAfterMove "z" dummySpan dummySpan
        formatBorrowError err `shouldBe` "Variable 'z' used after being moved"
