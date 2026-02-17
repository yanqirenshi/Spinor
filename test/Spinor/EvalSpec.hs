{-# LANGUAGE OverloadedStrings #-}

module Spinor.EvalSpec (spec) where

import Test.Hspec
import Data.Text (Text, pack)
import Spinor.Syntax    (readExpr, parseFile)
import Spinor.Val       (Val(..))
import Spinor.Eval      (runEval, eval)
import Spinor.Expander  (expand, expandAndEval)
import Spinor.Primitive (primitiveBindings)

-- | ヘルパー: 文字列をパース → 展開 → 評価し、結果の Val を返す
evalStr :: Text -> IO (Either Text Val)
evalStr input =
  case readExpr input of
    Left err  -> pure $ Left (pack err)
    Right ast -> do
      result <- runEval primitiveBindings (expand ast >>= eval)
      pure $ case result of
        Left err       -> Left err
        Right (val, _) -> Right val

-- | ヘルパー: 複数式を順次パース → 展開 → 評価し、最後の結果を返す
evalMulti :: Text -> IO (Either Text Val)
evalMulti input =
  case parseFile input of
    Left err    -> pure $ Left (pack err)
    Right exprs -> do
      result <- runEval primitiveBindings (mapM expandAndEval exprs)
      pure $ case result of
        Left err        -> Left err
        Right (vals, _) -> Right (last vals)

-- | ヘルパー: 評価結果が期待値と一致するか検証
shouldEvalTo :: Text -> Val -> Expectation
shouldEvalTo input expected = do
  result <- evalStr input
  result `shouldBe` Right expected

spec :: Spec
spec = describe "Spinor.Eval (Evaluator)" $ do

  describe "基本的な算術演算" $ do
    it "(+ 1 2) → 3" $
      "(+ 1 2)" `shouldEvalTo` VInt 3
    it "(- 10 3) → 7" $
      "(- 10 3)" `shouldEvalTo` VInt 7
    it "(* 4 5) → 20" $
      "(* 4 5)" `shouldEvalTo` VInt 20
    it "(% 7 3) → 1" $
      "(% 7 3)" `shouldEvalTo` VInt 1
    it "ネストした算術: (+ (* 2 3) (- 10 4))" $
      "(+ (* 2 3) (- 10 4))" `shouldEvalTo` VInt 12

  describe "比較演算" $ do
    it "(= 1 1) → #t" $
      "(= 1 1)" `shouldEvalTo` VBool True
    it "(= 1 2) → #f" $
      "(= 1 2)" `shouldEvalTo` VBool False
    it "(< 1 2) → #t" $
      "(< 1 2)" `shouldEvalTo` VBool True
    it "(> 1 2) → #f" $
      "(> 1 2)" `shouldEvalTo` VBool False

  describe "if 式" $ do
    it "(if #t 1 2) → 1" $
      "(if #t 1 2)" `shouldEvalTo` VInt 1
    it "(if #f 1 2) → 2" $
      "(if #f 1 2)" `shouldEvalTo` VInt 2
    it "条件が式: (if (= 1 1) 10 20) → 10" $
      "(if (= 1 1) 10 20)" `shouldEvalTo` VInt 10

  describe "関数定義と適用" $ do
    it "即時適用: ((fn (x) (+ x 1)) 10)" $
      "((fn (x) (+ x 1)) 10)" `shouldEvalTo` VInt 11
    it "多引数: ((fn (x y) (+ x y)) 3 4)" $
      "((fn (x y) (+ x y)) 3 4)" `shouldEvalTo` VInt 7

  describe "let 式" $ do
    it "(let x 5 (+ x 3)) → 8" $
      "(let x 5 (+ x 3))" `shouldEvalTo` VInt 8
    it "ネストした let" $
      "(let x 1 (let y 2 (+ x y)))" `shouldEvalTo` VInt 3

  describe "リスト操作" $ do
    it "cons でリスト構築" $
      "(cons 1 (cons 2 (cons 3 (list))))" `shouldEvalTo` VList [VInt 1, VInt 2, VInt 3]
    it "car でリストの先頭取得" $
      "(car (cons 1 (list 2 3)))" `shouldEvalTo` VInt 1
    it "cdr でリストの残り取得" $
      "(cdr (list 1 2 3))" `shouldEvalTo` VList [VInt 2, VInt 3]
    it "null? で空リスト判定" $
      "(null? (list))" `shouldEvalTo` VBool True
    it "null? で非空リスト判定" $
      "(null? (list 1))" `shouldEvalTo` VBool False

  describe "quote" $ do
    it "数値の quote" $
      "(quote 42)" `shouldEvalTo` VInt 42
    it "リストの quote" $
      "(quote (1 2 3))" `shouldEvalTo` VList [VInt 1, VInt 2, VInt 3]
    it "quote 略記" $
      "'(1 2)" `shouldEvalTo` VList [VInt 1, VInt 2]

  describe "ユーザー定義データ型 (ADT)" $ do
    it "0引数コンストラクタ: Nothing → VData" $ do
      result <- evalMulti "(data Maybe (Just a) (Nothing))\nNothing"
      result `shouldBe` Right (VData "Nothing" [])
    it "1引数コンストラクタ: (Just 10) → VData" $ do
      result <- evalMulti "(data Maybe (Just a) (Nothing))\n(Just 10)"
      result `shouldBe` Right (VData "Just" [VInt 10])
    it "ネストしたコンストラクタ: (MyCons 1 (MyCons 2 MyNil))" $ do
      result <- evalMulti "(data MyList (MyCons a (MyList a)) (MyNil))\n(MyCons 1 (MyCons 2 MyNil))"
      result `shouldBe` Right (VData "MyCons" [VInt 1, VData "MyCons" [VInt 2, VData "MyNil" []]])
