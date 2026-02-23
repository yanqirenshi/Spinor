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
    it "(let x 5 (+ x 3)) → 8 (旧形式)" $
      "(let x 5 (+ x 3))" `shouldEvalTo` VInt 8
    it "ネストした let (旧形式)" $
      "(let x 1 (let y 2 (+ x y)))" `shouldEvalTo` VInt 3
    it "新形式: (let ((x 10)) x) → 10" $
      "(let ((x 10)) x)" `shouldEvalTo` VInt 10
    it "新形式: 複数束縛 (let ((x 1) (y 2)) (+ x y)) → 3" $
      "(let ((x 1) (y 2)) (+ x y))" `shouldEvalTo` VInt 3
    it "並列束縛: 外側の x=10 を参照 (let ((x 2) (y (+ x 5))) (list x y)) → (2 15)" $ do
      result <- evalMulti "(let ((x 10)) (let ((x 2) (y (+ x 5))) (list x y)))"
      result `shouldBe` Right (VList [VInt 2, VInt 15])

  describe "setq (破壊的代入)" $ do
    it "(let ((x 0)) (begin (setq x 10) x)) -> 10" $
      "(let ((x 0)) (begin (setq x 10) x))" `shouldEvalTo` VInt 10
    it "(let ((counter 0)) (begin (setq counter (+ counter 1)) counter)) -> 1" $
      "(let ((counter 0)) (begin (setq counter (+ counter 1)) counter))" `shouldEvalTo` VInt 1
    it "未束縛変数への setq はエラー" $ do
      result <- evalStr "(setq undefined-var 10)"
      case result of
        Left _ -> pure ()  -- エラーが発生すればOK
        Right _ -> expectationFailure "未束縛変数への setq はエラーになるべき"

  describe "eq と equal" $ do
    it "(eq 1 1) → #t" $
      "(eq 1 1)" `shouldEvalTo` VBool True
    it "(eq 1 2) → #f" $
      "(eq 1 2)" `shouldEvalTo` VBool False
    it "(eq 'a 'a) → #t" $
      "(eq 'a 'a)" `shouldEvalTo` VBool True
    it "(equal 1 1) → #t" $
      "(equal 1 1)" `shouldEvalTo` VBool True
    it "(equal (list 1 2) (list 1 2)) → #t (構造的等価)" $
      "(equal (list 1 2) (list 1 2))" `shouldEvalTo` VBool True
    it "(eq (list 1 2) (list 1 2)) → #f (参照等価)" $
      "(eq (list 1 2) (list 1 2))" `shouldEvalTo` VBool False
    it "(equal \"hello\" \"hello\") → #t" $
      "(equal \"hello\" \"hello\")" `shouldEvalTo` VBool True
    it "(eq \"hello\" \"hello\") → #t (文字列は eq でも #t)" $
      "(eq \"hello\" \"hello\")" `shouldEvalTo` VBool True

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

  describe "パターンマッチ (match)" $ do
    it "Maybe のマッチ: (Just 42) → (+ v 1) → 43" $ do
      result <- evalMulti "(data Maybe (Just a) (Nothing))\n(match (Just 42) ((Just v) (+ v 1)) (Nothing 0))"
      result `shouldBe` Right (VInt 43)
    it "0引数コンストラクタマッチ: Nothing → 0" $ do
      result <- evalMulti "(data Maybe (Just a) (Nothing))\n(match Nothing ((Just v) (+ v 1)) (Nothing 0))"
      result `shouldBe` Right (VInt 0)
    it "ワイルドカードとリテラル: (match 10 (0 \"zero\") (_ \"non-zero\"))" $
      "(match 10 (0 \"zero\") (_ \"non-zero\"))" `shouldEvalTo` VStr "non-zero"
    it "リテラルマッチ: (match 0 (0 \"zero\") (_ \"non-zero\"))" $
      "(match 0 (0 \"zero\") (_ \"non-zero\"))" `shouldEvalTo` VStr "zero"
    it "ネストしたパターン: (MyCons x xs) マッチ" $ do
      result <- evalMulti "(data MyList (MyCons a (MyList a)) (MyNil))\n(match (MyCons 1 (MyCons 2 MyNil)) ((MyCons x xs) x) (MyNil 0))"
      result `shouldBe` Right (VInt 1)

  describe "行列操作 (Matrix)" $ do
    it "matrix で 2x2 行列を生成" $ do
      result <- evalStr "(matrix 2 2 '(1 2 3 4))"
      case result of
        Right (VMatrix 2 2 _) -> pure ()  -- 型が正しければOK
        _ -> expectationFailure "2x2 行列が生成されるべき"

    it "mdim で行列の次元を取得" $ do
      result <- evalStr "(mdim (matrix 3 4 '(1 2 3 4 5 6 7 8 9 10 11 12)))"
      result `shouldBe` Right (VList [VInt 3, VInt 4])

    it "mref で要素を取得 (0,0)" $ do
      result <- evalStr "(mref (matrix 2 2 '(1 2 3 4)) 0 0)"
      result `shouldBe` Right (VFloat 1.0)

    it "mref で要素を取得 (1,1)" $ do
      result <- evalStr "(mref (matrix 2 2 '(1 2 3 4)) 1 1)"
      result `shouldBe` Right (VFloat 4.0)

    it "mref で要素を取得 (0,1)" $ do
      result <- evalStr "(mref (matrix 2 3 '(1 2 3 4 5 6)) 0 2)"
      result `shouldBe` Right (VFloat 3.0)

    it "matrix で要素数不一致はエラー" $ do
      result <- evalStr "(matrix 2 2 '(1 2 3))"
      case result of
        Left _ -> pure ()  -- エラーが発生すればOK
        Right _ -> expectationFailure "要素数不一致でエラーになるべき"

    it "mref で範囲外アクセスはエラー (行)" $ do
      result <- evalStr "(mref (matrix 2 2 '(1 2 3 4)) 5 0)"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "範囲外アクセスでエラーになるべき"

    it "mref で範囲外アクセスはエラー (列)" $ do
      result <- evalStr "(mref (matrix 2 2 '(1 2 3 4)) 0 5)"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "範囲外アクセスでエラーになるべき"

    it "matrix は VInt を VFloat に変換" $ do
      result <- evalStr "(mref (matrix 1 1 '(42)) 0 0)"
      result `shouldBe` Right (VFloat 42.0)

  describe "BLAS/LAPACK 行列演算" $ do
    it "m+ で 2x2 行列の加算" $ do
      result <- evalStr "(mref (m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(5 6 7 8))) 0 0)"
      result `shouldBe` Right (VFloat 6.0)

    it "m+ の全要素が正しい" $ do
      r00 <- evalStr "(mref (m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(10 20 30 40))) 0 0)"
      r01 <- evalStr "(mref (m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(10 20 30 40))) 0 1)"
      r10 <- evalStr "(mref (m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(10 20 30 40))) 1 0)"
      r11 <- evalStr "(mref (m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(10 20 30 40))) 1 1)"
      r00 `shouldBe` Right (VFloat 11.0)
      r01 `shouldBe` Right (VFloat 22.0)
      r10 `shouldBe` Right (VFloat 33.0)
      r11 `shouldBe` Right (VFloat 44.0)

    it "m+ で次元不一致はエラー" $ do
      result <- evalStr "(m+ (matrix 2 2 '(1 2 3 4)) (matrix 2 3 '(1 2 3 4 5 6)))"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "次元不一致でエラーになるべき"

    it "m* で 2x2 行列の積" $ do
      -- [[1,2],[3,4]] * [[5,6],[7,8]] = [[19,22],[43,50]]
      r00 <- evalStr "(mref (m* (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(5 6 7 8))) 0 0)"
      r01 <- evalStr "(mref (m* (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(5 6 7 8))) 0 1)"
      r10 <- evalStr "(mref (m* (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(5 6 7 8))) 1 0)"
      r11 <- evalStr "(mref (m* (matrix 2 2 '(1 2 3 4)) (matrix 2 2 '(5 6 7 8))) 1 1)"
      r00 `shouldBe` Right (VFloat 19.0)
      r01 `shouldBe` Right (VFloat 22.0)
      r10 `shouldBe` Right (VFloat 43.0)
      r11 `shouldBe` Right (VFloat 50.0)

    it "m* で 2x3 と 3x2 の行列積" $ do
      -- [[1,2,3],[4,5,6]] * [[7,8],[9,10],[11,12]] = [[58,64],[139,154]]
      r00 <- evalStr "(mref (m* (matrix 2 3 '(1 2 3 4 5 6)) (matrix 3 2 '(7 8 9 10 11 12))) 0 0)"
      r11 <- evalStr "(mref (m* (matrix 2 3 '(1 2 3 4 5 6)) (matrix 3 2 '(7 8 9 10 11 12))) 1 1)"
      r00 `shouldBe` Right (VFloat 58.0)
      r11 `shouldBe` Right (VFloat 154.0)

    it "m* で次元不一致はエラー" $ do
      result <- evalStr "(m* (matrix 2 2 '(1 2 3 4)) (matrix 3 2 '(1 2 3 4 5 6)))"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "次元不一致でエラーになるべき"

    it "m* の結果の次元が正しい" $ do
      result <- evalStr "(mdim (m* (matrix 2 3 '(1 2 3 4 5 6)) (matrix 3 2 '(7 8 9 10 11 12))))"
      result `shouldBe` Right (VList [VInt 2, VInt 2])

    it "transpose で 2x3 行列の転置" $ do
      -- [[1,2,3],[4,5,6]] -> [[1,4],[2,5],[3,6]]
      dim <- evalStr "(mdim (transpose (matrix 2 3 '(1 2 3 4 5 6))))"
      dim `shouldBe` Right (VList [VInt 3, VInt 2])
      r00 <- evalStr "(mref (transpose (matrix 2 3 '(1 2 3 4 5 6))) 0 0)"
      r10 <- evalStr "(mref (transpose (matrix 2 3 '(1 2 3 4 5 6))) 1 0)"
      r01 <- evalStr "(mref (transpose (matrix 2 3 '(1 2 3 4 5 6))) 0 1)"
      r00 `shouldBe` Right (VFloat 1.0)
      r10 `shouldBe` Right (VFloat 2.0)
      r01 `shouldBe` Right (VFloat 4.0)

    it "inverse で単位行列の逆行列は単位行列" $ do
      r00 <- evalStr "(mref (inverse (matrix 2 2 '(1 0 0 1))) 0 0)"
      r01 <- evalStr "(mref (inverse (matrix 2 2 '(1 0 0 1))) 0 1)"
      r10 <- evalStr "(mref (inverse (matrix 2 2 '(1 0 0 1))) 1 0)"
      r11 <- evalStr "(mref (inverse (matrix 2 2 '(1 0 0 1))) 1 1)"
      r00 `shouldBe` Right (VFloat 1.0)
      r01 `shouldBe` Right (VFloat 0.0)
      r10 `shouldBe` Right (VFloat 0.0)
      r11 `shouldBe` Right (VFloat 1.0)

    it "inverse で非正方行列はエラー" $ do
      result <- evalStr "(inverse (matrix 2 3 '(1 2 3 4 5 6)))"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "非正方行列でエラーになるべき"

    it "inverse で特異行列はエラー" $ do
      result <- evalStr "(inverse (matrix 2 2 '(1 2 2 4)))"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "特異行列でエラーになるべき"
