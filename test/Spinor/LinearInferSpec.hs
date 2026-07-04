{-# LANGUAGE OverloadedStrings #-}

-- | Linear Spinor 型推論 (Phase R0) の受け入れ条件を検証するテスト。
--
--   * Issue #66 (R0-2): リソース消費トラッキング
--       - use-after-move / resource leak / 消費後の env 削除
--   * Issue #72 (R0-3): 所有権・借用のセマンティクス
--       - 借用 (&) は消費しない / ムーブ (@) は消費する
--       - 参照解決 (*&x) は借用型を剥がす / 隔離 (unsafe) は消費を漏らさない
module Spinor.LinearInferSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Data.Text (Text, isInfixOf)
import Data.Either (isRight)

import Spinor.Syntax
import Spinor.Type
import Spinor.Infer

-- | 線形変数 x を 1 つ持つ型環境
linearEnv :: TypeEnv
linearEnv = Map.fromList [("x", Scheme [] (TLinear Linear TInt))]

-- | runInfer の結果からエラーメッセージを取り出す (Right なら空文字)
errMsgOf :: Either SpinorError a -> Text
errMsgOf (Left e)  = errorMsg e
errMsgOf (Right _) = ""

-- | シンボル参照式のショートカット
sym :: Text -> Expr
sym = ESym dummySpan

spec :: Spec
spec = do
  describe "Spinor.Infer リソース消費トラッキング (Issue #66)" $ do

    describe "変数ルックアップ時の消費" $ do
      it "線形変数を 1 回だけ使用すれば成功する" $
        runInfer (infer linearEnv (sym "x")) `shouldSatisfy` isRight

      it "消費後の返却 env から線形変数が削除されている" $
        case runInfer (infer linearEnv (sym "x")) of
          Right (_, _, envAfter) -> Map.member "x" envAfter `shouldBe` False
          Left e                 -> expectationFailure (show e)

    describe "Move エラー (use-after-move)" $
      it "同じ線形変数を 2 回使用するとコンパイルエラーになる" $ do
        let r = runInfer $ do
                  (_, _, envAfter) <- infer linearEnv (sym "x")
                  infer envAfter (sym "x")
        errMsgOf r `shouldSatisfy` ("use-after-move" `isInfixOf`)

    describe "Resource Leak エラー" $ do
      it "let で束縛した線形変数を消費せずスコープを抜けるとエラーになる" $ do
        let expr = ELet dummySpan [("y", sym "x")] (EInt dummySpan 42)
        errMsgOf (runInfer (infer linearEnv expr))
          `shouldSatisfy` ("resource leak" `isInfixOf`)

      it "let で束縛した線形変数を body で消費すればエラーにならない" $ do
        let expr = ELet dummySpan [("y", sym "x")] (sym "y")
        runInfer (infer linearEnv expr) `shouldSatisfy` isRight

  describe "Spinor.Infer 所有権・借用セマンティクス (Issue #72)" $ do

    describe "借用 (&expr) は消費しない" $ do
      it "&x は x をムーブしないので、その後も x を使用できる" $ do
        -- 借用の返却 env を引き回して x を再度使用 → 成功 (消費されていない)
        let r = runInfer $ do
                  (_, _, envAfter) <- infer linearEnv (EBorrow dummySpan (sym "x"))
                  infer envAfter (sym "x")
        r `shouldSatisfy` isRight

      it "&x の型は借用参照型 (TBorrow _) になる" $
        case runInfer (infer linearEnv (EBorrow dummySpan (sym "x"))) of
          Right (_, TBorrow _, _) -> pure ()
          other                   -> expectationFailure ("expected TBorrow, got: " <> show other)

      it "借用後の返却 env に x が残っている" $
        case runInfer (infer linearEnv (EBorrow dummySpan (sym "x"))) of
          Right (_, _, envAfter) -> Map.member "x" envAfter `shouldBe` True
          Left e                 -> expectationFailure (show e)

    describe "ムーブ (@expr) は消費する" $
      it "@x は x をムーブするので、その後 x を使うと use-after-move になる" $ do
        let r = runInfer $ do
                  (_, _, envAfter) <- infer linearEnv (EMove dummySpan (sym "x"))
                  infer envAfter (sym "x")
        errMsgOf r `shouldSatisfy` ("use-after-move" `isInfixOf`)

    describe "参照解決 (*expr)" $ do
      it "*(&x) は借用型を 1 層剥がして中身の型を返す" $
        -- x :: TLinear Linear TInt なので &x :: TBorrow (TLinear Linear TInt)、
        -- *(&x) は借用層のみを剥がして TLinear Linear TInt を返す。
        case runInfer (infer linearEnv (EDeref dummySpan (EBorrow dummySpan (sym "x")))) of
          Right (_, t, _) -> t `shouldBe` TLinear Linear TInt
          Left e          -> expectationFailure (show e)

      it "*(&x) は x を消費しない (借用経由のため)" $
        case runInfer (infer linearEnv (EDeref dummySpan (EBorrow dummySpan (sym "x")))) of
          Right (_, _, envAfter) -> Map.member "x" envAfter `shouldBe` True
          Left e                 -> expectationFailure (show e)

      it "借用でない値の参照解決は型エラーになる" $
        -- *x : x は Int であり &_ ではないので unify に失敗する
        runInfer (infer linearEnv (EDeref dummySpan (sym "x")))
          `shouldSatisfy` either (const True) (const False)

    describe "隔離ブロック (unsafe expr)" $
      it "unsafe 内での線形変数の消費は外部に漏れない" $ do
        -- (unsafe x) は内部で x を参照するが、消費は隔離される
        let r = runInfer $ do
                  (_, _, envAfter) <- infer linearEnv (EUnsafe dummySpan (sym "x"))
                  infer envAfter (sym "x")   -- 消費が漏れていなければ成功
        r `shouldSatisfy` isRight
