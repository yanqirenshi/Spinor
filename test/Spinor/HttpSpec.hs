{-# LANGUAGE OverloadedStrings #-}

module Spinor.HttpSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Spinor.Val (Val(..))
import Spinor.Core.Http (httpPrimitives)

spec :: Spec
spec = describe "Spinor.Core.Http (HTTP Client)" $ do

  describe "httpPrimitives" $ do
    it "core-http-request プリミティブが登録されている" $ do
      let names = map fst httpPrimitives
      names `shouldContain` ["core-http-request"]

  describe "coreHttpRequest 引数検証" $ do
    it "引数が不足している場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "core-http-request" httpPrimitives
      case fn [] of
        Left err -> err `shouldSatisfy` T.isInfixOf "引数が不正"
        Right _ -> expectationFailure "Should fail with no arguments"

    it "引数が1つだけの場合はエラー" $ do
      let Just (VPrim _ fn) = lookup "core-http-request" httpPrimitives
      case fn [VStr "GET"] of
        Left err -> err `shouldSatisfy` T.isInfixOf "引数が不正"
        Right _ -> expectationFailure "Should fail with 1 argument"

  -- 以下は curl が利用可能な環境でのみ実行される統合テスト
  -- CI 環境では skip することを推奨
  describe "HTTP リクエスト (統合テスト - curl 必須)" $ do
    it "GET リクエストがレスポンスを返す" $ do
      let Just (VPrim _ fn) = lookup "core-http-request" httpPrimitives
      case fn [VStr "GET", VStr "https://httpbin.org/get", VNil, VNil] of
        Right (VList response) -> do
          -- レスポンス構造を検証
          length response `shouldBe` 3
          -- status-code があることを確認
          case head response of
            VList [VSym ":status-code", VInt code] ->
              code `shouldSatisfy` (>= 200)
            other -> expectationFailure $ "Expected status-code, got: " ++ show other
        Right other -> expectationFailure $ "Expected VList, got: " ++ show other
        Left err -> expectationFailure $ "HTTP request failed: " ++ T.unpack err

    it "POST リクエストがボディを送信できる" $ do
      let Just (VPrim _ fn) = lookup "core-http-request" httpPrimitives
          headers = VList [VList [VSym ":Content-Type", VStr "application/json"]]
          body = VStr "{\"test\": true}"
      case fn [VStr "POST", VStr "https://httpbin.org/post", headers, body] of
        Right (VList response) -> do
          length response `shouldBe` 3
          case head response of
            VList [VSym ":status-code", VInt code] ->
              code `shouldSatisfy` (>= 200)
            other -> expectationFailure $ "Expected status-code, got: " ++ show other
        Right other -> expectationFailure $ "Expected VList, got: " ++ show other
        Left err -> expectationFailure $ "HTTP request failed: " ++ T.unpack err

    it "レスポンスボディを取得できる" $ do
      let Just (VPrim _ fn) = lookup "core-http-request" httpPrimitives
      case fn [VStr "GET", VStr "https://httpbin.org/html", VNil, VNil] of
        Right (VList response) -> do
          -- body を取得
          let bodyEntry = response !! 2
          case bodyEntry of
            VList [VSym ":body", VStr bodyContent] ->
              bodyContent `shouldSatisfy` T.isInfixOf "html"
            other -> expectationFailure $ "Expected body, got: " ++ show other
        Right other -> expectationFailure $ "Expected VList, got: " ++ show other
        Left err -> expectationFailure $ "HTTP request failed: " ++ T.unpack err
