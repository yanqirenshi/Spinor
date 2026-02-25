{-# LANGUAGE OverloadedStrings #-}

module Spinor.JsonSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Spinor.Val (Val(..))
import Spinor.Library.Json (jsonParse, jsonStringify)

spec :: Spec
spec = describe "Spinor.Library.Json (JSON Support)" $ do

  describe "json-parse (JSON -> Spinor)" $ do
    describe "基本型" $ do
      it "整数をパース" $
        jsonParse "42" `shouldBe` Right (VInt 42)

      it "負の整数をパース" $
        jsonParse "-123" `shouldBe` Right (VInt (-123))

      it "浮動小数点数をパース" $
        jsonParse "3.14" `shouldBe` Right (VFloat 3.14)

      it "文字列をパース" $
        jsonParse "\"hello\"" `shouldBe` Right (VStr "hello")

      it "真偽値 true をパース" $
        jsonParse "true" `shouldBe` Right (VBool True)

      it "真偽値 false をパース" $
        jsonParse "false" `shouldBe` Right (VBool False)

      it "null をパース" $
        jsonParse "null" `shouldBe` Right VNil

    describe "配列" $ do
      it "空配列をパース" $
        jsonParse "[]" `shouldBe` Right (VList [])

      it "数値の配列をパース" $
        jsonParse "[1, 2, 3]" `shouldBe` Right (VList [VInt 1, VInt 2, VInt 3])

      it "混合型の配列をパース" $
        jsonParse "[1, \"two\", true]" `shouldBe` Right (VList [VInt 1, VStr "two", VBool True])

      it "ネストした配列をパース" $
        jsonParse "[[1, 2], [3, 4]]" `shouldBe`
          Right (VList [VList [VInt 1, VInt 2], VList [VInt 3, VInt 4]])

    describe "オブジェクト (Alist への変換)" $ do
      it "空オブジェクトをパース" $
        jsonParse "{}" `shouldBe` Right (VList [])

      it "単純なオブジェクトをパース" $ do
        let result = jsonParse "{\"a\": 1}"
        case result of
          Right (VList [(VList [VStr "a", VInt 1])]) -> pure ()
          _ -> expectationFailure $ "Expected Alist, got: " ++ show result

      it "複数キーのオブジェクトをパース" $ do
        let result = jsonParse "{\"name\": \"Alice\", \"age\": 30}"
        case result of
          Right (VList pairs) -> length pairs `shouldBe` 2
          _ -> expectationFailure $ "Expected Alist with 2 pairs, got: " ++ show result

      it "ネストしたオブジェクトをパース" $ do
        let result = jsonParse "{\"user\": {\"name\": \"Bob\"}}"
        case result of
          Right (VList [(VList [VStr "user", VList [(VList [VStr "name", VStr "Bob"])]])]) -> pure ()
          _ -> expectationFailure $ "Expected nested Alist, got: " ++ show result

    describe "エラー処理" $ do
      it "不正な JSON はエラー" $
        case jsonParse "{invalid}" of
          Left _ -> pure ()
          Right _ -> expectationFailure "Should fail on invalid JSON"

      it "閉じていないブラケットはエラー" $
        case jsonParse "[1, 2" of
          Left _ -> pure ()
          Right _ -> expectationFailure "Should fail on unclosed bracket"

  describe "json-stringify (Spinor -> JSON)" $ do
    describe "基本型" $ do
      it "整数を文字列化" $
        jsonStringify (VInt 42) `shouldBe` Right "42"

      it "浮動小数点数を文字列化" $
        jsonStringify (VFloat 3.14) `shouldBe` Right "3.14"

      it "文字列を文字列化" $
        jsonStringify (VStr "hello") `shouldBe` Right "\"hello\""

      it "真偽値 #t を文字列化" $
        jsonStringify (VBool True) `shouldBe` Right "true"

      it "真偽値 #f を文字列化" $
        jsonStringify (VBool False) `shouldBe` Right "false"

      it "nil を文字列化" $
        jsonStringify VNil `shouldBe` Right "null"

      it "シンボルを文字列として文字列化" $
        jsonStringify (VSym "foo") `shouldBe` Right "\"foo\""

    describe "配列" $ do
      it "空リストを文字列化" $
        jsonStringify (VList []) `shouldBe` Right "[]"

      it "数値のリストを文字列化" $
        jsonStringify (VList [VInt 1, VInt 2, VInt 3]) `shouldBe` Right "[1,2,3]"

      it "混合型のリストを文字列化" $
        jsonStringify (VList [VInt 1, VStr "two", VBool True]) `shouldBe` Right "[1,\"two\",true]"

    describe "オブジェクト (Alist から)" $ do
      it "Alist をオブジェクトとして文字列化" $ do
        let alist = VList [VList [VStr "a", VInt 1]]
        jsonStringify alist `shouldBe` Right "{\"a\":1}"

      it "複数キーの Alist をオブジェクトとして文字列化" $ do
        let alist = VList [VList [VStr "x", VInt 10], VList [VStr "y", VInt 20]]
        case jsonStringify alist of
          Right str -> do
            -- キーの順序は保証されないため、両方のキーが含まれることを確認
            str `shouldSatisfy` (\s -> T.isInfixOf "\"x\":10" s && T.isInfixOf "\"y\":20" s)
          Left err -> expectationFailure $ "Should succeed, got: " ++ show err

      it "ネストした Alist をオブジェクトとして文字列化" $ do
        let alist = VList [VList [VStr "data", VList [VList [VStr "value", VInt 42]]]]
        case jsonStringify alist of
          Right str -> str `shouldSatisfy` T.isInfixOf "\"data\":{"
          Left err -> expectationFailure $ "Should succeed, got: " ++ show err

    describe "エラー処理" $ do
      it "VData は変換不可" $
        case jsonStringify (VData "Maybe" [VInt 42]) of
          Left err -> err `shouldSatisfy` T.isInfixOf "VData"
          Right _ -> expectationFailure "Should fail on VData"

  describe "ラウンドトリップ" $ do
    it "整数のラウンドトリップ" $ do
      let original = VInt 42
      case jsonStringify original of
        Right str -> jsonParse str `shouldBe` Right original
        Left err -> expectationFailure $ show err

    it "文字列のラウンドトリップ" $ do
      let original = VStr "hello world"
      case jsonStringify original of
        Right str -> jsonParse str `shouldBe` Right original
        Left err -> expectationFailure $ show err

    it "配列のラウンドトリップ" $ do
      let original = VList [VInt 1, VInt 2, VInt 3]
      case jsonStringify original of
        Right str -> jsonParse str `shouldBe` Right original
        Left err -> expectationFailure $ show err

    it "Alist のラウンドトリップ" $ do
      let original = VList [VList [VStr "key", VInt 123]]
      case jsonStringify original of
        Right str -> jsonParse str `shouldBe` Right original
        Left err -> expectationFailure $ show err
