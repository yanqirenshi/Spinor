{-# LANGUAGE OverloadedStrings #-}

module Spinor.RegionSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

import Spinor.Syntax (Expr(..), readExpr, dummySpan)
import Spinor.EscapeAnalysis
import Spinor.Compiler.Codegen (compileProgramWithRegions, compileExpr)

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
  describe "Region Syntax Parsing" $ do
    it "with-region を正しくパースできる" $ do
      let expr = parseExpr "(with-region r (+ 1 2))"
      case expr of
        EWithRegion _ regionName _ -> regionName `shouldBe` "r"
        _ -> expectationFailure "Expected EWithRegion"

    it "alloc-in を正しくパースできる" $ do
      let expr = parseExpr "(alloc-in r 42)"
      case expr of
        EAllocIn _ regionName _ -> regionName `shouldBe` "r"
        _ -> expectationFailure "Expected EAllocIn"

    it "入れ子の with-region をパースできる" $ do
      let expr = parseExpr "(with-region outer (with-region inner 42))"
      case expr of
        EWithRegion _ "outer" (EWithRegion _ "inner" _) -> pure ()
        _ -> expectationFailure "Expected nested EWithRegion"

  describe "EscapeAnalysis" $ do
    describe "基本的な逃避チェック" $ do
      it "リージョン内での通常の使用はエラーなし" $ do
        let expr = EWithRegion dummySpan "r"
              (EAllocIn dummySpan "r" (EInt dummySpan 42))
            result = checkEscape [expr]
        erErrors result `shouldBe` []

      it "使用されたリージョン名が記録される" $ do
        let expr = EWithRegion dummySpan "myregion"
              (EInt dummySpan 1)
            result = checkEscape [expr]
        Set.member "myregion" (erRegions result) `shouldBe` True

      it "未定義リージョンへの alloc-in はエラー" $ do
        let expr = EAllocIn dummySpan "undefined_region" (EInt dummySpan 42)
            result = checkEscape [expr]
        length (erErrors result) `shouldBe` 1
        case head (erErrors result) of
          UndefinedRegion name _ -> name `shouldBe` "undefined_region"
          _ -> expectationFailure "Expected UndefinedRegion error"

    describe "エラーメッセージのフォーマット" $ do
      it "EscapeReturn エラーが正しくフォーマットされる" $ do
        let err = EscapeReturn "x" "r" dummySpan
        formatEscapeError err `shouldBe`
          "Value 'x' from region 'r' cannot escape (returned from function)"

      it "EscapeAssign エラーが正しくフォーマットされる" $ do
        let err = EscapeAssign "outer" "inner" "r" dummySpan
        formatEscapeError err `shouldBe`
          "Value 'inner' from region 'r' cannot be assigned to external variable 'outer'"

      it "UndefinedRegion エラーが正しくフォーマットされる" $ do
        let err = UndefinedRegion "r" dummySpan
        formatEscapeError err `shouldBe`
          "Region 'r' is not defined in current scope"

  describe "Codegen - Region Support" $ do
    it "with-region が C のブロックスコープに展開される" $ do
      let expr = EWithRegion dummySpan "r" (EInt dummySpan 42)
          code = compileExpr expr
      -- GCC statement expression を使用
      T.isInfixOf "create_region()" code `shouldBe` True
      T.isInfixOf "destroy_region(" code `shouldBe` True

    it "alloc-in が sp_region_make_int を生成する" $ do
      let expr = EAllocIn dummySpan "r" (EInt dummySpan 42)
          code = compileExpr expr
      T.isInfixOf "sp_region_make_int" code `shouldBe` True

    it "alloc-in が sp_region_make_str を生成する" $ do
      let expr = EAllocIn dummySpan "r" (EStr dummySpan "hello")
          code = compileExpr expr
      T.isInfixOf "sp_region_make_str" code `shouldBe` True

    it "compileProgramWithRegions が Arena アロケータコードを含む" $ do
      let exprs = [EInt dummySpan 1]
          result = checkEscape exprs
          code = compileProgramWithRegions exprs result
      -- Arena アロケータの構造体と関数が含まれる
      T.isInfixOf "typedef struct RegionBlock" code `shouldBe` True
      T.isInfixOf "Region* create_region(void)" code `shouldBe` True
      T.isInfixOf "void* region_alloc(Region* r, size_t size)" code `shouldBe` True
      T.isInfixOf "void destroy_region(Region* r)" code `shouldBe` True
