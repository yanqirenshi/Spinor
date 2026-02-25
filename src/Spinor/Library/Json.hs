{-# LANGUAGE OverloadedStrings #-}

module Spinor.Library.Json
  ( spinorToAeson
  , aesonToSpinor
  , jsonParse
  , jsonStringify
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Scientific as Sci
import qualified Data.Vector as V

import Spinor.Val (Val(..))

-- | Spinor の Val を Aeson の Value に変換する
--   変換不可能な型 (VFunc, VMacro, VPrim, VMVar など) はエラーを返す
spinorToAeson :: Val -> Either Text Aeson.Value
spinorToAeson (VInt n)    = Right $ Aeson.Number (fromIntegral n)
spinorToAeson (VFloat f)  = Right $ Aeson.Number (Sci.fromFloatDigits f)
spinorToAeson (VBool b)   = Right $ Aeson.Bool b
spinorToAeson (VStr s)    = Right $ Aeson.String s
spinorToAeson VNil        = Right Aeson.Null
spinorToAeson (VSym s)    = Right $ Aeson.String s  -- シンボルは文字列として変換
spinorToAeson (VList [])  = Right $ Aeson.Array V.empty

-- Alist (連想リスト) の検出とオブジェクトへの変換
-- Alist: すべての要素が (key value) の形式のリスト
spinorToAeson (VList xs)
  | isAlist xs = do
      pairs <- traverse toPair xs
      Right $ Aeson.Object (AKM.fromList pairs)
  | otherwise = do
      vals <- traverse spinorToAeson xs
      Right $ Aeson.Array (V.fromList vals)
  where
    -- Alist かどうかをチェック
    isAlist :: [Val] -> Bool
    isAlist = all isPair

    isPair :: Val -> Bool
    isPair (VList [VStr _, _]) = True
    isPair _                    = False

    -- Alist の各ペアを Aeson のキー・値ペアに変換
    toPair :: Val -> Either Text (Aeson.Key, Aeson.Value)
    toPair (VList [VStr k, v]) = do
      v' <- spinorToAeson v
      Right (AK.fromText k, v')
    toPair _ = Left "json-stringify: Alist の要素は (\"key\" value) の形式である必要があります"

-- 変換不可能な型
spinorToAeson (VData name _)  = Left $ "json-stringify: VData (" <> name <> ") は JSON に変換できません"
spinorToAeson (VFunc _ _ _)   = Left "json-stringify: 関数は JSON に変換できません"
spinorToAeson (VMacro _ _ _)  = Left "json-stringify: マクロは JSON に変換できません"
spinorToAeson (VPrim name _)  = Left $ "json-stringify: プリミティブ関数 (" <> name <> ") は JSON に変換できません"
spinorToAeson (VMVar _)       = Left "json-stringify: MVar は JSON に変換できません"
spinorToAeson (VMatrix _ _ _) = Left "json-stringify: 行列は JSON に変換できません"
spinorToAeson (VCLContext _ _)= Left "json-stringify: CLContext は JSON に変換できません"
spinorToAeson (VCLBuffer _ _) = Left "json-stringify: CLBuffer は JSON に変換できません"
spinorToAeson (VCLKernel _ _) = Left "json-stringify: CLKernel は JSON に変換できません"
spinorToAeson (VWindow _)     = Left "json-stringify: Window は JSON に変換できません"

-- | Aeson の Value を Spinor の Val に変換する
--   JSON -> Spinor の変換は常に成功する
aesonToSpinor :: Aeson.Value -> Val
aesonToSpinor Aeson.Null       = VNil
aesonToSpinor (Aeson.Bool b)   = VBool b
aesonToSpinor (Aeson.String s) = VStr s
aesonToSpinor (Aeson.Number n) =
  -- 整数として表現可能な場合は VInt、そうでなければ VFloat
  case Sci.floatingOrInteger n of
    Left d  -> VFloat d
    Right i -> VInt i
aesonToSpinor (Aeson.Array arr) =
  VList (V.toList (V.map aesonToSpinor arr))
aesonToSpinor (Aeson.Object obj) =
  -- JSON オブジェクトを Alist に変換
  -- { "a": 1, "b": 2 } -> (("a" 1) ("b" 2))
  VList (map toPair (AKM.toList obj))
  where
    toPair (k, v) = VList [VStr (AK.toText k), aesonToSpinor v]

-- | JSON 文字列をパースして Spinor の Val を返す
jsonParse :: Text -> Either Text Val
jsonParse input =
  case Aeson.eitherDecode (TLE.encodeUtf8 (TL.fromStrict input)) of
    Left err  -> Left $ "json-parse: " <> T.pack err
    Right val -> Right (aesonToSpinor val)

-- | Spinor の Val を JSON 文字列に変換する
jsonStringify :: Val -> Either Text Text
jsonStringify val = do
  aeson <- spinorToAeson val
  Right $ TL.toStrict (TLE.decodeUtf8 (Aeson.encode aeson))
