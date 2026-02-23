{-# LANGUAGE OverloadedStrings #-}

module Spinor.Primitive
  ( primitiveBindings
  ) where

import Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra as LA
import Control.Exception (SomeException, try, evaluate)
import System.IO.Unsafe (unsafePerformIO)

import Spinor.Val (Val(..), Env)

-- | 初期環境: プリミティブ関数を束縛した Env
primitiveBindings :: Env
primitiveBindings = Map.fromList
  [ ("+",  VPrim "+"  $ numBinOp (+))
  , ("-",  VPrim "-"  $ numBinOp (-))
  , ("*",  VPrim "*"  $ numBinOp (*))
  , ("%",  VPrim "%"  $ numBinOp mod)
  , ("=",  VPrim "="  numEq)
  , ("<",  VPrim "<"  $ numCmp (<))
  , (">",  VPrim ">"  $ numCmp (>))
  , ("cons",  VPrim "cons"  primCons)
  , ("car",   VPrim "car"   primCar)
  , ("cdr",   VPrim "cdr"   primCdr)
  , ("list",  VPrim "list"  primList)
  , ("null?",  VPrim "null?"  primNull)
  , ("empty?", VPrim "empty?" primNull)
  , ("eq",     VPrim "eq"     primEq)
  , ("equal",  VPrim "equal"  primEqual)
  -- 文字列操作
  , ("string-append", VPrim "string-append" primStringAppend)
  , ("string-length", VPrim "string-length" primStringLength)
  , ("substring",     VPrim "substring"     primSubstring)
  , ("string=?",      VPrim "string=?"      primStringEq)
  , ("string->list",  VPrim "string->list"  primStringToList)
  , ("list->string",  VPrim "list->string"  primListToString)
  -- 行列操作
  , ("matrix",    VPrim "matrix"    primMatrix)
  , ("mdim",      VPrim "mdim"     primMdim)
  , ("mref",      VPrim "mref"     primMref)
  , ("m+",        VPrim "m+"       primMAdd)
  , ("m*",        VPrim "m*"       primMMul)
  , ("transpose", VPrim "transpose" primMTranspose)
  , ("inverse",   VPrim "inverse"  primMInverse)
  ]

-- | 整数の二項演算をラップするヘルパー
numBinOp :: (Integer -> Integer -> Integer) -> [Val] -> Either Text Val
numBinOp op [VInt a, VInt b] = Right $ VInt (op a b)
numBinOp _  [_, _]           = Left "算術演算には整数が必要です"
numBinOp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                    <> tshow (length args) <> ")"

-- | 等値比較 (整数・真偽値対応)
numEq :: [Val] -> Either Text Val
numEq [VInt a, VInt b]   = Right $ VBool (a == b)
numEq [VBool a, VBool b] = Right $ VBool (a == b)
numEq [_, _]             = Left "= には整数または真偽値が必要です"
numEq args               = Left $ "引数の数が不正です (期待: 2, 実際: "
                                <> tshow (length args) <> ")"

-- | 整数の比較演算をラップするヘルパー
numCmp :: (Integer -> Integer -> Bool) -> [Val] -> Either Text Val
numCmp op [VInt a, VInt b] = Right $ VBool (op a b)
numCmp _  [_, _]           = Left "比較演算には整数が必要です"
numCmp _  args             = Left $ "引数の数が不正です (期待: 2, 実際: "
                                  <> tshow (length args) <> ")"

-- | cons: 値をリストの先頭に追加
primCons :: [Val] -> Either Text Val
primCons [val, VList xs] = Right $ VList (val : xs)
primCons [val, VNil]     = Right $ VList [val]
primCons [_, _]          = Left "cons の第2引数にはリストが必要です"
primCons args            = Left $ "cons: 引数の数が不正です (期待: 2, 実際: "
                                <> tshow (length args) <> ")"

-- | car: リストの先頭要素を返す
primCar :: [Val] -> Either Text Val
primCar [VList (x:_)] = Right x
primCar [VList []]     = Left "car: 空リストに対しては適用できません"
primCar [VNil]         = Left "car: nil に対しては適用できません"
primCar [_]            = Left "car: リストが必要です"
primCar args           = Left $ "car: 引数の数が不正です (期待: 1, 実際: "
                              <> tshow (length args) <> ")"

-- | cdr: リストの先頭以外を返す
primCdr :: [Val] -> Either Text Val
primCdr [VList (_:xs)] = Right $ VList xs
primCdr [VList []]     = Left "cdr: 空リストに対しては適用できません"
primCdr [VNil]         = Left "cdr: nil に対しては適用できません"
primCdr [_]            = Left "cdr: リストが必要です"
primCdr args           = Left $ "cdr: 引数の数が不正です (期待: 1, 実際: "
                              <> tshow (length args) <> ")"

-- | list: 引数をリストにまとめる (可変長引数)
primList :: [Val] -> Either Text Val
primList args = Right $ VList args

-- | null?: リストが空かどうかを判定
primNull :: [Val] -> Either Text Val
primNull [VList []] = Right $ VBool True
primNull [VNil]     = Right $ VBool True
primNull [_]        = Right $ VBool False
primNull args       = Left $ "null?: 引数の数が不正です (期待: 1, 実際: "
                           <> tshow (length args) <> ")"

-- | eq: 実装レベルでの同一性比較
--   アトム (VInt, VBool, VStr, VSym, VNil) は値で比較
--   その他 (VList, VData, VFunc, VMacro, VMVar 等) は常に False
primEq :: [Val] -> Either Text Val
primEq [a, b] = Right $ VBool (eqVal a b)
  where
    eqVal (VInt  x) (VInt  y) = x == y
    eqVal (VBool x) (VBool y) = x == y
    eqVal (VStr  x) (VStr  y) = x == y
    eqVal (VSym  x) (VSym  y) = x == y
    eqVal VNil      VNil      = True
    eqVal _         _         = False  -- リスト、データ、関数等は常に不等
primEq args = Left $ "eq: 引数の数が不正です (期待: 2, 実際: " <> tshow (length args) <> ")"

-- | equal: 構造的等価性比較
--   Val の Eq インスタンス (==) を使用して再帰的に比較
primEqual :: [Val] -> Either Text Val
primEqual [a, b] = Right $ VBool (a == b)
primEqual args = Left $ "equal: 引数の数が不正です (期待: 2, 実際: " <> tshow (length args) <> ")"

-- | string-append: 複数の文字列を連結する (可変長引数)
primStringAppend :: [Val] -> Either Text Val
primStringAppend args = case traverse getStr args of
    Just strs -> Right $ VStr (T.concat strs)
    Nothing   -> Left "string-append: 全ての引数は文字列である必要があります"
  where
    getStr (VStr s) = Just s
    getStr _        = Nothing

-- | string-length: 文字列の長さ (文字数) を返す
primStringLength :: [Val] -> Either Text Val
primStringLength [VStr s] = Right $ VInt (fromIntegral $ T.length s)
primStringLength [_]      = Left "string-length: 文字列が必要です"
primStringLength args     = Left $ "string-length: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"

-- | substring: 部分文字列を取得 (0-indexed, [start, end) の範囲)
primSubstring :: [Val] -> Either Text Val
primSubstring [VStr s, VInt start, VInt end]
    | start < 0 || end < start = Right $ VStr ""
    | otherwise = Right $ VStr $ T.take (fromIntegral $ end - start) $ T.drop (fromIntegral start) s
primSubstring [_, _, _] = Left "substring: (String, Int, Int) が必要です"
primSubstring args = Left $ "substring: 引数の数が不正です (期待: 3, 実際: " <> tshow (length args) <> ")"

-- | string=?: 文字列の等価判定
primStringEq :: [Val] -> Either Text Val
primStringEq [VStr a, VStr b] = Right $ VBool (a == b)
primStringEq [_, _]           = Left "string=?: 文字列が必要です"
primStringEq args             = Left $ "string=?: 引数の数が不正です (期待: 2, 実際: " <> tshow (length args) <> ")"

-- | string->list: 文字列を1文字ずつのリストに変換
primStringToList :: [Val] -> Either Text Val
primStringToList [VStr s] = Right $ VList (map (VStr . T.singleton) (T.unpack s))
primStringToList [_]      = Left "string->list: 文字列が必要です"
primStringToList args     = Left $ "string->list: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"

-- | list->string: 文字列のリストを連結して1つの文字列に
primListToString :: [Val] -> Either Text Val
primListToString [VList vs] = case traverse getStr vs of
    Just strs -> Right $ VStr (T.concat strs)
    Nothing   -> Left "list->string: リストの全要素は文字列である必要があります"
  where
    getStr (VStr s) = Just s
    getStr _        = Nothing
primListToString [_]   = Left "list->string: リストが必要です"
primListToString args  = Left $ "list->string: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"

tshow :: Show a => a -> Text
tshow = pack . show

-- ===========================================================================
-- 行列操作プリミティブ
-- ===========================================================================

-- | matrix: 行列を生成する
--   (matrix rows cols elements) -> VMatrix
--   elements は VInt または VFloat のリスト (VInt は Double に変換)
primMatrix :: [Val] -> Either Text Val
primMatrix [VInt rows, VInt cols, VList elements]
    | rows <= 0 || cols <= 0 = Left "matrix: 行数・列数は正の整数である必要があります"
    | expectedLen /= actualLen = Left $ "matrix: 要素数が不正です (期待: "
                                      <> tshow expectedLen <> ", 実際: "
                                      <> tshow actualLen <> ")"
    | otherwise = case traverse toDouble elements of
        Just doubles -> Right $ VMatrix (fromIntegral rows) (fromIntegral cols) (VS.fromList doubles)
        Nothing -> Left "matrix: 全ての要素は数値 (Int または Float) である必要があります"
  where
    expectedLen = fromIntegral (rows * cols) :: Int
    actualLen = length elements
    toDouble (VInt n)   = Just (fromIntegral n :: Double)
    toDouble (VFloat f) = Just f
    toDouble _          = Nothing
primMatrix [_, _, _] = Left "matrix: (Int, Int, List) が必要です"
primMatrix args = Left $ "matrix: 引数の数が不正です (期待: 3, 実際: " <> tshow (length args) <> ")"

-- | mdim: 行列の次元を返す
--   (mdim m) -> (rows cols)
primMdim :: [Val] -> Either Text Val
primMdim [VMatrix rows cols _] = Right $ VList [VInt (fromIntegral rows), VInt (fromIntegral cols)]
primMdim [_] = Left "mdim: 行列が必要です"
primMdim args = Left $ "mdim: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"

-- | mref: 行列の要素を参照する (0-indexed)
--   (mref m r c) -> Double
primMref :: [Val] -> Either Text Val
primMref [VMatrix rows cols vec, VInt r, VInt c]
    | r < 0 || r >= fromIntegral rows = Left $ "mref: 行インデックスが範囲外です (0-" <> tshow (rows - 1) <> ")"
    | c < 0 || c >= fromIntegral cols = Left $ "mref: 列インデックスが範囲外です (0-" <> tshow (cols - 1) <> ")"
    | otherwise = Right $ VFloat (vec VS.! (fromIntegral r * cols + fromIntegral c))
primMref [_, _, _] = Left "mref: (Matrix, Int, Int) が必要です"
primMref args = Left $ "mref: 引数の数が不正です (期待: 3, 実際: " <> tshow (length args) <> ")"

-- ===========================================================================
-- hmatrix 連携 (BLAS/LAPACK)
-- ===========================================================================

-- | VMatrix → hmatrix の Matrix Double へ変換
toLA :: Int -> Int -> VS.Vector Double -> LA.Matrix Double
toLA r c vec = (r LA.>< c) (VS.toList vec)

-- | hmatrix の Matrix Double → VMatrix の構成要素へ変換
fromLA :: LA.Matrix Double -> Val
fromLA m = VMatrix (LA.rows m) (LA.cols m) (LA.flatten m)

-- | m+: 行列の要素ごとの加算
--   (m+ a b) -> Matrix
--   a と b の行数・列数が完全に一致している必要がある
primMAdd :: [Val] -> Either Text Val
primMAdd [VMatrix r1 c1 v1, VMatrix r2 c2 v2]
    | r1 /= r2 || c1 /= c2 = Left $ "m+: 行列の次元が一致しません ("
                                   <> tshow r1 <> "x" <> tshow c1 <> " vs "
                                   <> tshow r2 <> "x" <> tshow c2 <> ")"
    | otherwise = Right $ fromLA (toLA r1 c1 v1 + toLA r2 c2 v2)
primMAdd [_, _] = Left "m+: 両方の引数に行列が必要です"
primMAdd args = Left $ "m+: 引数の数が不正です (期待: 2, 実際: " <> tshow (length args) <> ")"

-- | m*: 行列積
--   (m* a b) -> Matrix
--   a の列数と b の行数が一致している必要がある
primMMul :: [Val] -> Either Text Val
primMMul [VMatrix r1 c1 v1, VMatrix r2 c2 v2]
    | c1 /= r2 = Left $ "m*: 行列積の次元が不正です (左の列数 "
                       <> tshow c1 <> " ≠ 右の行数 " <> tshow r2 <> ")"
    | otherwise = Right $ fromLA (toLA r1 c1 v1 LA.<> toLA r2 c2 v2)
primMMul [_, _] = Left "m*: 両方の引数に行列が必要です"
primMMul args = Left $ "m*: 引数の数が不正です (期待: 2, 実際: " <> tshow (length args) <> ")"

-- | transpose: 行列の転置
--   (transpose m) -> Matrix
primMTranspose :: [Val] -> Either Text Val
primMTranspose [VMatrix r c vec] = Right $ fromLA (LA.tr (toLA r c vec))
primMTranspose [_] = Left "transpose: 行列が必要です"
primMTranspose args = Left $ "transpose: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"

-- | inverse: 正方行列の逆行列
--   (inverse m) -> Matrix
--   行列が正方でない場合、または特異行列の場合はエラー
primMInverse :: [Val] -> Either Text Val
primMInverse [VMatrix r c vec]
    | r /= c = Left $ "inverse: 正方行列が必要です (" <> tshow r <> "x" <> tshow c <> ")"
    | otherwise = case tryInv (toLA r c vec) of
        Right result -> Right (fromLA result)
        Left msg     -> Left msg
  where
    tryInv :: LA.Matrix Double -> Either Text (LA.Matrix Double)
    tryInv m = unsafePerformIO $ do
        result <- try (evaluate (LA.inv m)) :: IO (Either SomeException (LA.Matrix Double))
        pure $ case result of
            Right invM -> Right invM
            Left _     -> Left "inverse: 特異行列のため逆行列を計算できません"
primMInverse [_] = Left "inverse: 行列が必要です"
primMInverse args = Left $ "inverse: 引数の数が不正です (期待: 1, 実際: " <> tshow (length args) <> ")"
