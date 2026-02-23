{-# LANGUAGE OverloadedStrings #-}

module Spinor.Val
  ( Val(..)
  , Env
  , showVal
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar)
import qualified Data.Vector.Storable as VS
import Foreign.Ptr (Ptr)
import qualified Graphics.UI.GLFW as GLFW

import Spinor.Syntax (Expr)

-- | 変数環境: シンボル名 → 値
--   Val と Env は相互に参照するため同一モジュールに定義する。
type Env = Map.Map Text Val

-- | 評価結果の値型
--   Lisp の値を Haskell の代数的データ型にマッピングする。
--   Expr (構文) と Val (値) を分離することで、評価前後の区別を型レベルで保証する。
data Val
  = VInt  Integer                              -- 整数値
  | VFloat Double                              -- 浮動小数点数
  | VBool Bool                                 -- 真偽値
  | VPrim Text ([Val] -> Either Text Val)      -- プリミティブ関数 (名前, 実装)
  | VFunc  [Text] Expr Env                     -- ユーザー定義関数 (引数名, 本体, クロージャ環境)
  | VMacro [Text] Expr Env                     -- マクロ (引数名, 本体, クロージャ環境)
  | VList [Val]                                -- リスト
  | VNil                                       -- 空リスト / nil
  | VSym  Text                                 -- シンボル (quote 用)
  | VStr  Text                                 -- 文字列
  | VData Text [Val]                           -- データコンストラクタ (名前, フィールド値)
  | VMVar (MVar Val)                           -- 同期変数 (MVar)
  | VMatrix Int Int (VS.Vector Double)         -- 行列 (行数, 列数, データ)
  | VCLContext (Ptr ()) (Ptr ())               -- OpenCL コンテキスト (Context, CommandQueue)
  | VCLBuffer  (Ptr ()) Int                    -- OpenCL バッファ (Mem, 要素数)
  | VCLKernel  (Ptr ()) Text                   -- OpenCL カーネル (Kernel, カーネル名)
  | VWindow    GLFW.Window                     -- GLFW ウィンドウハンドル

-- | テスト用の構造的等値比較
--   VPrim, VFunc, VMacro, VMVar は関数/参照を含むため常に不等
instance Eq Val where
  VInt  a   == VInt  b   = a == b
  VFloat a  == VFloat b  = a == b
  VBool a   == VBool b   = a == b
  VStr  a   == VStr  b   = a == b
  VSym  a   == VSym  b   = a == b
  VList as  == VList bs  = as == bs
  VData n1 vs1 == VData n2 vs2 = n1 == n2 && vs1 == vs2
  VNil      == VNil      = True
  VMVar _   == VMVar _   = False  -- MVar は参照のため常に不等
  VMatrix r1 c1 v1 == VMatrix r2 c2 v2 = r1 == r2 && c1 == c2 && v1 == v2
  VCLContext c1 q1 == VCLContext c2 q2 = c1 == c2 && q1 == q2
  VCLBuffer m1 s1  == VCLBuffer m2 s2  = m1 == m2 && s1 == s2
  VCLKernel k1 n1  == VCLKernel k2 n2  = k1 == k2 && n1 == n2
  VWindow w1       == VWindow w2       = w1 == w2
  _         == _         = False

instance Show Val where
  show = showVal

-- | Val の表示用関数
showVal :: Val -> String
showVal (VInt n)       = show n
showVal (VFloat f)     = show f
showVal (VBool True)   = "#t"
showVal (VBool False)  = "#f"
showVal (VPrim name _) = "<primitive:" ++ show name ++ ">"
showVal (VFunc _ _ _)  = "<function>"
showVal (VMacro _ _ _) = "<macro>"
showVal (VList vs)     = "(" ++ unwords (map showVal vs) ++ ")"
showVal VNil           = "nil"
showVal (VSym s)       = show s
showVal (VStr s)       = show s
showVal (VData name []) = T.unpack name
showVal (VData name vs) = "(" ++ T.unpack name ++ " " ++ unwords (map showVal vs) ++ ")"
showVal (VMVar _)       = "<mvar>"
showVal (VMatrix rows cols vec) = "#m(" ++ unwords (map showRow [0..rows-1]) ++ ")"
  where
    showRow r = "(" ++ unwords [show (vec VS.! (r * cols + c)) | c <- [0..cols-1]] ++ ")"
showVal (VCLContext _ _)  = "<CLContext>"
showVal (VCLBuffer _ n)   = "<CLBuffer:size=" ++ show n ++ ">"
showVal (VCLKernel _ name) = "<CLKernel:" ++ T.unpack name ++ ">"
showVal (VWindow _)        = "<Window>"
