{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Spinor.BorrowCheck
Description : 所有権と線形性の静的解析 (Experimental)
License     : MIT

変数の使用回数とライフタイムを追跡し、線形型の不正使用を検出する。
- 線形変数の二重使用
- 線形変数の未消費
- スコープ終了時の所有権解放位置の特定
-}
module Spinor.BorrowCheck
  ( -- * 解析実行
    checkBorrow
  , BorrowResult(..)
  , OwnershipInfo(..)
  , VarState(..)
    -- * エラー
  , BorrowError(..)
  , formatBorrowError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad (forM_, when)
import Control.Monad.State.Strict

import Spinor.Syntax (Expr(..), Pattern(..), SourceSpan, dummySpan, exprSpan)
import Spinor.Type (Linearity(..))

-- ============================================================
-- データ型定義
-- ============================================================

-- | 変数の所有権状態
data VarState
  = Owned       -- ^ 所有権を保持 (スコープ終了時に解放責任あり)
  | Borrowed    -- ^ 借用 (解放責任なし)
  | Consumed    -- ^ すでに消費済み (ムーブ済み)
  | Dropped     -- ^ 明示的にドロップ済み
  deriving (Eq, Show)

-- | 変数の所有権情報
data OwnershipInfo = OwnershipInfo
  { ownerVar      :: Text        -- ^ 変数名
  , ownerState    :: VarState    -- ^ 現在の状態
  , ownerLinear   :: Bool        -- ^ 線形型かどうか
  , ownerDefSpan  :: SourceSpan  -- ^ 定義位置
  , ownerUseCount :: Int         -- ^ 使用回数
  } deriving (Show)

-- | ボローチェックエラー
data BorrowError
  = DoubleUse Text SourceSpan SourceSpan
    -- ^ 線形変数の二重使用 (変数名, 最初の使用位置, 二度目の使用位置)
  | Unconsumed Text SourceSpan
    -- ^ 線形変数が未消費のままスコープを抜けた (変数名, 定義位置)
  | UseAfterMove Text SourceSpan SourceSpan
    -- ^ ムーブ後の変数使用 (変数名, ムーブ位置, 使用位置)
  deriving (Show, Eq)

-- | エラーメッセージの整形
formatBorrowError :: BorrowError -> Text
formatBorrowError (DoubleUse var _ _) =
  "Linear variable '" <> var <> "' used more than once"
formatBorrowError (Unconsumed var _) =
  "Linear variable '" <> var <> "' must be consumed before scope ends"
formatBorrowError (UseAfterMove var _ _) =
  "Variable '" <> var <> "' used after being moved"

-- | ボローチェックの結果
data BorrowResult = BorrowResult
  { brErrors     :: [BorrowError]
  , brDropPoints :: Map.Map Text SourceSpan
    -- ^ 変数名 → 解放すべき位置のマップ (C コード生成で使用)
  } deriving (Show)

-- ============================================================
-- 解析状態
-- ============================================================

-- | 解析状態
data CheckState = CheckState
  { csVars       :: Map.Map Text OwnershipInfo  -- ^ 変数の所有権情報
  , csErrors     :: [BorrowError]               -- ^ 発見されたエラー
  , csDropPoints :: Map.Map Text SourceSpan     -- ^ 解放位置
  , csCurrentSpan :: SourceSpan                 -- ^ 現在の解析位置
  } deriving (Show)

-- | 初期状態
initialState :: CheckState
initialState = CheckState
  { csVars       = Map.empty
  , csErrors     = []
  , csDropPoints = Map.empty
  , csCurrentSpan = dummySpan
  }

-- | 解析モナド
type Check a = State CheckState a

-- ============================================================
-- 解析ロジック
-- ============================================================

-- | ボローチェックの実行
--   AST を走査し、変数の使用回数とライフタイムを追跡する
checkBorrow :: [Expr] -> BorrowResult
checkBorrow exprs =
  let finalState = execState (mapM_ checkExpr exprs >> finalizeScope) initialState
  in BorrowResult
       { brErrors     = reverse (csErrors finalState)
       , brDropPoints = csDropPoints finalState
       }

-- | スコープ終了時の処理
--   未消費の線形変数をエラーとして報告
finalizeScope :: Check ()
finalizeScope = do
  vars <- gets csVars
  forM_ (Map.toList vars) $ \(name, info) -> do
    when (ownerLinear info && ownerState info == Owned && ownerUseCount info == 0) $ do
      addError $ Unconsumed name (ownerDefSpan info)

-- | 式の解析
checkExpr :: Expr -> Check ()
checkExpr (EInt _ _) = pure ()
checkExpr (EBool _ _) = pure ()
checkExpr (EStr _ _) = pure ()

checkExpr (ESym sp name) = do
  -- 変数の使用を記録
  mInfo <- gets (Map.lookup name . csVars)
  case mInfo of
    Just info -> do
      case ownerState info of
        Consumed -> addError $ UseAfterMove name (ownerDefSpan info) sp
        _ -> do
          -- 使用回数をインクリメント
          let newCount = ownerUseCount info + 1
          when (ownerLinear info && newCount > 1) $ do
            addError $ DoubleUse name (ownerDefSpan info) sp
          -- 線形変数は使用後に Consumed に
          let newState = if ownerLinear info then Consumed else ownerState info
          modify $ \s -> s { csVars = Map.insert name
                              (info { ownerUseCount = newCount, ownerState = newState })
                              (csVars s) }
    Nothing -> pure ()  -- 未定義変数 (型チェッカーがエラーを出す)

checkExpr (EList sp exprs) = do
  modify $ \s -> s { csCurrentSpan = sp }
  case exprs of
    -- let 式: 新しいスコープで変数を束縛
    (ESym _ "let" : _) -> checkLetLike sp exprs
    -- fn/lambda: 引数を線形としてマーク可能
    (ESym _ "fn" : _) -> checkFnLike sp exprs
    -- linear: 線形変数の明示的な宣言 (experimental)
    (ESym _ "linear" : ESym defSpan name : valExpr : _) -> do
      checkExpr valExpr
      registerVar name True defSpan
    -- drop: 明示的な解放
    (ESym _ "drop" : ESym _ name : _) -> do
      mInfo <- gets (Map.lookup name . csVars)
      case mInfo of
        Just info -> do
          modify $ \s -> s
            { csVars = Map.insert name (info { ownerState = Dropped }) (csVars s)
            , csDropPoints = Map.insert name sp (csDropPoints s)
            }
        Nothing -> pure ()
    -- 通常の関数呼び出し
    _ -> mapM_ checkExpr exprs

checkExpr (ELet sp bindings body) = do
  modify $ \s -> s { csCurrentSpan = sp }
  -- 束縛を処理
  forM_ bindings $ \(name, valExpr) -> do
    checkExpr valExpr
    registerVar name False (exprSpan valExpr)
  -- 本体を処理
  checkExpr body
  -- スコープ終了: 束縛された変数の解放位置を記録
  forM_ bindings $ \(name, _) -> do
    mInfo <- gets (Map.lookup name . csVars)
    case mInfo of
      Just info | ownerState info == Owned -> do
        modify $ \s -> s { csDropPoints = Map.insert name sp (csDropPoints s) }
      _ -> pure ()

checkExpr (EData _ _ _) = pure ()  -- データ定義は所有権チェック不要

checkExpr (EMatch sp target branches) = do
  modify $ \s -> s { csCurrentSpan = sp }
  checkExpr target
  forM_ branches $ \(pat, body) -> do
    checkPattern pat
    checkExpr body

checkExpr (EModule _ _ _) = pure ()
checkExpr (EImport _ _ _) = pure ()

-- | let 風の式の処理
checkLetLike :: SourceSpan -> [Expr] -> Check ()
checkLetLike sp exprs = case exprs of
  [ESym _ "let", ESym defSpan name, val, body] -> do
    checkExpr val
    registerVar name False defSpan
    checkExpr body
  _ -> mapM_ checkExpr exprs

-- | fn/lambda 式の処理
checkFnLike :: SourceSpan -> [Expr] -> Check ()
checkFnLike _ exprs = case exprs of
  (ESym _ "fn" : EList _ params : body : _) -> do
    -- 引数を登録 (デフォルトは非線形)
    forM_ params $ \param -> case param of
      ESym defSpan name -> registerVar name False defSpan
      _ -> pure ()
    checkExpr body
  _ -> mapM_ checkExpr exprs

-- | パターンの処理
checkPattern :: Pattern -> Check ()
checkPattern (PVar name) = registerVar name False dummySpan
checkPattern (PCon _ pats) = mapM_ checkPattern pats
checkPattern (PLit _) = pure ()
checkPattern PWild = pure ()

-- | 変数を登録
registerVar :: Text -> Bool -> SourceSpan -> Check ()
registerVar name isLinear defSpan = do
  let info = OwnershipInfo
        { ownerVar = name
        , ownerState = Owned
        , ownerLinear = isLinear
        , ownerDefSpan = defSpan
        , ownerUseCount = 0
        }
  modify $ \s -> s { csVars = Map.insert name info (csVars s) }

-- | エラーを追加
addError :: BorrowError -> Check ()
addError err = modify $ \s -> s { csErrors = err : csErrors s }
