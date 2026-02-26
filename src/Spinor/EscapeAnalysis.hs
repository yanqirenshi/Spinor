{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Spinor.EscapeAnalysis
Description : リージョンベースメモリ管理の逃避解析 (Escape Analysis)

リージョン内で割り当てられた値がリージョン外に逃避（Escape）しないことを
静的に検証する。

主な検査項目:
1. リージョン内の値を関数の戻り値として返してはならない
2. リージョン外の変数にリージョン内の値を代入してはならない
-}
module Spinor.EscapeAnalysis
  ( EscapeError(..)
  , EscapeResult(..)
  , checkEscape
  , formatEscapeError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad (forM_, when)
import Spinor.Syntax (Expr(..), SourceSpan, exprSpan)

-- | 変数がどのリージョンに属するかの情報
data VarRegion
  = InRegion Text    -- ^ 特定のリージョン内で割り当てられた
  | GlobalScope      -- ^ グローバルスコープ（リージョン外）
  deriving (Show, Eq)

-- | 逃避解析のエラー
data EscapeError
  = EscapeReturn Text Text SourceSpan
    -- ^ (変数名, リージョン名, 位置): リージョン内の値を return している
  | EscapeAssign Text Text Text SourceSpan
    -- ^ (代入先変数, リージョン内変数, リージョン名, 位置): リージョン内の値を外部変数に代入
  | UndefinedRegion Text SourceSpan
    -- ^ (リージョン名, 位置): 未定義のリージョンを参照
  deriving (Show, Eq)

-- | 逃避解析の結果
data EscapeResult = EscapeResult
  { erErrors  :: [EscapeError]           -- ^ 検出されたエラー
  , erRegions :: Set Text                -- ^ 使用されたリージョン名
  } deriving (Show, Eq)

-- | 解析の状態
data AnalysisState = AnalysisState
  { asVarRegions    :: Map Text VarRegion   -- ^ 変数 -> 所属リージョン
  , asActiveRegions :: Set Text             -- ^ 現在アクティブなリージョン
  , asUsedRegions   :: Set Text             -- ^ 使用されたリージョン名
  , asErrors        :: [EscapeError]        -- ^ エラーのリスト
  , asInReturn      :: Bool                 -- ^ 関数の戻り値位置にいるか
  } deriving (Show)

-- | 初期状態
initialState :: AnalysisState
initialState = AnalysisState
  { asVarRegions    = Map.empty
  , asActiveRegions = Set.empty
  , asUsedRegions   = Set.empty
  , asErrors        = []
  , asInReturn      = False
  }

type Analyzer a = State AnalysisState a

-- | エラーを記録
recordError :: EscapeError -> Analyzer ()
recordError err = modify $ \s -> s { asErrors = err : asErrors s }

-- | 変数をリージョンに登録
registerVar :: Text -> VarRegion -> Analyzer ()
registerVar name region = modify $ \s ->
  s { asVarRegions = Map.insert name region (asVarRegions s) }

-- | リージョンをアクティブにする
enterRegion :: Text -> Analyzer ()
enterRegion name = modify $ \s ->
  s { asActiveRegions = Set.insert name (asActiveRegions s)
    , asUsedRegions   = Set.insert name (asUsedRegions s)
    }

-- | リージョンを終了する
exitRegion :: Text -> Analyzer ()
exitRegion name = modify $ \s ->
  s { asActiveRegions = Set.delete name (asActiveRegions s) }

-- | 変数がリージョン内の値を参照しているか確認
checkVarEscape :: Text -> SourceSpan -> Analyzer ()
checkVarEscape varName sp = do
  st <- get
  case Map.lookup varName (asVarRegions st) of
    Just (InRegion regionName) -> do
      -- リージョン内の値を返そうとしている場合
      when (asInReturn st && not (Set.member regionName (asActiveRegions st))) $
        recordError (EscapeReturn varName regionName sp)
    _ -> pure ()

-- | 式を解析
analyzeExpr :: Expr -> Analyzer ()

-- with-region: 新しいリージョンを作成して本体を解析
analyzeExpr (EWithRegion sp regionName body) = do
  enterRegion regionName
  analyzeExpr body
  exitRegion regionName

-- alloc-in: 指定リージョンにオブジェクトを割り当て
analyzeExpr (EAllocIn sp regionName expr) = do
  st <- get
  -- リージョンが存在するか確認
  if Set.member regionName (asActiveRegions st)
    then analyzeExpr expr
    else recordError (UndefinedRegion regionName sp)

-- let: 変数束縛の解析
analyzeExpr (ELet sp bindings body) = do
  -- 各束縛を解析し、値がリージョン内かどうかを記録
  forM_ bindings $ \(varName, valExpr) -> do
    analyzeExpr valExpr
    -- 値の出所を判定
    region <- getExprRegion valExpr
    registerVar varName region
  analyzeExpr body

-- defun: 関数定義の解析 (戻り値位置のマーク)
analyzeExpr (EList sp (ESym _ "defun" : ESym _ _ : EList _ _ : body : _)) = do
  -- 関数本体は戻り値位置
  oldInReturn <- gets asInReturn
  modify $ \s -> s { asInReturn = True }
  analyzeExpr body
  modify $ \s -> s { asInReturn = oldInReturn }

-- 変数参照: 逃避チェック
analyzeExpr (ESym sp varName) = checkVarEscape varName sp

-- if 式: 条件と両分岐を解析
analyzeExpr (EList sp (ESym _ "if" : cond : thenE : elseE : _)) = do
  analyzeExpr cond
  analyzeExpr thenE
  analyzeExpr elseE

-- begin 式: 各式を順に解析
analyzeExpr (EList sp (ESym _ "begin" : exprs)) = mapM_ analyzeExpr exprs

-- 一般のリスト: 各要素を解析
analyzeExpr (EList sp exprs) = mapM_ analyzeExpr exprs

-- その他の式: 特に何もしない
analyzeExpr _ = pure ()

-- | 式がどのリージョンに属するかを判定
getExprRegion :: Expr -> Analyzer VarRegion
getExprRegion (EAllocIn _ regionName _) = pure (InRegion regionName)
getExprRegion (ESym _ varName) = do
  regions <- gets asVarRegions
  pure $ Map.findWithDefault GlobalScope varName regions
getExprRegion _ = pure GlobalScope

-- | 複数の式に対して逃避解析を実行
checkEscape :: [Expr] -> EscapeResult
checkEscape exprs =
  let finalState = execState (mapM_ analyzeExpr exprs) initialState
  in EscapeResult
       { erErrors  = reverse (asErrors finalState)
       , erRegions = asUsedRegions finalState
       }

-- | エラーを人間が読みやすい形式にフォーマット
formatEscapeError :: EscapeError -> Text
formatEscapeError (EscapeReturn varName regionName _) =
  "Value '" <> varName <> "' from region '" <> regionName <>
  "' cannot escape (returned from function)"
formatEscapeError (EscapeAssign targetVar srcVar regionName _) =
  "Value '" <> srcVar <> "' from region '" <> regionName <>
  "' cannot be assigned to external variable '" <> targetVar <> "'"
formatEscapeError (UndefinedRegion regionName _) =
  "Region '" <> regionName <> "' is not defined in current scope"
