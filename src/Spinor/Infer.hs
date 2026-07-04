{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module:      Spinor.Infer
-- Description: Algorithm W に「リソース消費トラッキング」を載せた型推論器
--              (Phase 3 / Issue #66 Linear Spinor R0-2)
--
-- 設計サマリ:
--
--  * 'infer' は @(Subst, Type, TypeEnv)@ を返す。3 つ目の TypeEnv は
--    「評価後に消費されずに残った環境」 (post-state environment) である。
--
--  * 線形変数の判定は本 Phase では簡易判定:
--      * @Scheme [] (TLinear Linear _)@ — 旧 legacy 形式
--      * @Scheme [] (TArrMult One _ _)@ — Linear Spinor 多重度付き矢印で多重度が 'One'
--    上記を満たす環境エントリを「線形 (One)」として扱う。
--
--  * 'Infer' モナドには新たに @MovedSet :: Set Text@ を持たせ、
--    「線形変数として既に消費 (move) された名前」を追跡する。
--    これにより 'ESym' ルックアップ時に 「未定義」と「move 済み」を
--    明確に区別したエラーを出せる。
--
--  * 'let' 束縛は、束縛側で導入した線形変数が body 評価後 env から
--    全て消えている (= 全て消費された) かを検査し、残っていれば
--    「resource leak」エラーを出す。
--
--  * @(if cond thn els)@ は 'envAfterCond' を起点に 2 分岐を独立に推論し、
--    両分岐で消費された線形変数の集合が一致しなければエラーとする。
--    パターンマッチ ('EMatch') も同様にフォーク→マージする。
--
--  * 既存の非線形 (Many / Unrestricted) 動作とは挙動を変えていない:
--    線形扱いされる Scheme が登場しない限り、env は素通しされ
--    従来の Algorithm W と等価。これによって 'twister/' 互換性も最大限保つ。
module Spinor.Infer
  ( Subst
  , Types(..)
  , nullSubst
  , composeSubst
  , unify
  , Infer
  , runInfer
  , runInferFrom
  , infer
  , inferTop
  , generalize
  , baseTypeEnv
  ) where

import Data.Text (Text, pack, isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad (foldM, when)
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Type   (Type(..), Scheme(..), TypeEnv, Linearity(..), showType, showMult)
import Spinor.Syntax (Expr(..), Pattern(..), TypeExpr(..), ConstructorDef(..), Mult(..),
                      SourceSpan, SpinorError(..), dummySpan, exprSpan)

-- ============================================================
-- 置換 (Substitution)
-- ============================================================

-- | 型変数名から具体的な型へのマッピング
type Subst = Map.Map Text Type

-- | 空の置換
nullSubst :: Subst
nullSubst = Map.empty

-- | 置換の合成
--   s1 `composeSubst` s2 は「まず s2 を適用し、次に s1 を適用する」置換。
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

-- ============================================================
-- Types クラス (置換の適用と自由型変数の取得)
-- ============================================================

-- | 型に対する操作を統一するクラス
class Types a where
  apply :: Subst -> a -> a
  ftv   :: a -> Set.Set Text

instance Types Type where
  apply s (TVar n)     = Map.findWithDefault (TVar n) n s
  apply _ TInt         = TInt
  apply _ TBool        = TBool
  apply _ TStr         = TStr
  apply _ TKeyword     = TKeyword
  apply s (TArr t1 t2) = TArr (apply s t1) (apply s t2)
  apply s (TList t)    = TList (apply s t)
  apply _ (TCon n)     = TCon n
  apply s (TApp t1 t2) = TApp (apply s t1) (apply s t2)
  apply s (TLinear lin t) = TLinear lin (apply s t)  -- 線形型: 内部型に置換適用
  apply s (TArrMult m t1 t2) = TArrMult m (apply s t1) (apply s t2)  -- 多重度付き矢印
  apply s (TBorrow t)  = TBorrow (apply s t)  -- 借用参照型: 内部型に置換適用

  ftv (TVar n)     = Set.singleton n
  ftv TInt         = Set.empty
  ftv TBool        = Set.empty
  ftv TStr         = Set.empty
  ftv TKeyword     = Set.empty
  ftv (TArr t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TList t)    = ftv t
  ftv (TCon _)     = Set.empty
  ftv (TApp t1 t2) = ftv t1 `Set.union` ftv t2
  ftv (TLinear _ t) = ftv t  -- 線形型: 内部型の自由変数
  ftv (TArrMult _ t1 t2) = ftv t1 `Set.union` ftv t2  -- 多重度付き矢印
  ftv (TBorrow t)  = ftv t  -- 借用参照型: 内部型の自由変数

instance Types Scheme where
  apply s (Scheme vars t) = Scheme vars (apply s' t)
    where s' = foldr Map.delete s vars
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars

instance Types a => Types [a] where
  apply s = map (apply s)
  ftv     = foldr (Set.union . ftv) Set.empty

-- TypeEnv に対する Types インスタンス
instance {-# OVERLAPPING #-} Types TypeEnv where
  apply s = Map.map (apply s)
  ftv     = ftv . Map.elems

-- ============================================================
-- 単一化 (Unification)
-- ============================================================

unify :: Type -> Type -> Either Text Subst

unify TInt     TInt     = Right nullSubst
unify TBool    TBool    = Right nullSubst
unify TStr     TStr     = Right nullSubst
unify TKeyword TKeyword = Right nullSubst

unify (TVar a) t = varBind a t
unify t (TVar a) = varBind a t

unify (TArr t1 t2) (TArr t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  Right (composeSubst s2 s1)

unify (TList t1) (TList t2) = unify t1 t2

unify (TCon a) (TCon b)
  | a == b    = Right nullSubst
  | otherwise = Left $ "型が一致しません: " <> a <> " と " <> b

unify (TApp t1 t2) (TApp t3 t4) = do
  s1 <- unify t1 t3
  s2 <- unify (apply s1 t2) (apply s1 t4)
  Right (composeSubst s2 s1)

-- 多重度付き矢印: 多重度が一致するときのみ unify
unify (TArrMult m1 t1 t2) (TArrMult m2 t3 t4)
  | m1 == m2  = do
      s1 <- unify t1 t3
      s2 <- unify (apply s1 t2) (apply s1 t4)
      Right (composeSubst s2 s1)
  | otherwise = Left $ "多重度が一致しません: " <> showMult m1 <> " と " <> showMult m2

-- 借用参照型: 内部型同士を unify
unify (TBorrow t1) (TBorrow t2) = unify t1 t2

unify t1 t2 = Left $ "型が一致しません: " <> showType t1 <> " と " <> showType t2

varBind :: Text -> Type -> Either Text Subst
varBind a t
  | t == TVar a          = Right nullSubst
  | a `Set.member` ftv t = Left $ "無限型エラー: " <> a
                                <> " は " <> showType t <> " に出現します"
  | otherwise            = Right (Map.singleton a t)

-- ============================================================
-- Infer モナド
-- ============================================================

-- | 「既に move (消費) された線形変数」の集合
type MovedSet = Set.Set Text

-- | 'Infer' モナド state:
--     * 'Int'      — フレッシュ型変数カウンタ (t0, t1, ...)
--     * 'MovedSet' — 既に消費された線形変数名
data InferState = InferState
  { isCounter :: !Int
  , isMoved   :: !MovedSet
  } deriving (Show)

-- | 型推論モナド
--   StateT InferState: フレッシュ型変数カウンタ + 消費済み線形変数集合
--   ExceptT SpinorError: 型エラーの報告 (位置情報付き)
newtype Infer a = Infer (StateT InferState (Either SpinorError) a)
  deriving (Functor, Applicative, Monad, MonadState InferState, MonadError SpinorError)

-- | Infer モナドを実行する (カウンタ 0 から開始)
runInfer :: Infer a -> Either SpinorError a
runInfer (Infer m) = fmap fst (runStateT m initialState)
  where initialState = InferState 0 Set.empty

-- | Infer モナドを指定カウンタから実行する (boot 中の連続推論用)
--   戻り値: (結果, 次のカウンタ)
--   注意: MovedSet は呼び出しごとに空にリセットされる
--         (トップレベル間で線形性は跨がない仕様)
runInferFrom :: Int -> Infer a -> Either SpinorError (a, Int)
runInferFrom n (Infer m) =
  fmap (\(a, st) -> (a, isCounter st)) (runStateT m (InferState n Set.empty))

-- | 位置情報付きエラーを投げるヘルパー
throwErrorAt :: SourceSpan -> Text -> Infer a
throwErrorAt srcSpan msg = throwError (SpinorError srcSpan msg)

-- | Text エラーを SpinorError に変換してリフトするヘルパー
--   unify など Text を返す関数の結果をリフトする際に使用
liftUnify :: SourceSpan -> Either Text a -> Infer a
liftUnify srcSpan (Left msg)  = throwErrorAt srcSpan msg
liftUnify _       (Right val) = pure val

-- | 新しい型変数を生成する (t0, t1, t2, ...)
fresh :: Infer Type
fresh = do
  st <- get
  let n = isCounter st
  put st { isCounter = n + 1 }
  pure $ TVar ("t" <> pack (show n))

-- | MovedSet に名前を追加 (move 発生時)
markMoved :: Text -> Infer ()
markMoved name = modify $ \st -> st { isMoved = Set.insert name (isMoved st) }

-- | MovedSet から名前を取り除く (再導入時のリセット用)
--   let で同名の線形変数を再束縛する等の場合に使う
unmarkMoved :: Text -> Infer ()
unmarkMoved name = modify $ \st -> st { isMoved = Set.delete name (isMoved st) }

-- | 名前が move 済みかチェック
isMovedName :: Text -> Infer Bool
isMovedName name = gets (Set.member name . isMoved)

-- | MovedSet 全体のスナップショット取得
getMoved :: Infer MovedSet
getMoved = gets isMoved

-- | MovedSet を上書き
putMoved :: MovedSet -> Infer ()
putMoved ms = modify $ \st -> st { isMoved = ms }

-- ============================================================
-- 線形性判定ヘルパー
-- ============================================================

-- | Scheme が「線形 (multiplicity One)」とみなされるか判定
--   本 Phase は簡易判定:
--     * Scheme [] (TLinear Linear _)       — legacy
--     * Scheme [] (TArrMult One _ _)       — Linear Spinor の多重度付き矢印
--   将来的に Scheme 自身に多重度メタを持たせる設計に拡張予定。
isLinearScheme :: Scheme -> Bool
isLinearScheme (Scheme _ (TLinear Linear _))      = True
isLinearScheme (Scheme _ (TArrMult One _ _))      = True
isLinearScheme _                                   = False

-- | 環境から線形変数名の集合を取り出す
linearNames :: TypeEnv -> Set.Set Text
linearNames = Map.keysSet . Map.filter isLinearScheme

-- ============================================================
-- instantiate / generalize
-- ============================================================

-- | Scheme の量子化された変数をフレッシュ型変数に置き換える
--   forall a b. a -> b  →  t0 -> t1  (フレッシュな型変数で具体化)
instantiate :: Scheme -> Infer Type
instantiate (Scheme vars t) = do
  freshVars <- mapM (const fresh) vars
  let s = Map.fromList (zip vars freshVars)
  pure (apply s t)

-- | 環境に含まれない自由型変数を量子化して Scheme にする
--   例: 環境の ftv = {}, 型 = a -> a  →  forall a. a -> a
generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (Set.toList vars) t
  where vars = ftv t `Set.difference` ftv env

-- ============================================================
-- 型推論 (Algorithm W) — リソース消費トラッキング対応
-- ============================================================

-- | AST を走査して型を推論する
--
--   戻り値: @(置換, 推論された型, 評価後に残った環境)@
--
--   3 つ目の TypeEnv は「この式が評価された後に残る (= 消費されていない)
--   環境」。線形変数 (multiplicity One) は ESym ルックアップで「消費」され、
--   この返却 env から取り除かれる。
infer :: TypeEnv -> Expr -> Infer (Subst, Type, TypeEnv)

-- 整数リテラル → TInt (env 不変)
infer env (EInt _ _) = pure (nullSubst, TInt, env)

-- 真偽値リテラル → TBool (env 不変)
infer env (EBool _ _) = pure (nullSubst, TBool, env)

-- 文字列リテラル → TStr (env 不変)
infer env (EStr _ _) = pure (nullSubst, TStr, env)

-- シンボル → 型環境から検索して instantiate
--   キーワードシンボル (`:` で始まる) は自己評価: TKeyword を返す
--   線形変数はルックアップ後に env から消費する (= 削除)
infer env (ESym sp x)
  | ":" `isPrefixOf` x = pure (nullSubst, TKeyword, env)
  | otherwise =
      case Map.lookup x env of
        Just scheme -> do
          t <- instantiate scheme
          if isLinearScheme scheme
            then do
              -- 線形変数: env から消費し、MovedSet に登録
              markMoved x
              pure (nullSubst, t, Map.delete x env)
            else
              -- 非線形: env そのまま
              pure (nullSubst, t, env)
        Nothing -> do
          -- 未定義か、それとも消費済み (use-after-move) か?
          moved <- isMovedName x
          if moved
            then throwErrorAt sp $ "線形変数 '" <> x
                                <> "' は既に move されています (use-after-move)"
            else throwErrorAt sp $ "未定義のシンボル: " <> x

-- 空リスト → フレッシュな要素型の空リスト
infer env (EList _ []) = do
  a <- fresh
  pure (nullSubst, TList a, env)

-- quote → quote の中身を型推論 (リテラルとリストのみ)
infer env (EList _ [ESym _ "quote", expr]) =
  pure (nullSubst, inferQuote expr, env)

-- if: cond は Bool, then と else の型を単一化
--     2 分岐は envAfterCond を起点に独立に推論し、消費された線形変数の
--     集合 (差分) が一致しなければエラー。
infer env (EList sp [ESym _ "if", cond, thn, els]) = do
  -- 1. cond を推論
  (s1, tCond, envAfterCond) <- infer env cond
  s1' <- liftUnify (exprSpan cond) $ unify (apply s1 tCond) TBool
  let s1''         = composeSubst s1' s1
      envAfterCond' = apply s1'' envAfterCond
  -- 2. 分岐前の MovedSet を保存
  movedBefore <- getMoved
  let linBefore = linearNames envAfterCond'
  -- 3. then 推論
  (s2, tThn, envThn) <- infer envAfterCond' thn
  movedAfterThn <- getMoved
  -- 4. MovedSet を分岐前に戻し、envAfterCond' から else を推論
  putMoved movedBefore
  (s3, tEls, envEls) <- infer envAfterCond' els
  movedAfterEls <- getMoved
  -- 5. 両分岐で消費した線形変数の集合をチェック
  let consumedThn = Set.intersection linBefore (Set.difference movedAfterThn movedBefore)
      consumedEls = Set.intersection linBefore (Set.difference movedAfterEls movedBefore)
      onlyInThn   = Set.difference consumedThn consumedEls
      onlyInEls   = Set.difference consumedEls consumedThn
  when (not (Set.null onlyInThn)) $
    throwErrorAt sp $
      "if の分岐で線形変数の消費が一致しません: '"
        <> Set.findMin onlyInThn
        <> "' は then で消費されているが else では未消費"
  when (not (Set.null onlyInEls)) $
    throwErrorAt sp $
      "if の分岐で線形変数の消費が一致しません: '"
        <> Set.findMin onlyInEls
        <> "' は else で消費されているが then では未消費"
  -- 6. 型を unify
  let s123 = composeSubst s3 (composeSubst s2 s1'')
  s4 <- liftUnify sp $ unify (apply s123 tThn) (apply s123 tEls)
  let sFinal = composeSubst s4 s123
  -- 7. MovedSet の合流 (両分岐の和集合)
  putMoved (Set.union movedAfterThn movedAfterEls)
  -- 8. env の合流: 両分岐とも消費していれば消費扱い。
  --    一致しているはずなので envThn を採用 (env サイズが一致する保証あり)。
  --    厳密には envThn と envEls の intersection を取るほうが正確だが、
  --    上記チェックを通過していれば、線形変数キー集合は両者で同じ。
  let envMerged = Map.intersectionWith const envThn envEls
  pure (sFinal, apply sFinal tThn, apply sFinal envMerged)

-- let: Let多相 (並列束縛) — 各 val を順に推論 → generalize → body を推論
--      body 評価後、let が「導入した線形変数」が全て消費されているか検査。
infer env (ELet sp bindings body) = do
  -- 1. すべての束縛を現在の環境で順に推論
  --    (env を引き回し、各 val 評価で env が消費される可能性に対応)
  (sFinal, envAfterBindings, bindingResults) <-
    foldM inferBinding (nullSubst, env, []) bindings
  -- 2. 推論結果を generalize して環境に追加
  let extendEnv (e, introducedAcc) (name, t) =
        let scheme = generalize (apply sFinal e) (apply sFinal t)
            e'     = Map.insert name scheme e
            -- 同名が既に moved 済みなら、再導入につき MovedSet からクリアする
            -- (副作用は extendEnvM で実施。ここは純粋に「導入名集合」を蓄積)
            intro' = if isLinearScheme scheme
                     then Set.insert name introducedAcc
                     else introducedAcc
        in (e', intro')
      (envWithBindings, introducedLinears) =
        foldl extendEnv (envAfterBindings, Set.empty) bindingResults
  -- 同名再束縛で moved 扱いになっている可能性があるためクリア
  mapM_ unmarkMoved (Set.toList introducedLinears)
  -- 3. body を推論
  (s2, t2, envAfterBody) <- infer envWithBindings body
  -- 4. body 評価後の env に、let で導入した線形変数が残っていれば leak エラー
  let leaked = Set.intersection introducedLinears (Map.keysSet envAfterBody)
  when (not (Set.null leaked)) $
    throwErrorAt sp $
      "線形変数 '" <> Set.findMin leaked
        <> "' が消費されずにスコープを抜けました (resource leak)"
  -- 5. 返却 env からは let が導入した名前を除去 (スコープアウト)
  let envOut = foldr Map.delete envAfterBody (Set.toList introducedLinears)
  pure (composeSubst s2 sFinal, t2, envOut)
  where
    inferBinding (sAcc, eAcc, results) (name, val) = do
      let envApplied = apply sAcc eAcc
      (s1, t1, eAfter) <- infer envApplied val
      let sNew = composeSubst s1 sAcc
      pure (sNew, eAfter, results ++ [(name, t1)])

-- define / def: 本体を推論し、環境に追加
infer env (EList _ [ESym _ "define", ESym _ name, body]) = inferDefine env name body
infer env (EList _ [ESym _ "def",    ESym _ name, body]) = inferDefine env name body

-- fn (固定長引数): 引数にフレッシュ型変数を割り当て、本体を推論
--   関数本体の評価では env から線形変数が消費される可能性があるが、
--   関数自体は値なので、推論文脈の env は不変として返す。
infer env (EList _ [ESym _ "fn", EList _ params, body]) = do
  paramNames <- mapM extractSymName params
  freshTypes <- mapM (const fresh) paramNames
  let paramSchemes = map (\t -> Scheme [] t) freshTypes
      env' = Map.union (Map.fromList (zip paramNames paramSchemes)) env
  -- 関数本体の MovedSet 効果は外に漏らさないため保存・復元
  movedBefore <- getMoved
  (s, tBody, _envBody) <- infer env' body
  putMoved movedBefore
  let tFunc = foldr (\t acc -> TArr (apply s t) acc) (apply s tBody) freshTypes
  pure (s, tFunc, env)

-- fn (全引数キャプチャ): 引数リスト全体を1つのリスト型として扱う
infer env (EList _ [ESym _ "fn", ESym _ param, body]) = do
  a <- fresh
  let paramT = TList a
      env' = Map.insert param (Scheme [] paramT) env
  movedBefore <- getMoved
  (s, tBody, _envBody) <- infer env' body
  putMoved movedBefore
  pure (s, TArr (apply s paramT) (apply s tBody), env)

-- match 式: target を推論 → 各分岐のパターンと body を推論
--           分岐間で消費した線形変数の集合が一致することを要求
infer env (EMatch sp targetExpr branches) = do
  (s0, tTarget, envAfterTarget) <- infer env targetExpr
  tResult <- fresh
  movedBeforeBranches <- getMoved
  let linBefore = linearNames envAfterTarget
  -- 各分岐を独立に推論し、(置換, 結果型, 分岐後 env, 分岐後 moved) を集める
  branchResults <- mapM
    (\br -> do
       putMoved movedBeforeBranches
       (sB, tB, envB) <- inferBranch envAfterTarget tTarget tResult br
       movedAfterB <- getMoved
       pure (sB, tB, envB, movedAfterB))
    branches
  -- 分岐間の線形変数消費の一致を検証
  case branchResults of
    [] -> do
      -- 分岐なし: 環境はそのまま
      pure (s0, apply s0 tResult, envAfterTarget)
    (firstSB, firstTB, firstEnv, firstMoved) : rest -> do
      let firstConsumed = Set.intersection linBefore (Set.difference firstMoved movedBeforeBranches)
      mapM_ (\(_, _, _, m) -> do
        let consumed = Set.intersection linBefore (Set.difference m movedBeforeBranches)
            diff = Set.union
                     (Set.difference consumed firstConsumed)
                     (Set.difference firstConsumed consumed)
        when (not (Set.null diff)) $
          throwErrorAt sp $
            "match の分岐で線形変数の消費が一致しません: '"
              <> Set.findMin diff <> "'") rest
      -- 全分岐の MovedSet を和集合で合流
      let allMoved = foldr (\(_, _, _, m) acc -> Set.union m acc) firstMoved rest
      putMoved allMoved
      -- 結果置換は折り畳み、結果型を統一
      let sCombined = foldr (\(sB, _, _, _) acc -> composeSubst sB acc) (composeSubst firstSB s0) rest
      -- 結果型同士の unify (分岐ごとに inferBranch 内で tResult と unify 済み)
      let sFinal = sCombined
          tFinal = apply sFinal firstTB
      -- env は分岐 env の intersection (両分岐に残った名前のみ)
      let envMerged = foldr (\(_, _, e, _) acc -> Map.intersectionWith const e acc) firstEnv rest
      pure (sFinal, tFinal, apply sFinal envMerged)
  where
    inferBranch envBranch tTarget tResult (pat, body) = do
      (s1, envExt) <- inferPattern envBranch tTarget pat
      let env' = Map.union envExt (apply s1 envBranch)
      (s2, tBody, envAfterBody) <- infer env' body
      let s2Acc = composeSubst s2 s1
      s3 <- liftUnify (exprSpan body) $ unify (apply s2Acc tResult) (apply s2Acc tBody)
      let s3Acc = composeSubst s3 s2Acc
      pure (s3Acc, apply s3Acc tResult, envAfterBody)

-- data 式 (式レベル): inferTop で処理するが、infer にもケースが必要
infer env (EData _ _ _) = pure (nullSubst, TCon "Unit", env)

-- module 宣言: Unit を返す
infer env (EModule _ _ _) = pure (nullSubst, TCon "Unit", env)

-- import 宣言: Unit を返す
infer env (EImport _ _ _) = pure (nullSubst, TCon "Unit", env)

-- Experimental: Region-based memory management
-- with-region: 本体の型を返す (本体は内部で env を消費しうる)
infer env (EWithRegion _ _ body) = infer env body

-- alloc-in: 内部式の型を返す
infer env (EAllocIn _ _ expr) = infer env expr

-- Phase 3 (Linear Spinor): 所有権/借用システムの意味論 (Issue #72)

-- 借用 (&expr): 対象を型付けするが線形変数を「消費しない」。
--   内部式を推論した後、消費記録 (MovedSet) と環境をロールバックすることで
--   借用が所有権をムーブしないことを保証する。結果型は借用参照型 &T。
infer env (EBorrow _ e) = do
  movedBefore <- getMoved
  (s, t, _envAfter) <- infer env e
  putMoved movedBefore                       -- 借用中の消費をなかったことにする
  pure (s, TBorrow (apply s t), apply s env) -- 元の環境を返す (何も消費しない)

-- 参照解決 (*expr): 借用参照型 &T を受け取り、中身の型 T を返す。
--   対象の型を @TBorrow inner@ と単一化し、inner を取り出す。
infer env (EDeref sp e) = do
  (s1, t, env1) <- infer env e
  inner <- fresh
  s2 <- liftUnify sp (unify (apply s1 t) (TBorrow inner))
  pure (composeSubst s2 s1, apply s2 inner, env1)

-- 隔離ブロック (unsafe expr): 内部の線形消費を外部に漏らさない。
--   MovedSet と環境を復元し、内部式の型のみを返す (線形性検査のバイパス)。
infer env (EUnsafe _ e) = do
  movedBefore <- getMoved
  (s, t, _envAfter) <- infer env e
  putMoved movedBefore                       -- 内部の消費を外に漏らさない
  pure (s, apply s t, apply s env)           -- 環境も未消費のまま返す

-- 明示的ムーブ (@expr): 対象を強制的に「消費」し、線形な型として返す。
--   対象が変数なら、線形・非線形を問わず環境から除去して move 済みとする。
--   これにより @x のあとに x を使うと use-after-move エラーになる。
infer env (EMove _ e) = do
  (s, t, env1) <- infer env e
  env2 <- case e of
            ESym _ x -> do markMoved x
                           pure (Map.delete x env1)
            _        -> pure env1
  let baseT = apply s t
      movedT = case baseT of
                 TLinear _ inner -> TLinear Linear inner  -- 二重ラップを避ける
                 other           -> TLinear Linear other
  pure (s, movedT, env2)

-- 関数適用: (func arg1 arg2 ...)
--   多引数はカリー化として扱う
infer env (EList _ (func : args)) = inferApp env func args

-- ============================================================
-- トップレベル推論 (inferTop)
-- ============================================================

-- | トップレベル式を推論し、define の場合は型環境を更新する
--   define: 右辺を推論 → generalize して型環境に登録 → 更新された型環境を返す
--   それ以外: 通常の infer → 型環境は変更なし
inferTop :: TypeEnv -> Expr -> Infer (TypeEnv, Subst, Type)
inferTop env (EList _ [ESym _ "define", ESym _ name, body]) = inferTopDefine env name body
inferTop env (EList _ [ESym _ "def",    ESym _ name, body]) = inferTopDefine env name body
-- data 式: コンストラクタの型を環境に登録する
inferTop env (EData _ typeName constrs) = do
  let -- 全コンストラクタ引数から自由型変数を収集
      allTypeVars = Set.toList $ foldMap conFtv constrs
      -- 結果型: (TypeName a b ...) — 型パラメータを全て適用
      resultType  = foldl TApp (TCon typeName) (map TVar allTypeVars)
  -- 各コンストラクタの型スキームを生成して環境に登録
  let newEnv = foldl (registerCon allTypeVars resultType) env constrs
  pure (newEnv, nullSubst, resultType)
  where
    -- ConstructorDef から自由型変数を収集
    conFtv (ConstructorDef _ fields) = foldMap typeExprFtv fields
    typeExprFtv (TEVar v)      = Set.singleton v
    typeExprFtv (TEApp _ args) = foldMap typeExprFtv args
    -- コンストラクタを TypeEnv に登録
    registerCon tvars resType envAcc (ConstructorDef cname fields) =
      let fieldTypes = map typeExprToType fields
          -- コンストラクタ型: field1 -> field2 -> ... -> ResultType
          conType = foldr TArr resType fieldTypes
          scheme  = Scheme tvars conType
      in Map.insert cname scheme envAcc

inferTop env expr = do
  (s, t, _envAfter) <- infer env expr
  pure (apply s env, s, t)

-- | define / def のトップレベル推論
--   1. 再帰対応: フレッシュ型変数を環境に仮登録
--   2. 右辺を推論
--   3. 仮型変数と推論結果を単一化
--   4. generalize して多相型に昇格
--   5. 更新された型環境を返す
inferTopDefine :: TypeEnv -> Text -> Expr -> Infer (TypeEnv, Subst, Type)
inferTopDefine env name body = do
  tv <- fresh
  let env' = Map.insert name (Scheme [] tv) env
  (s1, tBody, _envAfter) <- infer env' body
  s2 <- liftUnify (exprSpan body) $ unify (apply s1 tv) tBody
  let sFinal = composeSubst s2 s1
      finalType = apply sFinal tBody
      scheme = generalize (apply sFinal env) finalType
      newEnv = Map.insert name scheme (apply sFinal env)
  pure (newEnv, sFinal, finalType)

-- | quote 内の型を静的に推論する (簡易版)
inferQuote :: Expr -> Type
inferQuote (EInt _ _)      = TInt
inferQuote (EBool _ _)     = TBool
inferQuote (EStr _ _)      = TStr
inferQuote (EList _ [])    = TList (TVar "_q")
inferQuote (EList _ (x:_)) = TList (inferQuote x)
inferQuote (ESym _ _)      = TStr  -- quote されたシンボルは文字列的に扱う
inferQuote (ELet _ _ body) = inferQuote body
inferQuote (EData _ _ _)   = TCon "Unit"
inferQuote (EMatch _ _ _)  = TVar "_match"
inferQuote (EModule _ _ _) = TCon "Unit"
inferQuote (EImport _ _ _) = TCon "Unit"
inferQuote (EWithRegion _ _ body) = inferQuote body
inferQuote (EAllocIn _ _ expr)    = inferQuote expr
inferQuote (EBorrow _ e)          = inferQuote e
inferQuote (EDeref  _ e)          = inferQuote e
inferQuote (EUnsafe _ e)          = inferQuote e
inferQuote (EMove   _ e)          = inferQuote e

-- | パターンの型推論
--   パターンの型と tTarget を unify し、パターン内変数の型環境を返す
--   (env は変更しない — リソース消費は body 側で発生する)
inferPattern :: TypeEnv -> Type -> Pattern -> Infer (Subst, TypeEnv)
inferPattern _ tTarget (PVar name) = do
  tv <- fresh
  s <- liftUnify dummySpan $ unify tTarget tv
  pure (s, Map.singleton name (Scheme [] (apply s tv)))
inferPattern _ _ PWild = pure (nullSubst, Map.empty)
inferPattern env tTarget (PLit expr) = do
  (s1, tLit, _envAfter) <- infer env expr
  s2 <- liftUnify (exprSpan expr) $ unify (apply s1 tTarget) tLit
  pure (composeSubst s2 s1, Map.empty)
inferPattern env tTarget (PCon conName pats) =
  case Map.lookup conName env of
    Nothing -> throwErrorAt dummySpan $ "未定義のコンストラクタ: " <> conName
    Just scheme -> do
      conType <- instantiate scheme
      -- コンストラクタ型を分解: arg1 -> arg2 -> ... -> ResultType
      let (argTypes, resType) = splitArrType conType
      when (length argTypes /= length pats) $
        throwErrorAt dummySpan $ conName <> ": パターンの引数の数が不正です"
      s1 <- liftUnify dummySpan $ unify tTarget resType
      -- 各サブパターンを再帰推論
      (sFinal, envExt) <- foldM (\(sAcc, envAcc) (argT, pat) -> do
        let argT' = apply sAcc argT
        (s', envP) <- inferPattern (apply sAcc env) argT' pat
        pure (composeSubst s' sAcc, Map.union envP envAcc)
        ) (s1, Map.empty) (zip argTypes pats)
      pure (sFinal, envExt)

-- | 関数型を引数リストと結果型に分解する
splitArrType :: Type -> ([Type], Type)
splitArrType (TArr t1 t2) = let (args, res) = splitArrType t2 in (t1 : args, res)
splitArrType t             = ([], t)

-- | define / def の型推論共通実装 (式レベル)
--   注意: トップレベルではなく、let や式の中で define が現れた場合に使用。
--         返却 env は body 評価後の env をそのまま (define 名は内部スコープ)
inferDefine :: TypeEnv -> Text -> Expr -> Infer (Subst, Type, TypeEnv)
inferDefine env name body = do
  -- 再帰対応: 本体推論前にフレッシュ型変数を環境に入れる
  tv <- fresh
  let env' = Map.insert name (Scheme [] tv) env
  (s1, tBody, envAfter) <- infer env' body
  s2 <- liftUnify (exprSpan body) $ unify (apply s1 tv) tBody
  let sFinal = composeSubst s2 s1
  pure (sFinal, apply sFinal tBody, envAfter)

-- | 関数適用の型推論 (多引数対応)
--   func を推論 → 引数を順に推論 (env を引き回す) →
--   func の型を arg1 -> arg2 -> ... -> ret と単一化
--
--   引数評価で env から線形変数が消費されるため、左から右へ env を引き回す。
inferApp :: TypeEnv -> Expr -> [Expr] -> Infer (Subst, Type, TypeEnv)
inferApp env func args = do
  (s0, tFunc, envAfterFunc) <- infer env func
  tRet <- fresh
  -- 引数を左から順に推論し、置換 + env を累積する
  (sFinal, envFinal, tArgTypes) <-
    foldM inferArg (s0, envAfterFunc, []) args
  -- func の型を arg1 -> arg2 -> ... -> ret と単一化
  let expectedFuncType = foldr TArr tRet (reverse tArgTypes)
  sUnify <- liftUnify (exprSpan func) $
    unify (apply sFinal tFunc) (apply sFinal expectedFuncType)
  let sResult = composeSubst sUnify sFinal
  pure (sResult, apply sResult tRet, apply sResult envFinal)
  where
    inferArg (sAcc, eAcc, ts) argExpr = do
      (s1, tArg, eAfter) <- infer (apply sAcc eAcc) argExpr
      let s' = composeSubst s1 sAcc
      pure (s', eAfter, tArg : ts)

-- | TypeExpr を Type に変換する
typeExprToType :: TypeExpr -> Type
typeExprToType (TEVar v)        = TVar v
typeExprToType (TEApp name args) = foldl TApp (TCon name) (map typeExprToType args)

-- | Expr からシンボル名を取り出す (パラメータリスト用)
extractSymName :: Expr -> Infer Text
extractSymName (ESym _ s) = pure s
extractSymName expr       = throwErrorAt (exprSpan expr) "引数にはシンボルが必要です"

-- ============================================================
-- プリミティブの型環境
-- ============================================================

-- | プリミティブ関数の初期型環境
baseTypeEnv :: TypeEnv
baseTypeEnv = Map.fromList
  [ -- 算術演算: Int -> Int -> Int
    ("+",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("-",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("*",  Scheme [] (TArr TInt (TArr TInt TInt)))
  , ("%",  Scheme [] (TArr TInt (TArr TInt TInt)))
    -- 比較演算: Int -> Int -> Bool
  , ("<",  Scheme [] (TArr TInt (TArr TInt TBool)))
  , (">",  Scheme [] (TArr TInt (TArr TInt TBool)))
    -- 等値比較: forall a. a -> a -> Bool
  , ("=",  Scheme ["a"] (TArr (TVar "a") (TArr (TVar "a") TBool)))
    -- リスト操作
  , ("cons",   Scheme ["a"] (TArr (TVar "a") (TArr (TList (TVar "a")) (TList (TVar "a")))))
  , ("car",    Scheme ["a"] (TArr (TList (TVar "a")) (TVar "a")))
  , ("cdr",    Scheme ["a"] (TArr (TList (TVar "a")) (TList (TVar "a"))))
  , ("list",   Scheme [] (TArr (TVar "_") (TList (TVar "_"))))  -- 簡易版: 単引数として扱う
  , ("nil?",   Scheme ["a"] (TArr (TList (TVar "a")) TBool))
  , ("empty?", Scheme ["a"] (TArr (TList (TVar "a")) TBool))
    -- 出力
  , ("print",  Scheme ["a"] (TArr (TVar "a") (TVar "a")))
    -- 文字列操作
  , ("string-append", Scheme [] (TArr TStr (TArr TStr TStr)))
  , ("string-length", Scheme [] (TArr TStr TInt))
  , ("substring",     Scheme [] (TArr TStr (TArr TInt (TArr TInt TStr))))
  , ("string=?",      Scheme [] (TArr TStr (TArr TStr TBool)))
  , ("string->list",  Scheme [] (TArr TStr (TList TStr)))
  , ("list->string",  Scheme [] (TArr (TList TStr) TStr))
    -- 等価性
  , ("eq",    Scheme ["a"] (TArr (TVar "a") (TArr (TVar "a") TBool)))
  , ("equal", Scheme ["a"] (TArr (TVar "a") (TArr (TVar "a") TBool)))
    -- 行列操作
  , ("matrix",    Scheme [] (TArr TInt (TArr TInt (TArr (TList TInt) (TCon "Matrix")))))
  , ("mdim",      Scheme [] (TArr (TCon "Matrix") (TList TInt)))
  , ("mref",      Scheme [] (TArr (TCon "Matrix") (TArr TInt (TArr TInt (TCon "Float")))))
    -- BLAS/LAPACK
  , ("m+",        Scheme [] (TArr (TCon "Matrix") (TArr (TCon "Matrix") (TCon "Matrix"))))
  , ("m*",        Scheme [] (TArr (TCon "Matrix") (TArr (TCon "Matrix") (TCon "Matrix"))))
  , ("transpose", Scheme [] (TArr (TCon "Matrix") (TCon "Matrix")))
  , ("inverse",   Scheme [] (TArr (TCon "Matrix") (TCon "Matrix")))
    -- OpenCL / GPGPU
  , ("cl-init",    Scheme [] (TCon "CLContext"))
  , ("to-device",  Scheme [] (TArr (TCon "CLContext") (TArr (TCon "Matrix") (TCon "CLBuffer"))))
  , ("to-host",    Scheme [] (TArr (TCon "CLContext") (TArr (TCon "CLBuffer") (TArr TInt (TArr TInt (TCon "Matrix"))))))
  , ("cl-compile", Scheme [] (TArr (TCon "CLContext") (TArr TStr (TArr TStr (TCon "CLKernel")))))
  , ("cl-enqueue", Scheme ["a"] (TArr (TCon "CLContext") (TArr (TCon "CLKernel") (TArr (TList TInt) (TArr (TList TInt) (TVar "a"))))))
    -- OpenGL / GLFW
  , ("gl-init",                Scheme [] (TArr TInt (TArr TInt (TArr TStr (TCon "Window")))))
  , ("gl-window-should-close", Scheme [] (TArr (TCon "Window") TBool))
  , ("gl-swap-buffers",        Scheme [] (TArr (TCon "Window") (TCon "Nil")))
  , ("gl-clear",               Scheme [] (TCon "Nil"))
  , ("gl-draw-points",         Scheme [] (TArr (TCon "Matrix") (TCon "Nil")))
    -- JSON 操作
  , ("json-parse",     Scheme ["a"] (TArr TStr (TVar "a")))
  , ("json-stringify", Scheme ["a"] (TArr (TVar "a") TStr))
  ]
