{-# LANGUAGE OverloadedStrings #-}

module Spinor.Expander
  ( expand
  , expandAndEval
  ) where

import Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import Control.Monad.Except

import Spinor.Syntax (Expr(..), parseFile, dummySpan)
import Spinor.Val    (Val(..))
import Spinor.Eval   (Eval, eval, applyClosureBody, exprToVal, valToExpr)

-- | マクロを全て展開した純粋な AST を返す
--   expand 後の Expr にはマクロ呼び出しが残らない。
expand :: Expr -> Eval Expr

-- アトム: そのまま返す
expand e@(EInt _ _)  = pure e
expand e@(EBool _ _) = pure e
expand e@(EStr _ _)  = pure e
expand e@(ESym _ _)  = pure e

-- data 式: 展開不要、そのまま返す
expand e@(EData _ _ _) = pure e

-- module 宣言: 展開不要、そのまま返す
expand e@(EModule _ _ _) = pure e

-- import 宣言: 展開不要、そのまま返す
expand e@(EImport _ _ _) = pure e

-- match 式: target と各 body を展開 (パターン自体は展開不要)
expand (EMatch sp target branches) = do
  target' <- expand target
  branches' <- mapM (\(pat, body) -> do { body' <- expand body; pure (pat, body') }) branches
  pure $ EMatch sp target' branches'

-- 空リスト
expand e@(EList _ []) = pure e

-- quote: 内部を展開しない
expand e@(EList _ [ESym _ "quote", _]) = pure e

-- def / define: 名前は展開せず、本体のみ展開
expand (EList sp [s@(ESym _ "define"), name, body]) = do
  body' <- expand body
  pure $ EList sp [s, name, body']
expand (EList sp [s@(ESym _ "def"), name, body]) = do
  body' <- expand body
  pure $ EList sp [s, name, body']

-- fn / mac: パラメータリストは展開せず、本体のみ展開
expand (EList sp [s@(ESym _ "fn"), params, body]) = do
  body' <- expand body
  pure $ EList sp [s, params, body']
expand (EList sp [s@(ESym _ "mac"), params, body]) = do
  body' <- expand body
  pure $ EList sp [s, params, body']

-- let: 各束縛の val と body を再帰展開
expand (ELet sp bindings body) = do
  bindings' <- mapM (\(name, val) -> do
    val' <- expand val
    pure (name, val')) bindings
  body' <- expand body
  pure $ ELet sp bindings' body'

-- load: ファイルを読み込み、各式を展開+評価 (副作用あり)
--   Eval.hs から移動。expand フェーズで処理する必要がある
--   (ロードされたファイル内のマクロ定義を後続の展開に使うため)
expand (EList sp [ESym _ "load", arg]) = do
  argExpr <- expand arg
  argVal  <- eval argExpr
  case argVal of
    VStr path -> do
      content <- liftIO $ TIO.readFile (T.unpack path)
      case parseFile content of
        Left err   -> throwError $ "load パースエラー (" <> path <> "): " <> pack err
        Right exprs -> do
          mapM_ expandAndEval exprs
          pure $ EBool sp True
    _ -> throwError "load: ファイルパス (文字列) が必要です"

-- dotimes: count-expr と body を展開 (var は束縛変数なので展開しない)
expand (EList sp (ESym sp2 "dotimes" : EList sp3 [var@(ESym _ _), countExpr] : body)) = do
  countExpr' <- expand countExpr
  body' <- mapM expand body
  pure $ EList sp (ESym sp2 "dotimes" : EList sp3 [var, countExpr'] : body')

-- dolist: list-expr と body を展開 (var は束縛変数なので展開しない)
expand (EList sp (ESym sp2 "dolist" : EList sp3 [var@(ESym _ _), listExpr] : body)) = do
  listExpr' <- expand listExpr
  body' <- mapM expand body
  pure $ EList sp (ESym sp2 "dolist" : EList sp3 [var, listExpr'] : body')

-- マクロ生成の let フォームを ELet に変換して再展開
--   マクロが (list 'let ...) で let を生成すると valToExpr で EList になるため、
--   ここで ELet に変換してから再展開する。
expand (EList sp (ESym _ "let" : EList _ bindingExprs : body@(_:_))) =
  case mapM parseBinding bindingExprs of
    Just bindings -> do
      let bodyExpr = case body of
            [b] -> b
            _   -> EList sp (ESym dummySpan "begin" : body)
      expand (ELet sp bindings bodyExpr)
    Nothing -> throwError "let: 不正な束縛リストです"
  where
    parseBinding (EList _ [ESym _ name, val]) = Just (name, val)
    parseBinding _ = Nothing

-- リスト: 先頭シンボルがマクロならマクロ展開、そうでなければ全要素を再帰展開
expand (EList sp (ESym sp2 name : args)) = do
  env <- get
  case Map.lookup name env of
    Just (VMacro params body closureEnv) -> do
      -- マクロ適用: 引数を評価せず Val に変換して適用
      let argVals = map exprToVal args
      result <- applyClosureBody params body closureEnv argVals
      -- 展開結果を Expr に逆変換し、再帰的に展開 (マクロがマクロを生成する場合)
      expand (valToExpr result)
    _ -> EList sp <$> mapM expand (ESym sp2 name : args)

-- その他のリスト (先頭が非シンボル)
expand (EList sp xs) = EList sp <$> mapM expand xs

-- | 展開してから評価するユーティリティ
expandAndEval :: Expr -> Eval Val
expandAndEval expr = expand expr >>= eval
