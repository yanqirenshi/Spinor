{-# LANGUAGE OverloadedStrings #-}

module Spinor.Loader
  ( ModuleRegistry
  , LoaderConfig(..)
  , newModuleRegistry
  , loadModule
  , loadFile
  , evalFileWithModules
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.IORef
import Control.Monad (foldM, when)
import System.FilePath ((</>), normalise)
import System.Directory (doesFileExist)

import Spinor.Syntax (Expr(..), ImportOption(..), parseFile, formatError)
import Spinor.Val (Env)
import Spinor.Eval (eval, runEval)
import Spinor.Expander (expand)

-- | ロード済みモジュールの公開環境を保持するレジストリ
type ModuleRegistry = IORef (Map.Map Text Env)

-- | ローダー設定
data LoaderConfig = LoaderConfig
  { baseDir       :: FilePath       -- ^ プロジェクトルートディレクトリ
  , primitiveEnv  :: Env            -- ^ プリミティブ関数環境
  }

-- | 新しいモジュールレジストリを作成
newModuleRegistry :: IO ModuleRegistry
newModuleRegistry = newIORef Map.empty

-- | モジュール名からファイルパスを解決
resolveModulePath :: FilePath -> Text -> FilePath
resolveModulePath base modName =
  normalise $ base </> T.unpack modName ++ ".spin"

-- | モジュールをロードする
--   循環参照を検出し、依存関係を解決して評価する
loadModule :: LoaderConfig
           -> ModuleRegistry
           -> [Text]              -- ^ 現在ロード中のモジュール (循環検出用)
           -> Text                -- ^ ロードするモジュール名
           -> IO (Either Text Env)
loadModule config registry loading modName = do
  -- 循環参照チェック
  if modName `elem` loading
    then pure $ Left $ "循環参照を検出しました: " <> T.intercalate " -> " (reverse (modName : loading))
    else do
      -- 既にロード済みかチェック
      regMap <- readIORef registry
      case Map.lookup modName regMap of
        Just env -> pure $ Right env
        Nothing  -> actuallyLoadModule config registry loading modName

-- | 実際にモジュールをロードする内部関数
actuallyLoadModule :: LoaderConfig
                   -> ModuleRegistry
                   -> [Text]
                   -> Text
                   -> IO (Either Text Env)
actuallyLoadModule config registry loading modName = do
  let path = resolveModulePath (baseDir config) modName
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "モジュールが見つかりません: " <> modName <> " (" <> T.pack path <> ")"
    else do
      content <- readFileUtf8 path
      case parseFile content of
        Left err -> pure $ Left $ "パースエラー (" <> modName <> "): " <> T.pack err
        Right exprs -> processModule config registry (modName : loading) modName exprs

-- | モジュールの式を処理する
processModule :: LoaderConfig
              -> ModuleRegistry
              -> [Text]
              -> Text
              -> [Expr]
              -> IO (Either Text Env)
processModule config registry loading modName exprs = do
  -- module 宣言と import 宣言を抽出
  let (moduleDecl, imports, bodyExprs) = scanTopLevel exprs

  -- module 宣言の検証
  case moduleDecl of
    Nothing -> pure $ Left $ "モジュール宣言がありません: " <> modName
    Just (declName, exports) -> do
      -- モジュール名の一致を確認 (警告のみ)
      when (declName /= modName) $
        TIO.putStrLn $ "警告: モジュール名が不一致 (宣言: " <> declName <> ", ファイル: " <> modName <> ")"

      -- 依存モジュールをロード
      depResult <- loadDependencies config registry loading imports
      case depResult of
        Left err -> pure $ Left err
        Right importEnv -> do
          -- モジュール環境を構築 (primitive + import)
          let moduleEnv = Map.union importEnv (primitiveEnv config)

          -- 本体を評価
          evalResult <- evalExprList moduleEnv bodyExprs
          case evalResult of
            Left err -> pure $ Left $ "評価エラー (" <> modName <> "): " <> err
            Right finalEnv -> do
              -- エクスポートするシンボルのみを抽出
              let publicEnv = extractExports exports finalEnv
              -- レジストリに登録
              modifyIORef' registry (Map.insert modName publicEnv)
              pure $ Right publicEnv

-- | トップレベルの式をスキャンして module, import, その他に分類
scanTopLevel :: [Expr] -> (Maybe (Text, [Text]), [(Text, [ImportOption])], [Expr])
scanTopLevel = go Nothing [] []
  where
    go modDecl imports body [] = (modDecl, reverse imports, reverse body)
    go modDecl imports body (e:es) = case e of
      EModule _ name exports -> go (Just (name, exports)) imports body es
      EImport _ name opts    -> go modDecl ((name, opts) : imports) body es
      _                      -> go modDecl imports (e : body) es

-- | 依存モジュールをロードし、インポートされたシンボルを含む環境を構築
loadDependencies :: LoaderConfig
                 -> ModuleRegistry
                 -> [Text]
                 -> [(Text, [ImportOption])]
                 -> IO (Either Text Env)
loadDependencies config registry loading imports = do
  foldM loadOne (Right Map.empty) imports
  where
    loadOne (Left err) _ = pure $ Left err
    loadOne (Right acc) (modName, opts) = do
      result <- loadModule config registry loading modName
      case result of
        Left err -> pure $ Left err
        Right modEnv -> do
          let filtered = applyImportOptions opts modEnv
          pure $ Right $ Map.union filtered acc

-- | インポートオプションを適用してシンボルをフィルタリング
applyImportOptions :: [ImportOption] -> Env -> Env
applyImportOptions [] env = env  -- オプションなし: 全シンボルをインポート
applyImportOptions opts env = foldl applyOpt env opts
  where
    applyOpt e (Only names) =
      Map.filterWithKey (\k _ -> k `elem` names) e
    applyOpt e (Except names) =
      Map.filterWithKey (\k _ -> k `notElem` names) e
    applyOpt e (Prefix pfx) =
      Map.mapKeys (pfx <>) e
    applyOpt e (Alias alias) =
      -- alias の場合は M:sym という形式でキーを変換
      Map.mapKeys (\k -> alias <> ":" <> k) e

-- | エクスポートリストに基づいて公開シンボルを抽出
extractExports :: [Text] -> Env -> Env
extractExports [] _ = Map.empty  -- export が空なら何も公開しない
extractExports exports env =
  Map.filterWithKey (\k _ -> k `elem` exports) env

-- | 式のリストを順番に評価
evalExprList :: Env -> [Expr] -> IO (Either Text Env)
evalExprList env [] = pure $ Right env
evalExprList env (expr:rest) = do
  -- マクロ展開
  expandResult <- runEval env (expand expr)
  case expandResult of
    Left err -> pure $ Left (formatError err)
    Right (expanded, envAfterExpand) -> do
      -- 評価
      evalResult <- runEval envAfterExpand (eval expanded)
      case evalResult of
        Left err -> pure $ Left (formatError err)
        Right (_, env') -> evalExprList env' rest

-- | ファイルをモジュールとしてロードする
--   モジュール宣言がないファイルも処理可能
loadFile :: LoaderConfig
         -> ModuleRegistry
         -> FilePath
         -> IO (Either Text Env)
loadFile config registry path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "ファイルが見つかりません: " <> T.pack path
    else do
      content <- readFileUtf8 path
      case parseFile content of
        Left err -> pure $ Left $ "パースエラー: " <> T.pack err
        Right exprs -> do
          let (moduleDecl, imports, bodyExprs) = scanTopLevel exprs
          case moduleDecl of
            Just (modName, _) ->
              -- モジュール宣言がある場合は通常のモジュールロード
              processModule config registry [modName] modName exprs
            Nothing -> do
              -- モジュール宣言がない場合は直接評価
              depResult <- loadDependencies config registry [] imports
              case depResult of
                Left err -> pure $ Left err
                Right importEnv -> do
                  let moduleEnv = Map.union importEnv (primitiveEnv config)
                  evalExprList moduleEnv bodyExprs

-- | ファイルをモジュールシステム下で評価する (バッチモード用)
evalFileWithModules :: LoaderConfig
                    -> ModuleRegistry
                    -> FilePath
                    -> IO (Either Text (Env, Bool))  -- ^ (最終環境, 成功フラグ)
evalFileWithModules config registry path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "ファイルが見つかりません: " <> T.pack path
    else do
      content <- readFileUtf8 path
      case parseFile content of
        Left err -> pure $ Left $ "パースエラー: " <> T.pack err
        Right exprs -> do
          let (moduleDecl, imports, bodyExprs) = scanTopLevel exprs

          -- 依存モジュールをロード
          depResult <- loadDependencies config registry [] imports
          case depResult of
            Left err -> pure $ Left err
            Right importEnv -> do
              -- モジュール環境を構築
              let baseEnv = Map.union importEnv (primitiveEnv config)

              -- 本体を評価
              result <- evalExprListWithStatus baseEnv bodyExprs
              case result of
                Left err -> pure $ Left err
                Right (env, ok) -> do
                  -- モジュール宣言がある場合はレジストリに登録
                  case moduleDecl of
                    Just (modName, exports) -> do
                      let publicEnv = extractExports exports env
                      modifyIORef' registry (Map.insert modName publicEnv)
                    Nothing -> pure ()
                  pure $ Right (env, ok)

-- | 式のリストを順番に評価 (エラー時も継続して成功フラグを追跡)
evalExprListWithStatus :: Env -> [Expr] -> IO (Either Text (Env, Bool))
evalExprListWithStatus env exprs = go env True exprs
  where
    go e ok [] = pure $ Right (e, ok)
    go e ok (expr:rest) = do
      -- マクロ展開
      expandResult <- runEval e (expand expr)
      case expandResult of
        Left err -> do
          TIO.putStrLn $ "エラー: " <> formatError err
          go e False rest
        Right (expanded, envAfterExpand) -> do
          -- 評価
          evalResult <- runEval envAfterExpand (eval expanded)
          case evalResult of
            Left err -> do
              TIO.putStrLn $ "エラー: " <> formatError err
              go envAfterExpand False rest
            Right (_, env') -> go env' ok rest

-- | UTF-8 でファイルを読み込む (Windows 対応)
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = TE.decodeUtf8 <$> BS.readFile path
