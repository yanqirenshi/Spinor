{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsEOF, hSetEncoding, utf8)
import System.Directory (doesFileExist, getCurrentDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeDirectory, takeBaseName, replaceExtension)
import System.Process (readProcessWithExitCode)
import Spinor.Syntax    (Expr(..), readExpr, parseFile)
import Spinor.Type      (TypeEnv, showType)
import Spinor.Val       (Env)
import Spinor.Eval      (Eval, eval, runEval)
import Spinor.Expander  (expand)
import Spinor.Infer     (Types(..), runInfer, inferTop, baseTypeEnv)
import Spinor.Primitive (primitiveBindings)
import Spinor.Loader    (LoaderConfig(..), newModuleRegistry, evalFileWithModules)
import Spinor.Compiler.Codegen (compileProgram)

-- | Twister ファイル一覧 (ロード順)
twisterFiles :: [FilePath]
twisterFiles =
  [ "twister/core.spin"
  , "twister/list.spin"
  , "twister/math.spin"
  ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  args <- getArgs
  case args of
    []                -> replMode
    ["compile", file] -> compileMode file
    ["build", file]   -> buildMode file
    [file]            -> batchMode file
    _                 -> putStrLn "Usage: spinor [file] | spinor compile <file> | spinor build <file>"

-- | REPL モード (引数なし)
replMode :: IO ()
replMode = do
  putStrLn "Spinor REPL (step16)"
  (env, tyEnv) <- loadBoot primitiveBindings baseTypeEnv
  loop env tyEnv

-- | バッチ実行モード (引数でファイル指定)
--   モジュールシステム対応: module 宣言の有無に関わらず動作
batchMode :: FilePath -> IO ()
batchMode file = do
  -- Twister 環境をロード (従来通り)
  (env, _tyEnv) <- loadBoot primitiveBindings baseTypeEnv

  -- モジュールレジストリを作成
  registry <- newModuleRegistry

  -- ファイルの親ディレクトリをベースディレクトリとする
  cwd <- getCurrentDirectory
  let fileDir = takeDirectory file
      config = LoaderConfig
        { baseDir      = if null fileDir then cwd else fileDir
        , primitiveEnv = env
        }

  -- モジュールシステム経由でファイルを評価
  result <- evalFileWithModules config registry file
  case result of
    Left err -> do
      TIO.putStrLn $ "エラー: " <> err
      exitFailure
    Right (_, True)  -> putStrLn "\nAll tests passed."
    Right (_, False) -> do
      putStrLn "\nSome tests FAILED."
      exitFailure

-- | コンパイルモード: .spin ファイルを C 言語に変換
compileMode :: FilePath -> IO ()
compileMode file = do
  content <- readFileUtf8 file
  case parseFile content of
    Left err -> do
      putStrLn $ "パースエラー: " ++ err
      exitFailure
    Right exprs -> do
      let cCode = compileProgram exprs
      TIO.writeFile "output.c" cCode
      putStrLn "Compiled to output.c"

-- | ビルドモード: .spin ファイルからネイティブバイナリを生成
buildMode :: FilePath -> IO ()
buildMode file = do
  -- 1. パス設定
  let baseName = takeBaseName file
      cFile = replaceExtension file ".c"
      outFile = baseName
      runtimeSrc = "runtime/spinor.c"

  -- 2. Cコード生成
  putStrLn $ "Compiling " <> file <> " to " <> cFile <> "..."
  content <- readFileUtf8 file
  case parseFile content of
    Left err -> do
      putStrLn $ "Parse error: " ++ err
      exitFailure
    Right exprs -> do
      let cCode = compileProgram exprs
      TIO.writeFile cFile cCode

      -- 3. Cコンパイラ呼び出し
      gccPath <- findGcc
      putStrLn $ "Building " <> outFile <> " with " <> gccPath <> "..."
      (exitCode, out, err) <- readProcessWithExitCode gccPath ["-Iruntime", "-o", outFile, cFile, runtimeSrc] ""
      case exitCode of
        ExitSuccess -> do
          -- 4. クリーンアップと完了メッセージ
          doesFileExist cFile >>= \exists ->
            if exists then removeFile cFile else pure ()
          putStrLn $ "Build successful. Executable created: " <> outFile
          exitSuccess
        _ -> do
          putStrLn "Build failed. C compiler output:"
          putStrLn out
          putStrLn err
          exitFailure

-- | gcc を探す (MSYS2 UCRT/MINGW64 対応)
findGcc :: IO FilePath
findGcc = do
    let candidates =
          [ "gcc"  -- PATH にあればこれを使う
          , "C:\\msys64\\ucrt64\\bin\\gcc.exe"
          , "C:\\msys64\\mingw64\\bin\\gcc.exe"
          ]
    findFirst candidates
  where
    findFirst [] = pure "gcc"  -- フォールバック
    findFirst (c:cs) = do
      exists <- doesFileExist c
      if exists then pure c else findFirst cs

-- | 起動時に Twister ファイルをロードする
--   各式を展開 → 型推論 (ベストエフォート) → 評価し、
--   実行環境と型環境の両方を更新する
loadBoot :: Env -> TypeEnv -> IO (Env, TypeEnv)
loadBoot env tyEnv = do
  allExist <- mapM doesFileExist twisterFiles
  if not (and allExist)
    then do
      putStrLn "(Twister ファイルが見つかりません — スキップ)"
      pure (env, tyEnv)
    else do
      putStrLn "Loading Twister environment..."
      (env', tyEnv') <- foldlM' loadSpinFile (env, tyEnv) twisterFiles
      putStrLn "Twister loaded."
      pure (env', tyEnv')

-- | 単一の .spin ファイルをロードする (UTF-8)
loadSpinFile :: (Env, TypeEnv) -> FilePath -> IO (Env, TypeEnv)
loadSpinFile (env, tyEnv) path = do
  content <- readFileUtf8 path
  case parseFile content of
    Left err -> do
      putStrLn $ path ++ " パースエラー: " ++ err
      pure (env, tyEnv)
    Right exprs -> foldlM' processBootExpr (env, tyEnv) exprs

-- | boot 中の単一式を処理する
--   1. module / import 宣言はスキップ (モジュールシステム用)
--   2. マクロ展開
--   3. 型推論 (失敗しても続行)
--   4. 評価
processBootExpr :: (Env, TypeEnv) -> Expr -> IO (Env, TypeEnv)
processBootExpr (env, tyEnv) expr = case expr of
  -- module 宣言はスキップ
  EModule _ _ -> pure (env, tyEnv)
  -- import 宣言はスキップ (boot では全て同じ環境で実行されるため不要)
  EImport _ _ -> pure (env, tyEnv)
  -- 通常の式
  _ -> do
    -- 1. マクロ展開
    expandResult <- runEval env (expand expr)
    case expandResult of
      Left _ -> do
        -- 展開失敗: 評価のみ試行
        evalResult <- runEval env (evalExpr expr)
        case evalResult of
          Left _           -> pure (env, tyEnv)
          Right (_, env')  -> pure (env', tyEnv)
      Right (expanded, envAfterExpand) -> do
        -- 2. 型推論 (ベストエフォート)
        let tyEnv' = case runInfer (inferTop tyEnv expanded) of
                       Right (te, _, _) -> te
                       Left _           -> tyEnv
        -- 3. 評価
        evalResult <- runEval envAfterExpand (eval expanded)
        case evalResult of
          Left _           -> pure (envAfterExpand, tyEnv')
          Right (_, env')  -> pure (env', tyEnv')

-- | 式を展開してから評価する (Eval モナド内)
evalExpr :: Expr -> Eval ()
evalExpr expr = expand expr >>= eval >> pure ()

-- | 厳密な左畳み込み (IO 版)
foldlM' :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM' _ acc []     = pure acc
foldlM' f acc (x:xs) = do
  acc' <- f acc x
  foldlM' f acc' xs

loop :: Env -> TypeEnv -> IO ()
loop env tyEnv = do
  putStr "spinor> "
  hFlush stdout
  eof <- hIsEOF stdin
  if eof
    then putStrLn "\nBye."
    else do
      line <- TIO.getLine
      if T.null (T.strip line)
        then loop env tyEnv
        else do
          case readExpr line of
            Left err -> do
              putStrLn err
              loop env tyEnv
            Right ast -> do
              -- 1. マクロ展開
              expandResult <- runEval env (expand ast)
              case expandResult of
                Left err -> do
                  TIO.putStrLn $ "展開エラー: " <> err
                  loop env tyEnv
                Right (expanded, _) -> do
                  -- 2. 型推論 (inferTop で型環境も更新)
                  case runInfer (inferTop tyEnv expanded) of
                    Left tyErr -> do
                      -- 型推論失敗: 型エラーを表示し、実行しない
                      TIO.putStrLn $ "型エラー: " <> tyErr
                      loop env tyEnv
                    Right (tyEnv', subst, ty) -> do
                      -- 型推論成功: 型を表示してから評価
                      let finalType = apply subst ty
                      TIO.putStrLn $ ":: " <> showType finalType
                      result <- runEval env (expand ast >>= eval)
                      case result of
                        Left err -> do
                          TIO.putStrLn $ "エラー: " <> err
                          loop env tyEnv'
                        Right (val, env') -> do
                          putStrLn (show val)
                          loop env' tyEnv'

-- | UTF-8 でファイルを読み込む (Windows 対応)
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = do
  bs <- BS.readFile path
  pure $ TE.decodeUtf8 bs
