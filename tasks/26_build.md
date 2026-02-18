# Step 26: スタンドアロン コンパイル - 実装指示書

## 概要

このタスクでは、`spinor build` コマンドを実装し、Spinor プログラムから直接ネイティブバイナリを生成する機能を追加します。`defun` のサポートが主な拡張点です。

仕様書 `specs/step26_build.md` に基づき、以下の手順で実装を進めてください。

## Implementation Policy (実装方針)

-   **C コンパイラの呼び出し:**
    -   Haskell の `System.Process` ライブラリ、特に `readProcessWithExitCode` 関数を使用し、`gcc` (または `clang`) を外部プロセスとして呼び出します。これにより、コンパイルの成否 (`ExitCode`) とエラー出力 (`stderr`) を取得できます。
-   **中間ファイルの扱い:**
    -   ビルドプロセス中に生成される `.c` ファイルは、C コンパイルが成功した場合は `System.Directory.removeFile` で削除します。
    -   コンパイルが失敗した場合は、ユーザーがデバッグできるよう `.c` ファイルを残します。
-   **パスの操作:**
    -   `System.FilePath` を利用して、入力ファイルパスから中間ファイルパスと最終的なバイナリ出力パスを安全に構築します。

## Implementation Details (実装内容)

### 1. `src/Spinor/Compiler/Codegen.hs` の拡張

`defun` と関数呼び出しをサポートするよう、コードジェネレータを大幅に拡張します。

1.  **`compileProgram` のリファクタリング:**
    -   `[Expr]` を受け取り、`defun` 式とそれ以外の式に分類します。`Data.List.partition` が便利です。
    -   `defun` 式を C の関数定義文字列のリストに変換します。
    -   その他の式を `main` 関数内の C の文のリストに変換します。
    -   これらを結合して、最終的な C ソースコードを生成します。

    ```haskell
    -- (ファイル先頭に追加)
    import Data.List (partition)
    import Data.Char (isAlphaNum)

    -- (既存の compileProgram を置き換え)
    compileProgram :: [Expr] -> CCode
    compileProgram exprs =
        let (defuns, others) = partition isDefun exprs
            funDefs = T.unlines (map compileFunDef defuns)
            mainStmts = T.unlines (map compileStmt others)
        in T.unlines
            [ "#include "runtime/spinor.h""
            , ""
            , funDefs
            , "int main(void) {"
            , mainStmts
            , "    return 0;"
            , "}"
            ]

    isDefun :: Expr -> Bool
    isDefun (EList (ESym "defun" : _)) = True
    isDefun _ = False
    ```

2.  **`compileFunDef` の新規作成:**
    -   `(defun name (args...) body)` の AST を受け取り、C の関数定義文字列を生成します。
    -   関数名には `mangle` 関数を適用してください。

    ```haskell
    -- (新規追加)
    compileFunDef :: Expr -> CCode
    compileFunDef (EList [ESym "defun", ESym name, EList args, body]) =
        let cName = mangle name
            cArgs = T.intercalate ", " (map toCArg args)
            cBody = compileExpr body
        in T.unlines
            [ "SpObject* " <> cName <> "(" <> cArgs <> ") {"
            , "    return " <> cBody <> ";"
            , "}"
            ]
      where
        toCArg (ESym argName) = "SpObject* " <> mangle argName
        toCArg _ = "" -- エラー処理 (省略)
    compileFunDef _ = "" -- エラー処理 (省略)
    ```

3.  **`compileExpr` の拡張:**
    -   関数呼び出し `(EList (ESym fname : args))` のパターンマッチを追加します。
    -   `fname` がプリミティブ（`+` など）でない場合、ユーザー定義関数呼び出しとみなし、`mangle` された C 関数を呼び出すコードを生成します。

    ```haskell
    -- (compileExpr 内に追加)
    -- ユーザー定義関数の呼び出し
    compileExpr (EList (ESym fname : args)) =
        let cFun = mangle fname
            cArgs = T.intercalate ", " (map compileExpr args)
        in cFun <> "(" <> cArgs <> ")"
    
    -- (compileExpr 内の変数参照を追加)
    compileExpr (ESym s) = mangle s
    ```

4.  **`mangle` 関数の作成:**
    -   ユーザー定義のシンボルを安全な C の識別子に変換します。

    ```haskell
    -- (新規追加)
    mangle :: Text -> CCode
    mangle name = "user_" <> T.map sanitize name
      where
        sanitize c | isAlphaNum c = c
                   | otherwise    = '_'
    ```

### 2. `app/Main.hs` の拡張

`build` サブコマンドを実装し、C コンパイラを呼び出すロジックを追加します。

1.  **インポートの追加:**
    ```habel
    import System.Process (readProcessWithExitCode)
    import System.FilePath (takeBaseName, replaceExtension, exeExtension)
    import System.Directory (removeFile)
    import System.Exit (exitSuccess)
    ```

2.  **`main` 関数の拡張:**
    -   `["build", file]` という引数パターンを追加し、`buildMode file` を呼び出します。

3.  **`buildMode` 関数の実装:**
    -   `FilePath` を操作して各パスを生成し、`Codegen` を呼び出し、`gcc` を実行し、後処理を行います。

    ```haskell
    -- (新規追加)
    buildMode :: FilePath -> IO ()
    buildMode file = do
      -- 1. パス設定
      let baseName = takeBaseName file
      let cFile = replaceExtension file ".c"
      let outFile = baseName
      let runtimeSrc = "runtime/spinor.c"

      -- 2. Cコード生成
      putStrLn $ "Compiling " <> file <> " to " <> cFile <> "..."
      content <- readFileUtf8 file
      case parseFile content of
        Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
        Right exprs -> do
          let cCode = compileProgram exprs
          TIO.writeFile cFile cCode

          -- 3. Cコンパイラ呼び出し
          putStrLn $ "Building " <> outFile <> " with gcc..."
          (exitCode, out, err) <- readProcessWithExitCode "gcc" ["-o", outFile, cFile, runtimeSrc] ""
          case exitCode of
            ExitSuccess -> do
              -- 4. クリーンアップと完了メッセージ
              removeFile cFile
              putStrLn $ "Build successful. Executable created: " <> outFile
              exitSuccess
            _ -> do
              putStrLn "Build failed. C compiler output:"
              putStrLn out
              putStrLn err
              exitFailure
    ```

## Verification (検証)

1.  **テスト用ファイル `test-build.spin` を作成:**
    ```lisp
    ; test-build.spin

    (defun add-100 (n)
      (+ n 100))

    (add-100 42)
    ```

2.  **`build` コマンドを実行:**
    ```sh
    cabal run spinor -- build test-build.spin
    ```

3.  **成功メッセージとバイナリを確認:**
    -   `Build successful.` というメッセージが表示されること。
    -   カレントディレクトリに `test-build` (または `test-build.exe`) が生成されていること。
    -   `test-build.c` は削除されていること。

4.  **生成されたバイナリを実行:**
    ```sh
    ./test-build
    ```
    -   実行結果として `142` が表示されれば成功です。

以上でタスクは完了です。

