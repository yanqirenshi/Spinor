# Step 29: TCP Socket Server - 実装指示書

## 概要

このタスクでは、Spinor を SLY/SLIME から利用するための基盤として、TCP ソケットサーバーを実装します。クライアントから送信された S式 を評価し、結果を返す単純な REPL サーバーを作成してください。

仕様書 `specs/29_socket.md` に基づき、以下の手順で実装を進めてください。

## Prerequisites (事前準備)

このタスクは TCP ソケット通信を扱うため、`network` パッケージへの依存を追加する必要があります。

1.  **`package.yaml` の修正:**
    -   `executables` セクションの `spinor` の `dependencies` リストに `network` を追加してください。

    ```yaml
    # package.yaml

    executables:
      spinor:
        main:                Main.hs
        source-dirs:         app
        ghc-options:
        - -threaded
        - -rtsopts
        - -with-rtsopts=-N
        dependencies:
        - Spinor
        - text
        - bytestring
        - directory
        - filepath
        - process
        - network   # <--- これを追加
    ```

## Steps (実装手順)

### 1. `src/Spinor/Server.hs` の作成

サーバー関連のロジックをカプセル化するため、新しいモジュールを作成します。

1.  **ファイル:** `src/Spinor/Server.hs` を新規作成します。
2.  **モジュール定義:**
    ```haskell
    module Spinor.Server (runServer) where

    import Network.Socket
    import Control.Concurrent (forkFinally)
    import Control.Monad (forever, void)
    import System.IO (IOMode(..), hSetBuffering, hGetLine, hPutStrLn, BufferMode(..))
    import qualified Data.Text as T
    import qualified Data.Text.IO as TIO
    import Data.Text.Encoding (encodeUtf8, decodeUtf8)
    import Control.Exception (try, SomeException, bracket)

    import Spinor.Syntax (readExpr)
    import Spinor.Eval (runEval, eval)
    import Spinor.Val (Env, showVal)
    import Spinor.Primitive (primitiveBindings)

    runServer :: String -> Env -> IO ()
    runServer port initialEnv = -- ...
    ```
3.  **`runServer` の実装:**
    -   `getAddrInfo` でアドレス情報を取得します。
    -   `bracket` を使って、ソケットの確保とクリーンなクローズを保証します。
    -   `bind`, `listen` を呼び出し、ポートを待ち受け状態にします。
    -   `forever` ループ内で `accept` を呼び出し、クライアント接続を待ちます。
    -   接続が確立したら、`forkFinally` を使って新しいスレッドで `handleClient` を実行します。`forkFinally` を使うことで、クライアント処理が終了（または失敗）した際にソケットを確実に閉じることができます。
4.  **`handleClient` の実装:**
    -   `socketToHandle` を使って、ソケットを `Handle` に変換し、テキストベースの I/O を行いやすくします。
    -   `hSetBuffering` で行バッファリングを設定します。
    -   `app/Main.hs` の `loop` 関数を参考に、クライアントからの入力を一行ずつ受け取り、評価して結果を返すループを実装します。`Env` はループ内で状態が更新されていくようにします。
    -   `readExpr`, `runEval`, `eval`, `showVal` を組み合わせて、リクエストの処理とレスポンスの生成を行います。
    -   エラーハンドリング: `runEval` の `Either` の結果を適切に処理し、成功時もエラー時もクライアントに文字列を返します。

### 2. `app/Main.hs` の拡張

作成したサーバーモジュールを呼び出すためのコマンドラインインターフェースを `main` 関数に追加します。

1.  **インポート:** `Spinor.Server` をインポートします。
2.  **`main` 関数の拡張:**
    -   引数リストのパターンマッチに `("server" : rest)` のケースを追加します。
    -   `rest` をパースして `--port` オプションを探し、ポート番号を取得します。指定がなければ `4005` をデフォルト値とします。
    -   `loadBoot` を呼び出して `twister` 環境をロードし、初期 `Env` を準備します。
    -   `runServer port initialEnv` を呼び出してサーバーを起動します。

    ```haskell
    -- main 関数の case 内
    ("server" : args) -> do
      let port = case args of
                   ["--port", p] -> p
                   _             -> "4005"
      putStrLn $ "Starting Spinor server on port " ++ port
      (env, _) <- loadBoot primitiveBindings baseTypeEnv
      runServer port env
    ```

### 3. 動作確認

`nc` (netcat) や `telnet` を使って、サーバーの動作を確認します。

1.  **サーバーの起動:**
    ```sh
    cabal run spinor -- server --port 4005
    ```
2.  **クライアントの接続 (別のターミナルで):**
    ```sh
    nc localhost 4005
    ```
3.  **対話:**
    -   `(+ 10 20)` と入力し、Enter キーを押します。`30` が返ってくることを確認します。
    -   `(define x 100)` と入力します。`100` が返ってくることを確認します。
    -   `(+ x 5)` と入力します。`105` が返ってくることを確認します。
    -   `(foobar)` と入力します。`エラー: 未定義のシンボル: foobar` のようなメッセージが返ってくることを確認します。
    -   クライアント側で `Ctrl+C` を押して接続を終了します。

---

## 実装報告

**実装完了後、この Markdown ファイルを直接編集し、以下の2つのセクションを追記して実装内容を報告してください。**

#### 実装方針

1. **モジュール構成:** `Spinor.Server` モジュールをライブラリ (`src/Spinor/Server.hs`) に配置し、サーバーロジックをカプセル化。これにより将来的なテストや拡張が容易になる。

2. **セッション管理:** 各クライアント接続に対して独立した `Env` を維持。`forkFinally` で新しいスレッドを生成し、クライアント終了時にソケットを確実にクローズ。

3. **エラーハンドリング:** パースエラーと評価エラーの両方を捕捉し、サーバーをクラッシュさせずにエラーメッセージをクライアントに返却。

4. **マクロ展開:** REPL と同様に `expand` を適用してからevalすることで、サーバー経由でもマクロが正しく動作。

#### 実装内容

**変更ファイル:**

1. **`spinor.cabal`**
   - `library` セクションに `Spinor.Server` モジュールを追加
   - `network >= 3.1 && < 4` 依存を追加
   - `cabal.project` ファイルを追加して Windows での network パッケージビルドをサポート

2. **`src/Spinor/Server.hs`** (新規作成)
   - `runServer :: String -> Env -> IO ()` - TCP サーバーのメインエントリポイント
   - `handleClient :: Socket -> Env -> IO ()` - クライアント接続の処理
   - `clientLoop :: Handle -> Env -> IO ()` - REPL ループの実装

3. **`app/Main.hs`**
   - `Spinor.Server` をインポート
   - `serverMode :: [String] -> IO ()` 関数を追加
   - `main` の case 文に `("server" : rest)` パターンを追加

**動作確認結果:**
```
(+ 10 20)      → 30
(define x 100) → 100
(+ x 5)        → 105
(未定義シンボル) → エラー: 未定義のシンボル: ...
```

**注意点:**
- Windows 環境では `network` パッケージのビルドに GHCup の MSYS2 bash を使用する必要がある場合がある
