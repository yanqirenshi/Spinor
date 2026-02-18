# Step 31: SLY Advanced (Interactive Evaluation) - 実装指示書

## 概要

このタスクでは、Step 30 で実装した Swank サーバーを拡張し、SLY/Emacs からのインタラクティブな評価リクエストに応答できるようにします。具体的には、`swank:interactive-eval` と `swank:compile-string-for-emacs` の2つの主要な RPC を実装します。

## Steps (実装手順)

### 1. ディスパッチャの拡張 (`src/Spinor/Server.hs`)

`handleSwankRequest` 関数に、新しい RPC のためのハンドラを追加します。

1.  **パターンマッチの追加:**
    -   `handleSwankRequest` 内の `case form of` に、以下の2つのパターンマッチを追加してください。

    ```haskell
    -- (handleSwankRequest 内)
    ...
    -- SLY REPL からの評価リクエスト
    EList [ESym "swank:interactive-eval", EStr code] ->
        evalAndRespondString h env code reqId

    -- Emacs バッファからのコンパイルリクエスト
    EList (ESym "swank:compile-string-for-emacs" : EStr code : _) ->
        compileAndRespondString h env code reqId
    ...
    ```

### 2. 評価ロジックの実装

上記で追加した RPC の実体を実装します。

1.  **`evalAndRespondString` 関数の実装:**
    -   この関数は `swank:interactive-eval` のロジックを担当します。
    -   文字列 `code` を受け取り、`readExpr` -> `expand` -> `eval` の順で実行します。
    -   成功した場合、評価結果の `Val` を `showVal` で文字列に変換し、`(:return (:ok "result-string") reqId)` の形式でレスポンスを返します。
    -   パースや評価でエラーが発生した場合は、`(:return (:abort "error-msg") reqId)` を返します。
    -   状態が更新された `Env` を返り値として、次のループに引き継げるようにしてください。

    ```haskell
    evalAndRespondString :: Handle -> Env -> Text -> Integer -> IO Env
    evalAndRespondString h env code reqId = do
        result <- runEval env (readExpr code >>= expand >>= eval)
        case result of
            Left err -> do
                let response = mkAbortResponse reqId err
                sendPacket h (exprToText response)
                pure env
            Right (val, env') -> do
                let resultStr = T.pack (showVal val)
                let response = mkOkResponse reqId (EStr resultStr)
                sendPacket h (exprToText response)
                pure env'
    ```

2.  **`compileAndRespondString` 関数の実装:**
    -   この関数は `swank:compile-string-for-emacs` のロジックを担当します。
    -   動作は `evalAndRespondString` とほぼ同じですが、成功時のレスポンス形式が異なります。
    -   評価が成功した場合、`(:return (:ok ("Compilation finished." t)) reqId)` のような固定の成功メッセージを返してください。

    ```haskell
    compileAndRespondString :: Handle -> Env -> Text -> Integer -> IO Env
    compileAndRespondString h env code reqId = do
        -- 評価ロジックは evalAndRespondString と共通化可能
        result <- runEval env (readExpr code >>= expand >>= eval)
        case result of
            Left err -> do
                let response = mkAbortResponse reqId err
                sendPacket h (exprToText response)
                pure env
            Right (_, env') -> do
                -- 成功時は固定メッセージを返す
                let successMsg = EList [EStr "Compilation finished.", ESym "t"]
                let response = mkOkResponse reqId successMsg
                sendPacket h (exprToText response)
                pure env'
    ```
    *ヒント: `evalAndRespondString` と `compileAndRespondString` はロジックが似ているため、共通のヘルパー関数に括り出すリファクタリングを検討してください。*

### 3. (任意) 標準出力のキャプチャ

**このステップは努力目標です。まずは上記 1, 2 の RPC 実装を優先してください。**

-   **課題:** `(print "foo")` のような式の出力を、評価結果とは別に、非同期メッセージ `(:write-string "foo
")` として SLY に送信する必要があります。
-   **アプローチ案:** `Eval` モナドの定義 (`newtype Eval a = ...`) を変更し、出力用の関数 `(Text -> IO ())` を状態として持たせることを検討します。
    -   サーバー起動時に、`sendPacket` を使って `(:write-string ...)` を送信する関数を `Eval` モナドに渡します。
    -   `print` プリミティブの実装を、この出力用関数を呼び出すように変更します。
-   この改修は影響範囲が広いため、今回はスキップし、次以降のステップで取り組んでも構いません。

### 4. 動作確認

1.  **`netcat` でのテスト:**
    -   `cabal run spinor -- server` でサーバーを起動します。
    -   `nc localhost 4005` で接続します。
    -   以下のパケットを送信し、期待する応答が返るか確認します。
        -   `000035(:emacs-rex (swank:interactive-eval "(+ 10 20)") "user" t 2)`
        -   期待する応答 (例): `000018(:return(:ok "30") 2)`
        -   `000057(:emacs-rex (swank:compile-string-for-emacs "(defun f () 42)") "user" t 3)`
        -   期待する応答 (例): `00002A(:return(:ok("Compilation finished." t))3)`

2.  **Emacs SLY でのテスト:**
    -   `M-x sly-connect` で `localhost:4005` に接続します。
    -   **REPL での評価:** REPL バッファで `(+ 100 200)` と入力し `C-j` (または Enter)。`"300"` と表示されることを確認します。
    -   **バッファからのコンパイル:**
        -   `test.spin` のようなファイルを開きます。
        -   `(defun my-add (x y) (+ x y))` と記述します。
        -   関数定義の上で `C-c C-c` (sly-compile-defun) を実行します。ミニバッファに "Compilation finished." と表示されることを確認します。
    -   **`C-x C-e` での評価:**
        -   バッファ内で `(my-add 1 2)` と記述し、式の末尾で `C-x C-e` (sly-eval-last-expression) を実行します。
        -   ミニバッファに `"3"` と表示されることを確認します。

---

### 実装報告

**実装完了後、この Markdown ファイルを直接編集し、以下の2つのセクションを追記して実装内容を報告してください。**

#### Implementation Policy (実装方針)

*(ここに、実装する上で考慮した点や設計判断、採用したアプローチなどを記述してください)*

#### Implementation Details (実装内容)

*(ここに、具体的なコードの変更点や追加した関数の役割、苦労した点などを記述してください)*
