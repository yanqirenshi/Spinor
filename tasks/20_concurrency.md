# Task 20: 並行処理 (Concurrency) の実装

## 1. 概要

仕様書 `specs/20_concurrency.md` に基づき、Spinor に `spawn`, `sleep`, `MVar` 関連の機能を実装する。
Haskell の `Control.Concurrent` モジュールを使用する。

## 2. 実装ステップ

### Step 1: データ型の拡張 (`src/Spinor/Val.hs`)

1.  **インポート追加:**
    ```haskell
    import Control.Concurrent.MVar (MVar)
    ```

2.  **Val 型の拡張:**
    `VMVar` コンストラクタを追加する。
    ```haskell
    data Val
      = ...
      | VMVar (MVar Val) -- ^ 同期変数
    ```

3.  **Eq インスタンスの修正:**
    Haskell の `MVar` は `Eq` を導出できないため、`data Val` から `deriving Eq` を削除し、手動で `instance Eq Val` を実装する必要がある。
    * 既存の型 (`VInt`, `VBool` 等) は通常通り比較。
    * `VMVar` 同士の比較は常に `False` を返すように実装する。

4.  **Show インスタンスの更新:**
    `showVal (VMVar _) = "<mvar>"`

### Step 2: 評価器の拡張 (`src/Spinor/Eval.hs`)

1.  **インポート追加:**
    ```haskell
    import Control.Concurrent (forkIO, threadDelay)
    import Control.Concurrent.MVar
    import Control.Monad (void)
    import Control.Monad.IO.Class (liftIO)
    ```

2.  **特殊形式 `spawn` の実装:**
    `eval` 関数に `spawn` のパターンマッチを追加する。
    ```haskell
    eval (EList [ESym "spawn", expr]) = do
        env <- ask -- 現在の環境をキャプチャ (クロージャと同じ原理)
        liftIO $ void $ forkIO $ do
            -- 新しいスレッドで評価を実行。エラーは無視または表示。
            -- 注意: runEval の実装に合わせて適切なランナーを呼ぶこと。
            _ <- runEval env (eval expr)
            pure ()
        pure $ VBool True
    ```

3.  **プリミティブ関数の追加:**
    `primitiveEnv` に以下の関数を追加する。

    * `sleep`: `threadDelay` (マイクロ秒) を使用。引数を 1000 倍する。
    * `new-mvar`: 引数の数を見て `newEmptyMVar` か `newMVar` を呼ぶ。
    * `take-mvar`: `takeMVar` を呼ぶ。引数が `VMVar` かチェックする。
    * `put-mvar`: `putMVar` を呼ぶ。

### Step 3: 動作確認

以下の内容で `test-concurrency.spin` を作成し、動作を確認する。

```lisp
(module main (export))

(print "Start")
(define box (new-mvar))

(spawn
  (progn
    (sleep 500)
    (print "Thread: Waking up and putting value...")
    (put-mvar box 100)))

(print "Main: Waiting for box...")
(define val (take-mvar box))
(print "Main: Got value!")

(if (= val 100)
    (print "Test Passed")
    (print "Test Failed"))
```

---

## 3. 実装方針

### 設計判断

1. **特殊形式として実装**
   - 当初のタスク指示では `sleep`, `new-mvar`, `take-mvar`, `put-mvar` を `primitiveEnv` に追加する案だったが、既存の `VPrim` 型は純粋関数 (`[Val] -> Either Text Val`) のみをサポートしていた。
   - MVar 操作は IO を伴うため、これらを全て `Eval.hs` 内の**特殊形式**として実装することで、既存のプリミティブシステムを変更せずに済む設計を採用した。

2. **begin/progn の追加**
   - `spawn` 内で複数の式を順次実行するために `begin` / `progn` 特殊形式を追加した。
   - これにより、関数本体で複数の式を実行する際に `(begin ...)` で囲むことで対応可能となった。

3. **UTF-8 対応**
   - Windows 環境でのファイル読み込み時にエンコーディング問題が発生したため、`Main.hs` と `Loader.hs` で UTF-8 を明示的に使用するよう修正した。

### スレッドエラーの扱い

- `spawn` で生成されたスレッド内でエラーが発生した場合、エラーメッセージを標準出力に表示し、そのスレッドのみが終了する。
- メインスレッドには影響しない設計。

---

## 4. 実装内容

### 変更ファイル一覧

| ファイル | 変更内容 |
|----------|----------|
| `src/Spinor/Val.hs` | `VMVar` コンストラクタ追加、`Eq`/`Show` インスタンス更新 |
| `src/Spinor/Eval.hs` | `spawn`, `sleep`, `new-mvar`, `take-mvar`, `put-mvar`, `begin`, `progn` 特殊形式を追加 |
| `src/Spinor/Loader.hs` | UTF-8 ファイル読み込み対応 |
| `app/Main.hs` | UTF-8 ファイル読み込み対応 |
| `spinor.cabal` | `bytestring` 依存追加 |

### Val.hs の変更

```haskell
import Control.Concurrent.MVar (MVar)

data Val
  = ...
  | VMVar (MVar Val)  -- 同期変数 (MVar)

instance Eq Val where
  ...
  VMVar _ == VMVar _ = False  -- MVar は参照のため常に不等

showVal (VMVar _) = "<mvar>"
```

### Eval.hs の変更

```haskell
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (void)

-- begin/progn: 複数の式を順次評価
eval (EList (ESym "begin" : exprs)) = evalSequence exprs
eval (EList (ESym "progn" : exprs)) = evalSequence exprs

-- spawn: 新しいスレッドで式を評価
eval (EList [ESym "spawn", expr]) = do
  env <- get
  liftIO $ void $ forkIO $ do
    result <- runEval env (eval expr)
    case result of
      Left err -> TIO.putStrLn $ "[spawn error] " <> err
      Right _  -> pure ()
  pure $ VBool True

-- sleep: 指定ミリ秒だけスレッドを停止
eval (EList [ESym "sleep", arg]) = do
  val <- eval arg
  case val of
    VInt ms -> do
      liftIO $ threadDelay (fromInteger ms * 1000)
      pure $ VBool True
    _ -> throwError "sleep: 整数が必要です"

-- new-mvar: MVar を作成
eval (EList [ESym "new-mvar"]) = VMVar <$> liftIO newEmptyMVar
eval (EList [ESym "new-mvar", arg]) = do
  val <- eval arg
  VMVar <$> liftIO (newMVar val)

-- take-mvar: MVar から値を取り出す (ブロッキング)
eval (EList [ESym "take-mvar", arg]) = do
  val <- eval arg
  case val of
    VMVar mvar -> liftIO $ takeMVar mvar
    _ -> throwError "take-mvar: MVar が必要です"

-- put-mvar: MVar に値を格納する (ブロッキング)
eval (EList [ESym "put-mvar", mvarExpr, valExpr]) = do
  mvarVal <- eval mvarExpr
  val <- eval valExpr
  case mvarVal of
    VMVar mvar -> liftIO (putMVar mvar val) >> pure (VBool True)
    _ -> throwError "put-mvar: 第1引数には MVar が必要です"

-- ヘルパー: 式リストを順次評価
evalSequence :: [Expr] -> Eval Val
evalSequence []     = pure VNil
evalSequence [e]    = eval e
evalSequence (e:es) = eval e >> evalSequence es
```

### 動作確認結果

```
Loading Twister environment...
Twister loaded.
Start
Main: Waiting for box...
Thread: Putting value...
Main: Got value!
Test Passed

All tests passed.
```

- `spawn` で生成されたスレッドが `sleep` 後に MVar へ値を書き込み、メインスレッドが `take-mvar` でブロッキング待機して値を受け取ることを確認。
