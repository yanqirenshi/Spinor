# Step 20: 並行処理 (Concurrency) 仕様書

## 概要

Spinor に軽量スレッド (Green Threads) と、スレッド間通信のための同期プリミティブ (MVar) を導入する。
Haskell の GHC Runtime System (RTS) の強力な並行処理能力を、Spinor 言語レベルで利用できるようにする。

## 1. データ型 (Val.hs)

### VMVar

Haskell の `MVar Val` をラップする新しい値型。

- **型名:** `VMVar`
- **構文:** ユーザーがリテラルとして書くことはできない。`new-mvar` 等の関数経由で生成される。
- **表示:** `<mvar>`
- **等価性:** `MVar` は Haskell の `Eq` クラスを持たないため、`VMVar` 同士の比較は常に `False` (あるいは実行時エラー) とする。

## 2. 構文とプリミティブ

### スレッド操作

#### `(spawn expr)` [Special Form]

- **形式:** 特殊形式 (Special Form)。関数ではないため、引数は即座に評価されない。
- **動作:**
    1. 新しい軽量スレッド (Haskell thread) を作成する。
    2. そのスレッド内で `expr` を評価する。
    3. メインスレッドは即座に `#t` (将来的に ThreadID になる可能性あり) を返して続行する。
    4. エラー処理: スレッド内でエラーが発生した場合、そのスレッドのみが終了し、メインスレッドには影響しない。
- **戻り値:** `#t`

#### `(sleep millis)` [Primitive]

- **引数:** ミリ秒単位の整数 (`Int`)。
- **動作:** 指定された時間だけ現在のスレッドを停止する。Haskell の `threadDelay` を使用 (引数はマイクロ秒なので 1000倍する)。
- **戻り値:** `#t`

### 同期・通信 (MVar)

#### `(new-mvar [initial-val])` [Primitive]

- **引数:** オプションで初期値。
- **動作:**
    - 引数なし: 空の MVar を作成する (`newEmptyMVar`)。
    - 引数あり: 初期値が入った MVar を作成する (`newMVar`)。
- **戻り値:** `VMVar` オブジェクト。

#### `(take-mvar mvar)` [Primitive]

- **引数:** `VMVar` オブジェクト。
- **動作:**
    - MVar が空の場合: 値が入るまで**ブロック** (待機) する。
    - MVar に値がある場合: 値を取り出し、MVar を空にする。
- **戻り値:** 取り出した値。

#### `(put-mvar mvar val)` [Primitive]

- **引数:** `VMVar` オブジェクト、格納する値。
- **動作:**
    - MVar に値がある場合: 空になるまで**ブロック** (待機) する。
    - MVar が空の場合: 値を格納する。
- **戻り値:** `#t`

## 3. 使用例

```lisp
(module main (export))

;; MVar を作成
(define m (new-mvar))

;; 別スレッドで重い処理を実行
(spawn
  (progn
    (sleep 1000)        ; 1秒待機
    (put-mvar m 42)))   ; 結果を書き込む

(print "Waiting...")
(print (take-mvar m))   ; 値が来るまで待機 -> 42
```
