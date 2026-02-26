# Spinor Cookbook (逆引きレシピ集)

「やりたいこと」から逆引きできる、実践的なコード例集です。各レシピはそのままコピー＆ペーストして試すことができます。

---

## 1. 安全なファイル処理 (Resource Protection)

ファイル操作中にエラーが発生しても、必ず後処理（ログ出力やリソース解放）を実行するパターンです。

### 基本パターン: unwind-protect によるリソース保護

```lisp
;; unwind-protect を使った安全なファイル読み込み
;; エラーが発生しても必ずログが出力される

(unwind-protect
  ;; 保護されるメイン処理
  (begin
    (print "ファイル処理を開始...")
    (def content (read-file "data.txt"))
    (print (string-append "読み込み完了: " content))
    content)
  ;; 必ず実行されるクリーンアップ処理
  (print "処理完了 (成功/失敗に関わらず実行)"))
```

### 応用例: エラー時にデフォルト値を返す

```lisp
;; ファイルが存在しない場合はデフォルト値を返す
(def read-file-or-default
  (fn (path default-value)
    (handler-case
      (read-file path)
      (error (_)
        (print (string-append "警告: " path " が見つかりません"))
        default-value))))

;; 使用例
(read-file-or-default "config.txt" "{}")  ; ファイルがなければ "{}" を返す
```

### 実践例: 設定ファイルの安全な読み書き

```lisp
;; 設定の読み込み → 処理 → 保存 を安全に行う
(def process-config
  (fn (config-path)
    (unwind-protect
      (begin
        ;; 設定を読み込む (エラーハンドリング付き)
        (def config
          (handler-case
            (json-parse (read-file config-path))
            (error (_) '())))  ; パース失敗時は空リスト

        ;; 設定を処理 (例: バージョンを更新)
        (def updated-config
          (cons '("version" . "2.0") config))

        ;; 結果を保存
        (write-file config-path (json-stringify updated-config))
        (print "設定を更新しました"))

      ;; クリーンアップ: 処理完了をログに記録
      (print (string-append "config処理終了: " config-path)))))
```

---

## 2. JSON データの抽出と変換 (Web API Mockup)

JSON 文字列をパースし、必要なデータを抽出するパターンです。

### 基本パターン: JSON のパースとキー抽出

```lisp
;; JSON 文字列をパースして Alist に変換
(def json-str "{\"name\": \"Alice\", \"age\": 30, \"active\": true}")
(def data (json-parse json-str))

;; 結果: (("name" . "Alice") ("age" . 30) ("active" . #t))

;; 特定のキーを取得するヘルパー関数
(def get-value
  (fn (alist key)
    (def pair (assoc key alist))
    (if pair
        (cdr pair)
        nil)))

;; 使用例
(get-value data "name")    ; => "Alice"
(get-value data "age")     ; => 30
(get-value data "active")  ; => #t
```

### 応用例: ネストした JSON の処理

```lisp
;; ネストした JSON データ
(def api-response "{\"user\": {\"id\": 123, \"profile\": {\"name\": \"Bob\"}}}")
(def response (json-parse api-response))

;; ネストしたデータへのアクセス
(def get-nested
  (fn (data . keys)
    (if (null? keys)
        data
        (let ((current (get-value data (car keys))))
          (if current
              (apply get-nested (cons current (cdr keys)))
              nil)))))

;; 使用例: user.profile.name を取得
(get-nested response "user" "profile" "name")  ; => "Bob"
```

### 実践例: API レスポンスのバリデーション

```lisp
;; API レスポンスを検証して必要なフィールドを抽出
(def validate-user-response
  (fn (json-str)
    (handler-case
      (begin
        (def data (json-parse json-str))

        ;; 必須フィールドのチェック
        (def id (get-value data "id"))
        (def name (get-value data "name"))

        (if (and id name)
            ;; 成功: ユーザー情報を構造化して返す
            (list (cons 'id id) (cons 'name name) (cons 'valid #t))
            ;; 失敗: 必須フィールドがない
            (error "必須フィールドが不足しています")))

      (error (msg)
        ;; パースエラーまたはバリデーションエラー
        (list (cons 'valid #f) (cons 'error msg))))))

;; 使用例
(validate-user-response "{\"id\": 1, \"name\": \"Charlie\"}")
; => ((id . 1) (name . "Charlie") (valid . #t))

(validate-user-response "{\"id\": 1}")
; => ((valid . #f) (error . "必須フィールドが不足しています"))
```

---

## 3. 並行ワーカーパターン (Concurrency with MVar)

`spawn` と `MVar` を使用したスレッド間通信と状態同期のパターンです。

### 基本パターン: ワーカースレッドとの同期

```lisp
;; MVar を使った基本的なスレッド同期
;; メインスレッド: タスクを投げる
;; ワーカースレッド: 計算して結果を返す

;; 結果を受け取るための MVar を作成
(def result-mvar (new-mvar))

;; ワーカースレッドを起動
(spawn
  (begin
    ;; 重い計算をシミュレート
    (sleep 100)
    (def answer (* 21 2))
    ;; 結果を MVar に格納
    (put-mvar result-mvar answer)
    (print "ワーカー: 計算完了")))

(print "メイン: ワーカーを起動しました")

;; 結果を待つ (ブロッキング)
(def result (take-mvar result-mvar))
(print (string-append "メイン: 結果 = " (number->string result)))
; => "メイン: 結果 = 42"
```

### 応用例: 複数ワーカーの並列実行

```lisp
;; 複数のワーカーを並列実行し、結果を集約する

;; 各ワーカーの結果を格納する MVar のリスト
(def mvars (list (new-mvar) (new-mvar) (new-mvar)))

;; 3つのワーカーを起動
(def tasks '(10 20 30))

(dolist (pair (zip tasks mvars))
  (def task (car pair))
  (def mvar (cdr pair))
  (spawn
    (begin
      (sleep (* task 10))  ; タスクに応じた処理時間
      (put-mvar mvar (* task task)))))  ; task の2乗を計算

(print "全ワーカーを起動しました")

;; 全ての結果を収集
(def results
  (map (fn (mvar) (take-mvar mvar)) mvars))

(print results)  ; => (100 400 900)
```

### 実践例: プロデューサー・コンシューマーパターン

```lisp
;; キューとして機能する MVar を使った
;; プロデューサー・コンシューマーパターン

;; タスクキュー
(def task-queue (new-mvar))

;; コンシューマー (ワーカー) を起動
(spawn
  (begin
    (print "コンシューマー: 待機中...")
    ;; タスクを受け取って処理
    (dotimes (i 3)
      (def task (take-mvar task-queue))
      (print (string-append "処理中: " task))
      (sleep 50))
    (print "コンシューマー: 完了")))

;; プロデューサー: タスクを投入
(sleep 100)  ; コンシューマーの起動を待つ

(dolist (task '("タスクA" "タスクB" "タスクC"))
  (print (string-append "投入: " task))
  (put-mvar task-queue task)
  (sleep 20))

(print "プロデューサー: 全タスク投入完了")
```

---

## 4. 科学技術計算と GPU 演算 (Matrix & OpenCL)

行列の基本演算と、OpenCL を用いた高速並列計算のパターンです。

### 基本パターン: 行列の生成と基本演算

```lisp
;; 2x3 行列の生成 (row-major order)
(def A (matrix 2 3 '(1 2 3
                     4 5 6)))

;; 行列の次元を取得
(mdim A)  ; => (2 3)

;; 要素へのアクセス (0-indexed)
(mref A 0 0)  ; => 1.0
(mref A 1 2)  ; => 6.0

;; 3x2 行列
(def B (matrix 3 2 '(7  8
                     9  10
                     11 12)))

;; 行列積: A(2x3) * B(3x2) = C(2x2)
(def C (m* A B))
(mdim C)  ; => (2 2)
; C = ((1*7+2*9+3*11)  (1*8+2*10+3*12))
;     ((4*7+5*9+6*11)  (4*8+5*10+6*12))
; C = ((58 64) (139 154))
```

### 応用例: 転置と行列加算

```lisp
;; 行列の転置
(def D (matrix 2 3 '(1 2 3
                     4 5 6)))
(def D-T (transpose D))  ; => 3x2 行列

(mdim D)    ; => (2 3)
(mdim D-T)  ; => (3 2)

;; 行列加算 (同じ次元の行列同士)
(def E (matrix 2 2 '(1 2 3 4)))
(def F (matrix 2 2 '(5 6 7 8)))
(def G (m+ E F))
; G = ((6 8) (10 12))
```

### 実践例: OpenCL による GPU 並列計算

```lisp
;; OpenCL を使った大規模ベクトルの並列加算
;; 注意: OpenCL 環境が必要です

;; OpenCL の初期化
(def ctx (cl-init))

;; GPU カーネルのソースコード
(def kernel-src "
__kernel void vector_add(__global const float *a,
                         __global const float *b,
                         __global float *c,
                         const unsigned int n) {
    int i = get_global_id(0);
    if (i < n) {
        c[i] = a[i] + b[i];
    }
}")

;; カーネルをコンパイル
(def kernel (cl-compile ctx kernel-src "vector_add"))

;; 入力データ (行列として作成し、フラットなベクトルとして使用)
(def N 1024)
(def a-data (matrix 1 N (map (fn (x) x) (range N))))
(def b-data (matrix 1 N (map (fn (x) (* x 2)) (range N))))

;; GPU メモリにデータを転送
(def a-gpu (to-device ctx a-data))
(def b-gpu (to-device ctx b-data))
(def c-gpu (to-device ctx (matrix 1 N (map (fn (_) 0) (range N)))))

;; カーネルを実行
(cl-enqueue ctx kernel N a-gpu b-gpu c-gpu N)

;; 結果をホストに転送
(def result (to-host c-gpu))

;; 結果の確認 (最初の10要素)
(dotimes (i 10)
  (print (mref result 0 i)))
; => 0.0, 3.0, 6.0, 9.0, 12.0, ...  (a[i] + b[i] = i + 2*i = 3*i)
```

### 実践例: 線形代数 (逆行列と行列式)

```lisp
;; 2x2 行列の逆行列
(def M (matrix 2 2 '(4 7
                     2 6)))

;; 逆行列の計算
(def M-inv (inverse M))

;; 検証: M * M-inv = 単位行列
(def I (m* M M-inv))
(mref I 0 0)  ; => 1.0 (近似)
(mref I 0 1)  ; => 0.0 (近似)
(mref I 1 0)  ; => 0.0 (近似)
(mref I 1 1)  ; => 1.0 (近似)
```

---

## Tips & ベストプラクティス

### エラーハンドリングの組み合わせ

```lisp
;; handler-case + unwind-protect の組み合わせ
(handler-case
  (unwind-protect
    (risky-operation)
    (cleanup))
  (error (msg)
    (log-error msg)
    default-value))
```

### 関数型スタイルでのデータ変換

```lisp
;; map, filter, reduce を組み合わせたパイプライン処理
(def process-numbers
  (fn (numbers)
    (->> numbers
         (filter (fn (x) (> x 0)))      ; 正の数のみ
         (map (fn (x) (* x x)))          ; 2乗
         (reduce + 0))))                 ; 合計

(process-numbers '(-1 2 -3 4 5))  ; => 45 (4+16+25)
```

### デバッグのコツ

```lisp
;; print を使った値のトレース
(def debug
  (fn (label value)
    (print (string-append label ": " (if (string? value)
                                         value
                                         (number->string value))))
    value))  ; 値をそのまま返すので、式の中に挿入できる

;; 使用例
(+ 1 (debug "中間値" (* 2 3)))
; 出力: "中間値: 6"
; 結果: 7
```

---

## 関連ドキュメント

- [Control Flow](syntax/control-flow) - 条件分岐と制御構文
- [Conditions & Errors](syntax/conditions) - エラーハンドリング詳細
- [Packages & Modules](syntax/packages) - パッケージシステム
- [API Reference](api-index) - 全プリミティブ一覧
