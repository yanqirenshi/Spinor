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
    (if (nil? keys)
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

## 4. HTTP クライアント (Web API 連携)

HTTP/HTTPS リクエストを送信して Web API と連携するパターンです。

### 基本パターン: GET リクエスト

```lisp
;; twister/http モジュールをロード
(require 'twister/http)

;; シンプルな GET リクエスト
(def response (http-get "https://httpbin.org/get"))

;; レスポンスからステータスコードとボディを取得
(http-status response)  ; => 200
(http-body response)    ; => "{\"args\": {}, ...}"

;; JSON レスポンスのパース
(def data (json-parse (http-body response)))
```

### 応用例: POST リクエスト (JSON 送信)

```lisp
(require 'twister/http)

;; JSON データを POST
(def user-data '(("name" "Alice") ("age" 30)))
(def response
  (http-post "https://httpbin.org/post"
    :body (json-stringify user-data)
    :headers '((:Content-Type "application/json"))))

;; レスポンスを確認
(if (= (http-status response) 200)
    (print "成功!")
    (print "エラー発生"))
```

### 実践例: REST API クライアント

```lisp
(require 'twister/http)

;; 汎用 API クライアント
(def api-client
  (fn (base-url)
    (list
      ;; GET メソッド
      (cons 'get
        (fn (path &key headers)
          (def url (string-append base-url path))
          (http-get url :headers headers)))

      ;; POST メソッド
      (cons 'post
        (fn (path data &key headers)
          (def url (string-append base-url path))
          (def all-headers
            (cons '(:Content-Type "application/json") headers))
          (http-post url
            :body (json-stringify data)
            :headers all-headers)))

      ;; DELETE メソッド
      (cons 'delete
        (fn (path &key headers)
          (def url (string-append base-url path))
          (http-request url :method "DELETE" :headers headers))))))

;; 使用例
(def client (api-client "https://api.example.com"))
(def get-fn (cdr (assoc 'get client)))
(def post-fn (cdr (assoc 'post client)))

;; ユーザー一覧を取得
(get-fn "/users")

;; 新しいユーザーを作成
(post-fn "/users" '(("name" "Bob") ("email" "bob@example.com")))
```

### 実践例: エラーハンドリング付き API 呼び出し

```lisp
(require 'twister/http)

;; 安全な API 呼び出し
(def safe-api-call
  (fn (url &key (retries 3))
    (handler-case
      (begin
        (def response (http-get url))
        (def status (http-status response))
        (if (>= status 400)
            ;; HTTP エラー
            (error (string-append "HTTP error: " (number->string status)))
            ;; 成功
            (json-parse (http-body response))))
      (error (msg)
        (if (> retries 0)
            (begin
              (print (string-append "リトライ中... 残り: " (number->string retries)))
              (sleep 1000)
              (safe-api-call url :retries (- retries 1)))
            (begin
              (print (string-append "最終エラー: " msg))
              nil))))))

;; 使用例
(safe-api-call "https://api.example.com/data" :retries 3)
```

---

## 5. HTTP サーバー (Web Server)

Spinor の Socket API を使った軽量 HTTP サーバーの構築パターンです。

### 基本パターン: Hello World サーバー

```lisp
;; シンプルな Hello World HTTP サーバー

;; ハンドラ関数: リクエストを受け取り、レスポンスを返す
(def hello-handler
  (fn (request)
    (list (list "status" 200)
          (list "headers" (list (list "Content-Type" "text/plain")
                                (list "Server" "Spinor/0.1")))
          (list "body" "Hello, Spinor!"))))

;; サーバーを起動 (ポート 8080)
(http-serve 8080 hello-handler)

;; テスト: curl http://localhost:8080/
;; 出力: Hello, Spinor!
```

### 応用例: パスに基づくルーティング

```lisp
;; パスに応じて異なるレスポンスを返すルーター

(def router
  (fn (request)
    (let ((path (http-assoc-val "path" request "/")))
      (if (equal path "/")
          ;; トップページ
          (list (list "status" 200)
                (list "headers" (list (list "Content-Type" "text/html")))
                (list "body" "<h1>Welcome to Spinor!</h1>"))

      (if (equal path "/api/hello")
          ;; JSON API エンドポイント
          (list (list "status" 200)
                (list "headers" (list (list "Content-Type" "application/json")))
                (list "body" "{\"message\": \"Hello, API!\"}"))

      ;; 404 Not Found
      (list (list "status" 404)
            (list "headers" (list (list "Content-Type" "text/plain")))
            (list "body" "Not Found")))))))

(http-serve 8080 router)
```

### 実践例: JSON API サーバー

```lisp
;; ユーザー情報を返す REST API サーバー

;; 仮のデータベース
(def users
  (list (list "id" 1 "name" "Alice" "email" "alice@example.com")
        (list "id" 2 "name" "Bob"   "email" "bob@example.com")))

;; ユーザーを検索
(def find-user
  (fn (id)
    (let ((find-by-id
           (fn (users id)
             (if (nil? users)
                 nil
                 (if (= (http-assoc-val "id" (car users) 0) id)
                     (car users)
                     (find-by-id (cdr users) id))))))
      (find-by-id users id))))

;; API ハンドラ
(def api-handler
  (fn (request)
    (let ((path   (http-assoc-val "path" request "/"))
          (method (http-assoc-val "method" request "GET")))

      ;; GET /api/users - 全ユーザー一覧
      (if (equal path "/api/users")
          (list (list "status" 200)
                (list "headers" (list (list "Content-Type" "application/json")))
                (list "body" (json-stringify users)))

      ;; GET /api/users/1 - 特定ユーザー
      (if (string-starts-with? path "/api/users/")
          (let ((id-str (substring path 11 (string-length path)))
                (id (string->number id-str)))
            (let ((user (find-user id)))
              (if user
                  (list (list "status" 200)
                        (list "headers" (list (list "Content-Type" "application/json")))
                        (list "body" (json-stringify user)))
                  (list (list "status" 404)
                        (list "body" "{\"error\": \"User not found\"}")))))

      ;; 404 for other paths
      (list (list "status" 404)
            (list "headers" (list (list "Content-Type" "application/json")))
            (list "body" "{\"error\": \"Not Found\"}")))))))

(print "Starting JSON API server on port 3000...")
(http-serve 3000 api-handler)
```

### リクエスト/レスポンス形式

```lisp
;; Request Alist の構造:
;; ((method "GET")
;;  (path "/api/users")
;;  (version "HTTP/1.1")
;;  (headers (("host" "localhost") ("user-agent" "curl/8.0")))
;;  (body ""))

;; Response Alist の構造:
;; ((status 200)
;;  (headers (("Content-Type" "text/plain") ("Server" "Spinor")))
;;  (body "Response body here"))

;; リクエストからフィールドを取得
(http-assoc-val "method" request "GET")   ; メソッド
(http-assoc-val "path" request "/")       ; パス
(http-assoc-val "body" request "")        ; ボディ

;; ヘッダーを取得
(let ((headers (http-assoc-val "headers" request nil)))
  (http-assoc-val "content-type" headers ""))
```

---

## 6. 科学技術計算と GPU 演算 (Matrix & OpenCL)

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
