; http-server.spin — HTTP サーバーライブラリ
; Issue #7 で実装された Socket API を活用した高レベル HTTP サーバー

;; ===========================================================================
;; HTTP ステータスコード → テキスト
;; ===========================================================================

(def http-status-text
  (fn (code)
    (if (= code 200) "OK"
    (if (= code 201) "Created"
    (if (= code 204) "No Content"
    (if (= code 301) "Moved Permanently"
    (if (= code 302) "Found"
    (if (= code 304) "Not Modified"
    (if (= code 400) "Bad Request"
    (if (= code 401) "Unauthorized"
    (if (= code 403) "Forbidden"
    (if (= code 404) "Not Found"
    (if (= code 405) "Method Not Allowed"
    (if (= code 500) "Internal Server Error"
    (if (= code 502) "Bad Gateway"
    (if (= code 503) "Service Unavailable"
    "Unknown"))))))))))))))))

;; ===========================================================================
;; ユーティリティ関数
;; ===========================================================================

;; リストの長さ
(def http-list-length
  (fn (xs)
    (if (nil? xs) 0 (+ 1 (http-list-length (cdr xs))))))

;; リストの n 番目の要素を取得
(def http-nth
  (fn (n xs)
    (if (nil? xs) ""
    (if (= n 0) (car xs)
    (http-nth (- n 1) (cdr xs))))))

;; alist 内のキーを検索 (キーはリストの car)
;; alist: ((key1 val1) (key2 val2) ...)
(def http-assoc
  (fn (key alist)
    (if (nil? alist)
        nil
        (if (equal key (car (car alist)))
            (car alist)
            (http-assoc key (cdr alist))))))

;; alist から値を取得 (cadr)
(def http-assoc-val
  (fn (key alist default)
    (let ((pair (http-assoc key alist)))
      (if (nil? pair)
          default
          (car (cdr pair))))))

;; 整数を文字列に変換
(def http-int->string
  (fn (n)
    (if (< n 0)
        (string-append "-" (http-int->string-pos (- 0 n)))
        (http-int->string-pos n))))

(def http-int->string-pos
  (fn (n)
    (if (< n 10)
        (http-digit->char n)
        (string-append (http-int->string-pos (http-div n 10))
                       (http-digit->char (% n 10))))))

;; 整数除算
(def http-div
  (fn (a b)
    (if (< a b)
        0
        (+ 1 (http-div (- a b) b)))))

;; 1桁の数字を文字に変換
(def http-digit->char
  (fn (d)
    (if (= d 0) "0"
    (if (= d 1) "1"
    (if (= d 2) "2"
    (if (= d 3) "3"
    (if (= d 4) "4"
    (if (= d 5) "5"
    (if (= d 6) "6"
    (if (= d 7) "7"
    (if (= d 8) "8"
    (if (= d 9) "9"
    "?"))))))))))))

;; ===========================================================================
;; HTTP リクエストパーサー
;; ===========================================================================

;; リクエストライン ("GET /path HTTP/1.1") をパース
(def http-parse-request-line
  (fn (line)
    (let ((parts (string-split line " ")))
      (if (< (http-list-length parts) 3)
          (list "GET" "/" "HTTP/1.1")
          (list (http-nth 0 parts) (http-nth 1 parts) (http-nth 2 parts))))))

;; ヘッダー行をパースして (key val) のリストを返す
(def http-parse-header-line
  (fn (line)
    (let ((idx (string-index-of line ": ")))
      (if (< idx 0)
          nil
          (let ((key (string-downcase (string-trim (substring line 0 idx))))
                (val (string-trim (substring line (+ idx 2) (string-length line)))))
            (list key val))))))

;; ヘッダー行リストをパースして alist を構築
(def http-parse-headers
  (fn (lines)
    (if (nil? lines)
        nil
        (let ((pair (http-parse-header-line (car lines))))
          (if (nil? pair)
              (http-parse-headers (cdr lines))
              (cons pair (http-parse-headers (cdr lines))))))))

;; http-parse-request: 生文字列から Request Alist を構築
;; 戻り値: ((method "GET") (path "/") (version "HTTP/1.1") (headers (...)) (body ""))
(def http-parse-request
  (fn (raw)
    (let ((header-body-split (string-split raw "\r\n\r\n")))
      (let ((header-part (if (nil? header-body-split) "" (http-nth 0 header-body-split)))
            (body-part   (if (< (http-list-length header-body-split) 2) "" (http-nth 1 header-body-split))))
        (let ((lines (string-split header-part "\r\n")))
          (if (nil? lines)
              (list (list "method"  "GET")
                    (list "path"    "/")
                    (list "version" "HTTP/1.1")
                    (list "headers" nil)
                    (list "body"    ""))
              (let ((req-line-parts (http-parse-request-line (http-nth 0 lines)))
                    (header-lines   (cdr lines)))
                (list (list "method"  (http-nth 0 req-line-parts))
                      (list "path"    (http-nth 1 req-line-parts))
                      (list "version" (http-nth 2 req-line-parts))
                      (list "headers" (http-parse-headers header-lines))
                      (list "body"    body-part)))))))))

;; ===========================================================================
;; HTTP レスポンスビルダー
;; ===========================================================================

;; ヘッダー alist を HTTP ヘッダー文字列に変換
(def http-build-header-lines
  (fn (headers)
    (if (nil? headers)
        ""
        (let ((pair (car headers)))
          (string-append (http-nth 0 pair)
                         ": "
                         (http-nth 1 pair)
                         "\r\n"
                         (http-build-header-lines (cdr headers)))))))

;; http-build-response: Response Alist から HTTP レスポンス文字列を構築
;; 入力: ((status 200) (headers (("Content-Type" "text/plain"))) (body "Hello"))
(def http-build-response
  (fn (response)
    (let ((status  (http-assoc-val "status" response 200))
          (headers (http-assoc-val "headers" response nil))
          (body    (http-assoc-val "body" response "")))
      (let ((body-len (string-length body)))
        (let ((headers-with-length (http-add-content-length headers body-len)))
          (string-append "HTTP/1.1 "
                         (http-int->string status)
                         " "
                         (http-status-text status)
                         "\r\n"
                         (http-build-header-lines headers-with-length)
                         "\r\n"
                         body))))))

;; Content-Length ヘッダーを追加 (存在しない場合のみ)
(def http-add-content-length
  (fn (headers body-len)
    (if (nil? (http-assoc "content-length" headers))
        (cons (list "Content-Length" (http-int->string body-len)) headers)
        headers)))

;; ===========================================================================
;; HTTP サーバー本体
;; ===========================================================================

;; クライアント接続を処理する内部関数
(def http-handle-client
  (fn (client-sock handler-fn)
    (handler-case
      (begin
        ;; リクエスト受信
        (let ((raw-request (socket-recv client-sock 8192)))
          (if (= (string-length raw-request) 0)
              ;; 空のリクエスト (接続クローズ)
              (socket-close client-sock)
              ;; リクエスト処理
              (let ((request (http-parse-request raw-request)))
                (let ((response (handler-fn request)))
                  (let ((raw-response (http-build-response response)))
                    (begin
                      (socket-send client-sock raw-response)
                      (socket-close client-sock))))))))
      (error (e)
        ;; エラー発生時もソケットをクローズ
        (begin
          (print (string-append "HTTP Server Error: " e))
          (ignore-errors (socket-close client-sock)))))))

;; http-serve: HTTP サーバーを起動
(def http-serve
  (fn (port handler-fn)
    (let ((server-sock (socket-listen port)))
      (begin
        (print (string-append "HTTP Server listening on port " (http-int->string port)))
        (http-serve-loop server-sock handler-fn)))))

;; サーバーメインループ
(def http-serve-loop
  (fn (server-sock handler-fn)
    (handler-case
      (let ((accept-result (socket-accept server-sock)))
        (let ((client-sock (car accept-result)))
          (begin
            ;; 新しいスレッドでクライアントを処理
            (spawn (http-handle-client client-sock handler-fn))
            ;; 次の接続を待つ
            (http-serve-loop server-sock handler-fn))))
      (error (e)
        (begin
          (print (string-append "Accept Error: " e))
          ;; 短い遅延後に再試行
          (sleep 100)
          (http-serve-loop server-sock handler-fn))))))
