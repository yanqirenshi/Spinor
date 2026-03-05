; http.spin — HTTP クライアントライブラリ
(module twister/http (export http-get http-post http-request))

;; ===========================================================================
;; HTTP リクエスト関数
;; ===========================================================================

;; http-request: 汎用 HTTP リクエスト
;; 引数:
;;   url         : リクエスト先 URL (必須)
;;   :method     : HTTP メソッド (デフォルト: "GET")
;;   :headers    : ヘッダーの alist ((:Content-Type "application/json") ...)
;;   :body       : リクエストボディ (文字列)
;; 戻り値:
;;   ((:status-code 200) (:headers nil) (:body "..."))
(def http-request
  (fn (url &key (method "GET") (headers nil) (body nil))
    (core-http-request method url headers body)))

;; http-get: GET リクエスト
;; 引数:
;;   url         : リクエスト先 URL (必須)
;;   :headers    : ヘッダーの alist (オプション)
;; 戻り値:
;;   ((:status-code 200) (:headers nil) (:body "..."))
(def http-get
  (fn (url &key (headers nil))
    (core-http-request "GET" url headers nil)))

;; http-post: POST リクエスト
;; 引数:
;;   url         : リクエスト先 URL (必須)
;;   :body       : リクエストボディ (文字列, オプション)
;;   :headers    : ヘッダーの alist (オプション)
;; 戻り値:
;;   ((:status-code 200) (:headers nil) (:body "..."))
(def http-post
  (fn (url &key (body nil) (headers nil))
    (core-http-request "POST" url headers body)))

;; ===========================================================================
;; レスポンス操作ユーティリティ
;; ===========================================================================

;; http-status: レスポンスからステータスコードを取得
(def http-status
  (fn (response)
    (car (cdr (car response)))))

;; http-body: レスポンスからボディを取得
(def http-body
  (fn (response)
    (car (cdr (car (cdr (cdr response)))))))
