; test-modules/lib.spin — テスト用モジュール
(module lib (export my-val double))

(define my-val 100)

(define double (fn (x) (* x 2)))
