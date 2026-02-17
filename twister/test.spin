; test.spin — Spinor セルフテスト
;
; assert-equal: 期待値と実測値が等しければ "." を表示し、
; 異なればエラーメッセージを表示する。

(def assert-equal
  (fn (expected actual)
    (if (= expected actual)
        (print ".")
        (print "FAIL"))))

; ============================================================
; math.spin のテスト
; ============================================================

; fact
(assert-equal 1   (fact 0))
(assert-equal 1   (fact 1))
(assert-equal 120 (fact 5))
(assert-equal 720 (fact 6))

; fib
(assert-equal 0  (fib 0))
(assert-equal 1  (fib 1))
(assert-equal 55 (fib 10))

; even? / odd?
(assert-equal #t (even? 0))
(assert-equal #t (even? 4))
(assert-equal #f (even? 3))
(assert-equal #t (odd? 3))
(assert-equal #f (odd? 4))

; ============================================================
; list.spin のテスト
; ============================================================

; foldl: 合計
(assert-equal 15
  (foldl (fn (acc x) (+ acc x)) 0
         (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))))

; foldl: 積
(assert-equal 120
  (foldl (fn (acc x) (* acc x)) 1
         (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil)))))))

; length
(assert-equal 0 (length nil))
(assert-equal 3 (length (cons 1 (cons 2 (cons 3 nil)))))

; map: 各要素を2倍にして先頭を検証
(assert-equal 2
  (car (map (fn (x) (* x 2)) (cons 1 (cons 2 (cons 3 nil))))))

; map: 最後の要素を検証
(assert-equal 6
  (car (cdr (cdr (map (fn (x) (* x 2)) (cons 1 (cons 2 (cons 3 nil))))))))

; filter: 偶数のみ
(assert-equal 2
  (length (filter even? (cons 1 (cons 2 (cons 3 (cons 4 nil)))))))

(assert-equal 2
  (car (filter even? (cons 1 (cons 2 (cons 3 (cons 4 nil)))))))

; reverse: 先頭と末尾の入れ替え
(assert-equal 3
  (car (reverse (cons 1 (cons 2 (cons 3 nil))))))

(assert-equal 1
  (car (cdr (cdr (reverse (cons 1 (cons 2 (cons 3 nil))))))))

; append: 連結後の長さ
(assert-equal 5
  (length (append (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 nil))))))
