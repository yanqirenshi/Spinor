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

; ============================================================
; Step 21: Common Lisp互換機能 (基本) のテスト
; ============================================================

; --- let の複数・並列束縛 ---

; 新形式: 単一束縛
(assert-equal 10 (let ((x 10)) x))

; 新形式: 複数束縛
(assert-equal 3 (let ((x 1) (y 2)) (+ x y)))

; 並列束縛: 外側のxを参照
(assert-equal 15
  (let ((x 10))
    (let ((x 2) (y (+ x 5)))
      y)))

; --- setq (破壊的代入) ---

; 基本的な setq
(assert-equal 10
  (let ((x 0))
    (begin
      (setq x 10)
      x)))

; カウンタのインクリメント
(assert-equal 3
  (let ((counter 0))
    (begin
      (setq counter (+ counter 1))
      (setq counter (+ counter 1))
      (setq counter (+ counter 1))
      counter)))

; --- eq と equal ---

; eq: 数値は値で比較
(assert-equal #t (eq 1 1))
(assert-equal #f (eq 1 2))

; eq: シンボルは値で比較
(assert-equal #t (eq 'a 'a))
(assert-equal #f (eq 'a 'b))

; eq: リストは参照比較 (異なるリストは常にfalse)
(assert-equal #f (eq (list 1 2) (list 1 2)))

; equal: 構造比較
(assert-equal #t (equal 1 1))
(assert-equal #t (equal (list 1 2 3) (list 1 2 3)))
(assert-equal #f (equal (list 1 2) (list 1 2 3)))
(assert-equal #t (equal "hello" "hello"))
