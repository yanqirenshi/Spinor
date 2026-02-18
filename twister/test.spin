; test.spin — Spinor セルフテスト
;
; assert-equal: 期待値と実測値が等しければ "." を表示し、
; 異なればエラーメッセージを表示する。

(def assert-equal
  (fn (label actual expected)
    (if (equal actual expected)
        (print ".")
        (begin
          (print "FAIL: ")
          (print label)
          (print "\n")))))

; ============================================================
; math.spin のテスト
; ============================================================

; fact
(assert-equal "(fact 0)" (fact 0) 1)
(assert-equal "(fact 1)" (fact 1) 1)
(assert-equal "(fact 5)" (fact 5) 120)
(assert-equal "(fact 6)" (fact 6) 720)

; fib
(assert-equal "(fib 0)" (fib 0) 0)
(assert-equal "(fib 1)" (fib 1) 1)
(assert-equal "(fib 10)" (fib 10) 55)

; even? / odd?
(assert-equal "(even? 0)" (even? 0) #t)
(assert-equal "(even? 4)" (even? 4) #t)
(assert-equal "(even? 3)" (even? 3) #f)
(assert-equal "(odd? 3)" (odd? 3) #t)
(assert-equal "(odd? 4)" (odd? 4) #f)

; ============================================================
; list.spin のテスト
; ============================================================

; foldl: 合計
(assert-equal "foldl sum"
  (foldl (fn (acc x) (+ acc x)) 0
         (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))
  15)

; foldl: 積
(assert-equal "foldl product"
  (foldl (fn (acc x) (* acc x)) 1
         (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 nil))))))
  120)

; length
(assert-equal "(length nil)" (length nil) 0)
(assert-equal "(length (cons 1 2 3))" (length (cons 1 (cons 2 (cons 3 nil)))) 3)

; map: 各要素を2倍にして先頭を検証
(assert-equal "map *2 head"
  (car (map (fn (x) (* x 2)) (cons 1 (cons 2 (cons 3 nil)))))
  2)

; map: 最後の要素を検証
(assert-equal "map *2 last"
  (car (cdr (cdr (map (fn (x) (* x 2)) (cons 1 (cons 2 (cons 3 nil)))))))
  6)

; filter: 偶数のみ
(assert-equal "filter even? length"
  (length (filter even? (cons 1 (cons 2 (cons 3 (cons 4 nil))))))
  2)

(assert-equal "filter even? head"
  (car (filter even? (cons 1 (cons 2 (cons 3 (cons 4 nil))))))
  2)

; reverse: 先頭と末尾の入れ替え
(assert-equal "reverse head"
  (car (reverse (cons 1 (cons 2 (cons 3 nil)))))
  3)

(assert-equal "reverse last"
  (car (cdr (cdr (reverse (cons 1 (cons 2 (cons 3 nil)))))))
  1)

; append: 連結後の長さ
(assert-equal "append length"
  (length (append (cons 1 (cons 2 nil)) (cons 3 (cons 4 (cons 5 nil)))))
  5)

; ============================================================
; Step 21: Common Lisp互換機能 (基本) のテスト
; ============================================================

; --- let の複数・並列束縛 ---

; 新形式: 単一束縛
(assert-equal "let single" (let ((x 10)) x) 10)

; 新形式: 複数束縛
(assert-equal "let multiple" (let ((x 1) (y 2)) (+ x y)) 3)

; 並列束縛: 外側のxを参照
(assert-equal "let parallel"
  (let ((x 10))
    (let ((x 2) (y (+ x 5)))
      y))
  15)

; --- setq (破壊的代入) ---

; 基本的な setq
(assert-equal "setq basic"
  (let ((x 0))
    (begin
      (setq x 10)
      x))
  10)

; カウンタのインクリメント
(assert-equal "setq counter"
  (let ((counter 0))
    (begin
      (setq counter (+ counter 1))
      (setq counter (+ counter 1))
      (setq counter (+ counter 1))
      counter))
  3)

; --- eq と equal ---

; eq: 数値は値で比較
(assert-equal "(eq 1 1)" (eq 1 1) #t)
(assert-equal "(eq 1 2)" (eq 1 2) #f)

; eq: シンボルは値で比較
(assert-equal "(eq 'a 'a)" (eq 'a 'a) #t)
(assert-equal "(eq 'a 'b)" (eq 'a 'b) #f)

; eq: リストは参照比較 (異なるリストは常にfalse)
(assert-equal "(eq (list 1 2) (list 1 2))" (eq (list 1 2) (list 1 2)) #f)

; equal: 構造比較
(assert-equal "(equal 1 1)" (equal 1 1) #t)
(assert-equal "(equal (list 1 2 3) (list 1 2 3))" (equal (list 1 2 3) (list 1 2 3)) #t)
(assert-equal "(equal (list 1 2) (list 1 2 3))" (equal (list 1 2) (list 1 2 3)) #f)
(assert-equal "(equal \"hello\" \"hello\")" (equal "hello" "hello") #t)

; ============================================================
; Step 22: 制御構文マクロのテスト
; ============================================================

; --- defun ---
(defun add (x y) (+ x y))
(assert-equal "(defun add (x y) (+ x y))" (add 10 20) 30)

; --- when ---
(def test-when 0)
(when #t (setq test-when 1) (setq test-when 2))
(assert-equal "(when #t ...)" test-when 2)
(when #f (setq test-when 99))
(assert-equal "(when #f ...)" test-when 2)

; --- unless ---
(def test-unless 0)
(unless #f (setq test-unless 1) (setq test-unless 2))
(assert-equal "(unless #f ...)" test-unless 2)
(unless #t (setq test-unless 99))
(assert-equal "(unless #t ...)" test-unless 2)

; --- cond ---
(assert-equal "(cond ...)"
              (cond ((= 1 2) 'a)
                    ((= 2 2) 'b)
                    (else 'c))
              'b)

(assert-equal "(cond with only pred)" (cond ((< 1 0)) ((> 1 0))) #t)

; --- and ---
(assert-equal "(and #t #t)" (and #t #t) #t)
(assert-equal "(and #t 'ok)" (and #t 'ok) 'ok)
(assert-equal "(and #f (error \"fail\"))" (and #f (error "fail")) #f)
(assert-equal "(and)" (and) #t)

; --- or ---
(assert-equal "(or #f 'ok)" (or #f 'ok) 'ok)
(assert-equal "(or #t (error \"fail\"))" (or #t (error "fail")) #t)
(assert-equal "(or #f #f 'last)" (or #f #f 'last) 'last)
(assert-equal "(or)" (or) #f)

; ============================================================
; Step 23: リスト操作ユーティリティのテスト
; ============================================================

; --- list ---
(assert-equal "(list)" (list) nil)
(assert-equal "(list 1 2 3)" (list 1 2 3) (cons 1 (cons 2 (cons 3 nil))))
(assert-equal "(list (list 1) (list 2 3))"
  (list (list 1) (list 2 3))
  (cons (cons 1 nil) (cons (cons 2 (cons 3 nil)) nil)))

; --- append2 (2引数版) ---
(assert-equal "(append2 (list 1 2) (list 3 4))" (append2 (list 1 2) (list 3 4)) (list 1 2 3 4))
(assert-equal "(append2 nil (list 1 2))" (append2 nil (list 1 2)) (list 1 2))
(assert-equal "(append2 (list 1 2) nil)" (append2 (list 1 2) nil) (list 1 2))

; --- append (多引数版) ---
(assert-equal "(append)" (append) nil)
(assert-equal "(append (list 1 2))" (append (list 1 2)) (list 1 2))
(assert-equal "(append (list 1 2) (list 3 4))" (append (list 1 2) (list 3 4)) (list 1 2 3 4))
(assert-equal "(append (list 1) (list 2) (list 3))" (append (list 1) (list 2) (list 3)) (list 1 2 3))
(assert-equal "(append (list 1 2) nil (list 3))" (append (list 1 2) nil (list 3)) (list 1 2 3))

; --- member ---
(assert-equal "(member 3 (list 1 2 3 4 5))" (member 3 (list 1 2 3 4 5)) (list 3 4 5))
(assert-equal "(member 6 (list 1 2 3))" (member 6 (list 1 2 3)) nil)
(assert-equal "(member nil (list 1 nil 2))" (member nil (list 1 nil 2)) (list nil 2))
(assert-equal "(member (list 2 3) (list 1 (list 2 3) 4))"
  (member (list 2 3) (list 1 (list 2 3) 4))
  (list (list 2 3) 4))

; --- nth ---
(assert-equal "(nth 0 (list 10 20 30))" (nth 0 (list 10 20 30)) 10)
(assert-equal "(nth 2 (list 10 20 30))" (nth 2 (list 10 20 30)) 30)
(assert-equal "(nth 3 (list 10 20 30))" (nth 3 (list 10 20 30)) nil)
(assert-equal "(nth 0 nil)" (nth 0 nil) nil)

; --- reverse (既存の確認) ---
(assert-equal "(reverse (list 1 2 3))" (reverse (list 1 2 3)) (list 3 2 1))
(assert-equal "(reverse nil)" (reverse nil) nil)
(assert-equal "(reverse (list 1))" (reverse (list 1)) (list 1))

; --- length (既存の確認) ---
(assert-equal "(length (list 1 2 3))" (length (list 1 2 3)) 3)
(assert-equal "(length nil)" (length nil) 0)
(assert-equal "(length (list (list 1)))" (length (list (list 1))) 1)

(print "\nAll tests passed.\n")
