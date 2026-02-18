; list.spin — リスト操作関数
(module twister/list (export nil null? map length append append2 foldl foldr reverse filter list member nth))

; 多相的な空リスト
(def nil (quote ()))

(def null? (fn (x) (empty? x)))
(def map (fn (f xs) (if (null? xs) nil (cons (f (car xs)) (map f (cdr xs))))))

(def length (fn (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))

(def foldl (fn (f acc xs) (if (null? xs) acc (foldl f (f acc (car xs)) (cdr xs)))))
(def foldr (fn (f acc xs) (if (null? xs) acc (f (car xs) (foldr f acc (cdr xs))))))

(def append2 (fn (xs ys) (if (null? xs) ys (cons (car xs) (append2 (cdr xs) ys)))))
(def append (fn lists (foldr append2 nil lists)))

(def reverse (fn (xs) (foldl (fn (acc x) (cons x acc)) nil xs)))

(def filter (fn (p xs) (if (null? xs) nil (if (p (car xs)) (cons (car xs) (filter p (cdr xs))) (filter p (cdr xs))))))

(def member (fn (item xs)
  (if (null? xs)
      nil
      (if (equal item (car xs))
          xs
          (member item (cdr xs))))))

(def nth (fn (n xs)
  (if (null? xs)
      nil
      (if (< n 0)
          nil
          (if (= n 0)
              (car xs)
              (nth (- n 1) (cdr xs)))))))
