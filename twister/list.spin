; list.spin — リスト操作関数

(def null? (fn (x) (empty? x)))
(def map (fn (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))

(def length (fn (xs) (if (null? xs) 0 (+ 1 (length (cdr xs))))))
(def append (fn (xs ys) (if (null? xs) ys (cons (car xs) (append (cdr xs) ys)))))

(def foldl (fn (f acc xs) (if (null? xs) acc (foldl f (f acc (car xs)) (cdr xs)))))
(def foldr (fn (f acc xs) (if (null? xs) acc (f (car xs) (foldr f acc (cdr xs))))))

(def reverse (fn (xs) (foldl (fn (acc x) (cons x acc)) '() xs)))
