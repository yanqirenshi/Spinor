; list.spin — リスト操作関数

(def null? (fn (x) (empty? x)))
(def map (fn (f xs) (if (null? xs) (quote ()) (cons (f (car xs)) (map f (cdr xs))))))
