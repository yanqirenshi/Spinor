; math.spin — 数学関数

(def even? (fn (n) (= (% n 2) 0)))
(def odd?  (fn (n) (not (even? n))))

(def fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))
(def fib  (fn (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))
