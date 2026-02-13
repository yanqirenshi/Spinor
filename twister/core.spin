; core.spin — 基本論理関数とマクロ

(def not (fn (x) (if x #f #t)))
(def id  (fn (x) x))

; マクロ: (when cond body) → (if cond body #f)
(def when (mac (cond body) (list 'if cond body #f)))

; マクロ: (let var val body) → ((fn (var) body) val)
(def let (mac (var val body) (list (list 'fn (list var) body) val)))

; マクロ: (cond (pred1 expr1) (pred2 expr2) ...)
;   → (if pred1 expr1 (if pred2 expr2 ...))
(def cond
  (mac args
    (if (null? args)
        '()
        (let clause (car args)
          (let rest (cdr args)
            (let pred (car clause)
              (let expr (car (cdr clause))
                (list 'if pred expr (cons 'cond rest)))))))))
