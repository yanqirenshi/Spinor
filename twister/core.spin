; core.spin — 基本論理関数とマクロ
(module twister/core (export not id defun when unless cond and or))

(def not (fn (x) (if x #f #t)))
(def id  (fn (x) x))

; (defun name (args...) body...) -> (def name (fn (args...) body...))
(def defun
  (mac (name params . body)
    (list 'def name (cons 'fn (cons params body)))))

; (when condition body...) -> (if condition (begin body...) #f)
(def when
  (mac (condition . body)
    (list 'if condition (cons 'begin body) '#f)))

; (unless condition body...) -> (if condition #f (begin body...))
(def unless
  (mac (condition . body)
    (list 'if condition '#f (cons 'begin body))))

; (cond (p1 e1...) (p2 e2...) ...) -> (if p1 (begin e1...) (if p2 (begin e2...) ...))
; If body is missing, the predicate's value is returned.
(def cond
  (mac args
    (if (null? args)
        '#f
        (list 'if
              (car (car args))
              (if (null? (cdr (car args)))
                  (car (car args))
                  (cons 'begin (cdr (car args))))
              (cons 'cond (cdr args))))))

; (and form1 form2 ...)
(def and
  (mac args
    (if (null? args)
        '#t
        (if (null? (cdr args))
            (car args)
            (list 'if (car args) (cons 'and (cdr args)) '#f)))))

; (or form1 form2 ...)
(def or
  (mac args
    (if (null? args)
        '#f
        (if (null? (cdr args))
            (car args)
            (list 'if (car args) (car args) (cons 'or (cdr args)))))))
