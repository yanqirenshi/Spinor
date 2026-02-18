; core.spin — 基本論理関数とマクロ
(module twister/core (export not id defun when unless cond and or let* defparameter defvar))

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

; (let* ((var1 val1) (var2 val2) ...) body...)
;   順次束縛: 前の変数を後の定義で参照できる。ネストした let に展開。
(def let*
  (mac (bindings . body)
    (if (null? bindings)
        (cons 'begin body)
        (list 'let (list (car bindings))
              (cons 'let* (cons (cdr bindings) body))))))

; (defparameter name value) — 常に値を上書きする
(def defparameter
  (mac (name val . docs)
    (list 'def name val)))

; (defvar name value) — 既に定義されている場合は値を上書きしない
(def defvar
  (mac (name val . docs)
    (list 'if (list 'bound? (list 'quote name))
          name
          (list 'def name val))))

; dotimes と dolist は Eval.hs で特殊形式として実装
; (setq の副作用がクロージャ境界を超えないため)
