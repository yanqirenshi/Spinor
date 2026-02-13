; core.spin — 基本論理関数とマクロ

(def not (fn (x) (if x #f #t)))
(def id  (fn (x) x))

; マクロ: (when cond body) → (if cond body #f)
(def when (mac (cond body) (list 'if cond body #f)))

; マクロ: (let var val body) → ((fn (var) body) val)
(def let (mac (var val body) (list (list 'fn (list var) body) val)))
