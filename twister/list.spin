; list.spin — リスト操作関数
(module twister/list (export nil nil? map length append append2 foldl foldr reverse filter list member nth assoc alist-get get-in))

; 多相的な空リスト
(def nil (quote ()))

(def nil? (fn (x) (empty? x)))
(def map (fn (f xs) (if (nil? xs) nil (cons (f (car xs)) (map f (cdr xs))))))

(def length (fn (xs) (if (nil? xs) 0 (+ 1 (length (cdr xs))))))

(def foldl (fn (f acc xs) (if (nil? xs) acc (foldl f (f acc (car xs)) (cdr xs)))))
(def foldr (fn (f acc xs) (if (nil? xs) acc (f (car xs) (foldr f acc (cdr xs))))))

(def append2 (fn (xs ys) (if (nil? xs) ys (cons (car xs) (append2 (cdr xs) ys)))))
(def append (fn lists (foldr append2 nil lists)))

(def reverse (fn (xs) (foldl (fn (acc x) (cons x acc)) nil xs)))

(def filter (fn (p xs) (if (nil? xs) nil (if (p (car xs)) (cons (car xs) (filter p (cdr xs))) (filter p (cdr xs))))))

(def member (fn (item xs)
  (if (nil? xs)
      nil
      (if (equal item (car xs))
          xs
          (member item (cdr xs))))))

(def nth (fn (n xs)
  (if (nil? xs)
      nil
      (if (< n 0)
          nil
          (if (= n 0)
              (car xs)
              (nth (- n 1) (cdr xs)))))))

; assoc: Alist からキーに一致するペア (key value) を返す
; Alist は ((key1 value1) (key2 value2) ...) の形式
(def assoc (fn (k xs)
  (if (nil? xs)
      nil
      (let ((p (car xs)))
        (if (eq k (car p))
            p
            (assoc k (cdr xs)))))))

; alist-get: Alist からキーに対応する値を取得
; 見つからない場合は nil を返す
(def alist-get (fn (k xs)
  (let ((p (assoc k xs)))
    (if (nil? p)
        nil
        (car (cdr p))))))

; get-in: ネストしたデータからパスを辿って値を取得
; path は (:key1 0 :key2 ...) のようなリストでインデックス(整数)とキーを混在可能
(def get-in (fn (obj path)
  (if (nil? path)
      obj
      (if (nil? obj)
          nil
          (let ((p (car path))
                (r (cdr path)))
            (if (int? p)
                (get-in (nth p obj) r)
                (get-in (alist-get p obj) r)))))))
