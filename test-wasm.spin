;; test-wasm.spin

(defun fact (n)
  (if (<= n 1)
      1
      (* n (fact (- n 1)))))

;; この計算結果 (120) がブラウザに表示されるはず
(fact 5)
