; test-tco.spin - TCO verification (1M recursive countdown)
(defun countdown (n)
  (if (= n 0)
    0
    (countdown (- n 1))))

(countdown 1000000)
