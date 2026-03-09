; fib-aot.spin - Fibonacci function for LLVM AOT compilation test
;
; Usage:
;   cabal run spinor -- build-llvm fib-aot.spin
;   ./fib-aot
;
; Expected output: 55 (fib(10))

; Fibonacci function using recursion
(def fib (fn (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

; Compute fib(10) - should return 55
(fib 10)
