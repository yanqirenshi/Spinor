; examples/hello.spin - Smoke test for the AOT codegen primitive fix (Issue #50)
;
; Usage:
;   cabal run spinor -- build --emit-c examples/hello.spin   # Cコードのみ
;   cabal run spinor -- build examples/hello.spin            # ネイティブビルド
;   ./hello                                                  # 実行
;
; Expected output: Hello, World!
(print "Hello, World!")
