; hello.spin — Spinor AOT ベアメタル実証スクリプト (Phase R1-3 / Issue #75)
;
; このファイルは Spinor コンパイラ (--emit-c) で C に変換され、
; ベアメタルカーネル (kernel/) にリンクされて QEMU 上で直接実行される。
;
;   make hello.c   … C コードを再生成 (要 spinor コンパイラ)
;   make run       … ISO をビルドして QEMU で起動

; 1. 文字列出力
(print "Spinor on bare metal!")

; 2. 単純な計算
(print (+ 40 2))

; 3. リストのアロケーションと走査 (cons セルは kmalloc で確保される)
(defun sum-list (xs)
  (if (null? xs)
      0
      (+ (car xs) (sum-list (cdr xs)))))

(print (list 10 20 30 12))
(print (sum-list (list 10 20 30 12)))

; 4. 文字列結合 (ヒープ確保を伴う)
(print (string-append "Linear " "Spinor"))
