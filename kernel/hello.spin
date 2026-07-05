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

; 5. drop セマンティクス (Phase R2-1 / Issue #76)
;    「リスト確保 → drop (sp_free → kfree)」を 10,000 回繰り返す。
;    リスト 1 本あたり 416 バイト (ints/pairs/nil 含む) を確保するため
;    総確保量は約 6MB となり、4MiB の kmalloc ヒープを上回る。
;    よって drop が実際にメモリを解放し kfree が再利用していなければ
;    ヒープが枯渇して完走できない。
;    (churn は末尾自己再帰なので TCO により while ループ化され、
;     スタックも消費しない。中間オブジェクト (比較結果の bool 等) は
;     まだ drop されないため、その分の蓄積は 4MiB 内に収まる回数とした。
;     中間値の自動解放は次フェーズの自動 drop 挿入で対応する)
(defun churn (n)
  (if (= n 0)
      "drop churn: done"
      (if (null? (drop (list 10 20 30 12)))
          (churn (- n 1))
          "drop churn: unreachable")))

(print "drop test: alloc+drop x 10000 (approx 6MB through 4MiB heap)")
(print (churn 10000))

; 6. drop 後に新しい確保が正常に動くこと (解放済み領域の再利用)
(print (sum-list (list 1 2 3)))
(print "drop test: allocation after drop works")
