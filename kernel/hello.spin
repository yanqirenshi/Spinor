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

; 5. 自動 Drop 挿入 (Phase R2-2 / Issue #77)
;    明示的な (drop ...) を書かずに「リスト確保 → 使用」を 100,000 回
;    繰り返す。リスト本体 (416B) と中間オブジェクト (比較 bool・
;    デクリメント int 等) を合わせ、1 周あたり約 600B を確保するため
;    総確保量は約 60MB — 4MiB の kmalloc ヒープの 14 倍以上。
;    コンパイラが中間値とスコープ終端の自動 sp_free を挿入していなければ
;    ヒープが枯渇して完走できない。
;    (churn は末尾自己再帰なので TCO により while ループ化され、
;     旧パラメータ値も _owned_ フラグ機構で毎周解放される)
(defun churn (n)
  (if (= n 0)
      "auto-drop churn: done"
      (if (null? (list 10 20 30 12))
          "auto-drop churn: unreachable"
          (churn (- n 1)))))

(print "auto-drop test: alloc x 100000 with NO explicit drop (approx 60MB through 4MiB heap)")
(print (churn 100000))

; 6. 明示的 (drop x) も引き続き動作すること (Phase R2-1 回帰確認)
;    注: drop の対象は関数内で生成した Fresh な値に限る。引数の drop や
;    パラメータを含む構造の drop は、現行の呼び出し規約 (引数は呼び出し側が
;    解放) と deep free が衝突するため、関数シグネチャへの所有権伝搬
;    (今後のフェーズ) までは未サポート。
(defun drop-demo ()
  (if (null? (drop (list 1 2 3)))
      "explicit drop still works"
      "unreachable"))
(print (drop-demo))

; 7. 大量チャーン後も新しい確保が正常に動くこと
(print (sum-list (list 1 2 3)))
(print "auto-drop test: allocation after churn works")
