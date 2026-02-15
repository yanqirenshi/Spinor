; boot.spin — Twister ブートローダー
; ステップ14以降: Main.hs が各 .spin ファイルを直接ロードする方式に変更。
; このファイルは互換性のため残してあるが、直接使用されない。

(print "Loading Twister environment...")
(load "twister/core.spin")
(load "twister/list.spin")
(load "twister/math.spin")
(print "Twister loaded.")
