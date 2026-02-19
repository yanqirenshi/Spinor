; === String Operations ===
(print "=== String Tests ===")

; string-append
(def greeting (string-append "Hello" ", " "World" "!"))
(print greeting)  ; => "Hello, World!"

; string-length
(print (string-length "hello"))  ; => 5
(print (string-length ""))       ; => 0

; substring
(print (substring "hello" 1 3))  ; => "el"
(print (substring "hello" 0 5))  ; => "hello"

; string=?
(print (string=? "abc" "abc"))   ; => #t
(print (string=? "abc" "def"))   ; => #f

; string->list
(print (string->list "abc"))     ; => ("a" "b" "c")

; list->string
(print (list->string (list "a" "b" "c")))  ; => "abc"

; === File I/O ===
(print "=== File I/O Tests ===")

; write-file
(write-file "test-output.txt" "Hello from Spinor!\n")

; append-file
(append-file "test-output.txt" "Second line\n")

; read-file
(def content (read-file "test-output.txt"))
(print content)

; file-exists?
(print (file-exists? "test-output.txt"))  ; => #t
(print (file-exists? "nonexistent.txt"))  ; => #f

(print "=== All tests passed ===")
