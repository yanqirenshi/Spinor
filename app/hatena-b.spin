; hatena-b.spin - Hatena Bookmark CLI Tool (Enhanced with JSON API)
;
; Usage:
;   spinor app/hatena-b.spin <URL>
;
; Example:
;   spinor app/hatena-b.spin https://github.com/yanqirenshi/Spinor

; Note: twister/list is preloaded by the Spinor runtime

; Integer division (recursive)
(def int-div
  (fn (a b)
    (if (< a b)
        0
        (+ 1 (int-div (- a b) b)))))

; Single digit to character
(def digit->char
  (fn (d)
    (nth d (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))))

; Integer to string conversion (positive numbers)
(def int->string-pos
  (fn (n)
    (if (< n 10)
        (digit->char n)
        (string-append (int->string-pos (int-div n 10))
                       (digit->char (% n 10))))))

; Integer to string conversion
(def int->string
  (fn (n)
    (if (< n 0)
        (string-append "-" (int->string-pos (- 0 n)))
        (int->string-pos n))))

; Show usage message
(def show-usage
  (fn ()
    (begin
      (print "Usage: spinor app/hatena-b.spin <URL>")
      (print "")
      (print "Example:")
      (print "  spinor app/hatena-b.spin https://github.com/yanqirenshi/Spinor")
      (print "")
      (print "This tool fetches the Hatena Bookmark data for a given URL.")
      (print "It displays bookmark count and user comments."))))

; Fetch bookmark data from Hatena jsonlite API
(def fetch-bookmark-data
  (fn (url)
    (let ((api-url (string-append "https://b.hatena.ne.jp/entry/jsonlite/?url=" url)))
      (http-get api-url))))

; Check if comment is non-empty (not nil and not empty string)
(def has-comment?
  (fn (comment)
    (if (nil? comment)
        #f
        (if (equal comment "")
            #f
            #t))))

; Display a single bookmark entry if it has a comment
(def display-bookmark
  (fn (bookmark)
    (let ((user    (alist-get :user bookmark))
          (comment (alist-get :comment bookmark)))
      (if (has-comment? comment)
          (print (string-append "  [" user "] " comment))
          nil))))

; Process all bookmarks recursively
(def process-bookmarks
  (fn (bookmarks)
    (if (nil? bookmarks)
        nil
        (begin
          (display-bookmark (car bookmarks))
          (process-bookmarks (cdr bookmarks))))))

; Display result from API response
(def display-result
  (fn (url response)
    (let ((status (http-status response))
          (body   (http-body response)))
      (if (= status 200)
          (let ((body-trimmed (string-trim body)))
            (if (equal body-trimmed "null")
                ; URL has no bookmarks (API returns "null")
                (print (string-append "[Hatena] 0 bookmarks: " url))
                ; Parse JSON and display data
                (let ((data (json-parse body-trimmed)))
                  (let ((count     (alist-get :count data))
                        (bookmarks (alist-get :bookmarks data)))
                    (begin
                      (print (string-append "[Hatena] " (int->string count) " bookmarks: " url))
                      (if (nil? bookmarks)
                          nil
                          (begin
                            (print "")
                            (print "Comments:")
                            (process-bookmarks bookmarks))))))))
          (begin
            (print (string-append "[Error] HTTP status: " (int->string status)))
            (print (string-append "        URL: " url)))))))

; Main entry point
(def main
  (fn ()
    (let ((args (command-line-args)))
      (if (empty? args)
          (show-usage)
          (let ((url (car args)))
            (let ((response (fetch-bookmark-data url)))
              (display-result url response)))))))

; Run
(main)
