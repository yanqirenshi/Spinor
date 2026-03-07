; hatena-b.spin - Hatena Bookmark Count CLI Tool
;
; Usage:
;   spinor app/hatena-b.spin <URL>
;
; Example:
;   spinor app/hatena-b.spin https://github.com/yanqirenshi/Spinor

; Show usage message
(def show-usage
  (fn ()
    (begin
      (print "Usage: spinor app/hatena-b.spin <URL>")
      (print "")
      (print "Example:")
      (print "  spinor app/hatena-b.spin https://github.com/yanqirenshi/Spinor")
      (print "")
      (print "This tool fetches the Hatena Bookmark count for a given URL."))))

; Fetch bookmark count from Hatena API
(def fetch-bookmark-count
  (fn (url)
    (let ((api-url (string-append "https://bookmark.hatenaapis.com/count/entry?url=" url)))
      (http-get api-url))))

; Display result from response
(def display-result
  (fn (url response)
    (let ((status (http-status response))
          (body   (http-body response)))
      (if (= status 200)
          (print (string-append "[Hatena] " (string-trim body) " bookmarks: " url))
          (begin
            (print (string-append "[Error] HTTP status: " (http-int->string status)))
            (print (string-append "        URL: " url)))))))

; Main entry point
(def main
  (fn ()
    (let ((args (command-line-args)))
      (if (empty? args)
          (show-usage)
          (let ((url (car args)))
            (let ((response (fetch-bookmark-count url)))
              (display-result url response)))))))

; Run
(main)
