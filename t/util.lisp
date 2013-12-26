#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage util
  (:use :cl
        :util
        :cl-test-more))
(in-package :util)

(plan nil)


(diag "*** util test ***")

;; gen-keyword
(deftest "util:gen-keyword"
    (diag "* gen-keyword *")
  (is (util:gen-keyword "hoge") :hoge)
  (is (util:gen-keyword "HOGE") (util:gen-keyword "hoge")))


;; debug
(deftest "util:dbg"
    (diag "* debug *")
  (flet ((test-debug (*debug* str rtn out)
           (let ((*debug-output* (make-string-output-stream)))
             (is (util:dbg str) rtn)
             (is (get-output-stream-string *debug-output*) out))))

    (test-debug nil nil nil "")
    (test-debug nil "" nil "")
    (test-debug nil "hoge" nil "")
    (test-debug nil '("hoge" "fuga") nil "")

    (test-debug t nil nil "[debug]
")
    (test-debug t "" nil "[debug] 
")
    (test-debug t "hoge" nil "[debug] hoge
")
    (test-debug t '("hoge" "fuga") nil "[debug]
    hoge
    fuga
")))


(finalize)
