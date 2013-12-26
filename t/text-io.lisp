#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.text-io-test
  (:use :cl
        :ngn.text-io
        :cl-test-more))
(in-package :ngn.text-io-test)


(plan nil)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro deftest-with-handler (test-name &rest forms)
    `(deftest ,test-name
         (diag ,(format nil "test for ~a..." test-name))
       (handler-case 
           (progn ,@forms)
         (condition (c) 
           (fail "unexpected condition caused!: ~a~%" c))))))

(diag "ngn.text-io test")


(deftest-with-handler
  text-io.guess-encoding
  (diag "*** for utf-8")
  (is (ngn.text-io::guess-encoding "t/data/utf8-unix.txt") (guess::utf8-keyword))
  (is (ngn.text-io::guess-encoding "t/data/utf8-osx.txt") (guess::utf8-keyword))
  (is (ngn.text-io::guess-encoding "t/data/utf8-dos.txt") (guess::utf8-keyword))

  (diag "*** for euc-jp")
  (is (ngn.text-io::guess-encoding "t/data/eucjp-unix.txt") (guess::eucj-keyword))
  (is (ngn.text-io::guess-encoding "t/data/eucjp-osx.txt") (guess::eucj-keyword))
  (is (ngn.text-io::guess-encoding "t/data/eucjp-dos.txt") (guess::eucj-keyword))
  
  (diag "*** for cp932 (sjis)")
  (is (ngn.text-io::guess-encoding "t/data/sjis-unix.txt") (guess::sjis-keyword))
  (is (ngn.text-io::guess-encoding "t/data/sjis-dos.txt") (guess::sjis-keyword)))


(deftest-with-handler
  text-io.guess-line-break
  (diag "*** for utf8")
  (is (ngn.text-io::guess-line-break "t/data/utf8-unix.txt" (guess::utf8-keyword)) :unix)
  (is (ngn.text-io::guess-line-break "t/data/utf8-osx.txt" (guess::utf8-keyword)) :macos)
  (is (ngn.text-io::guess-line-break "t/data/utf8-dos.txt" (guess::utf8-keyword)) :dos))


(deftest-with-handler
  text-io.external-format-from-file
  (let ((keywords `((:utf-8 . ,#'guess::utf8-keyword)
                    (:euc-jp . ,#'guess::eucj-keyword)
                    (:cp032 . ,#'guess::sjis-keyword))))
    (flet ((one-test (f enc lb)
             (is (multiple-value-list 
                  (ngn.text-io::external-format-from-file f))
                 (list #+ccl (ccl:make-external-format :character-encoding enc
                                                       :line-termination lb)
                       #-ccl (funcall (cdr (assoc enc keywords)))
                       enc lb))))

      (diag "*** for utf-8")
      (one-test "t/data/utf8-unix.txt" :utf-8 :unix)
      (one-test "t/data/utf8-osx.txt" :utf-8 :macos)
      (one-test "t/data/utf8-dos.txt" :utf-8 :dos)

      (diag "*** for euc-jp")
      (one-test "t/data/eucjp-unix.txt" :euc-jp :unix)
      (one-test "t/data/eucjp-osx.txt" :euc-jp :macos)
      (one-test "t/data/eucjp-dos.txt" :euc-jp :dos)

      (diag "*** for cp932 (sjis)")
      (one-test "t/data/sjis-unix.txt" :cp932 :unix)
      (one-test "t/data/sjis-dos.txt" :cp932 :dos))))

(deftest-with-handler
  text-io.make-external-format
  (is (ngn.text-io:make-external-format :utf-8 :unix)
      #+ccl (ccl:make-external-format :character-encoding :utf-8
                                  :line-termination :unix)
      #-ccl :utf-8))


(deftest-with-handler
  text-io.-read-text
  (is (ngn.text-io::-read-text nil) :does-not-exists)
  (macrolet ((test-with-stream (test txt expected)
			   `(let ((sin (make-string-input-stream ,txt)))
				  (,test (ngn.text-io::-read-text sin)
						 ,expected))))
	(test-with-stream is "" nil)
	(test-with-stream is "hoge" '("hoge"))
	(test-with-stream is (format nil "first~%second~%") '("first" "second"))))


(deftest-with-handler
	text-io.-write-text
	(macrolet ((test-with-stream (test-func text expected-for-format)
				 `(let ((sout (make-string-output-stream)))
					(,test-func (progn
								  (ngn.text-io::-write-text sout ,text)
								  (get-output-stream-string sout))
								,expected-for-format))))
	  (test-with-stream is nil "")
	  (test-with-stream is '("a line") (format nil "a line~%"))
	  (test-with-stream is '("first line."
							 "second line."
							 "third line.")
						(format nil "first line.~%second line.~%third line.~%"))))


(deftest-with-handler
  text-io.read-text
  (is (multiple-value-list (ngn.text-io:read-text "t/data/read-text.txt"))
      (list
       '("　目覚めると、今日もわたしだ。"
        "　そんな当然すぎる事柄が本当は何を意味しているのか、最近ようやく少し理解できるようになったと感じる。"
        "")
        (cons (guess::utf8-keyword) :unix))))


(deftest-with-handler
  text-io.write-text
  (let ((file "t/data/write-text.txt")
        (enc :utf-8)
        (lb :unix)
        (text '("　叔父は文字だ。文字通り。"
                "　だからわたしは、叔父を記すための道具を探さなければならない。"))
        (writed "　叔父は文字だ。文字通り。
　だからわたしは、叔父を記すための道具を探さなければならない。
"))
    (ngn.text-io:write-text file text enc lb)
    (with-output-to-string (strout)
      (with-open-file (in file
                          :direction :input
                          :external-format (ccl:make-external-format
                                            :character-encoding :utf-8
                                            :line-termination :unix))
        (do ((c (read-char in nil nil) (read-char in nil nil))) nil
          (if c
              (write-char c strout)
              (return)))
        (is (get-output-stream-string strout) writed)))))
    

(finalize)
