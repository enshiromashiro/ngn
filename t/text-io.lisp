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
  (is (ngn.text-io::guess-line-break "t/data/utf8-unix.txt" (guess::utf8-keyword)) :lf)
  (is (ngn.text-io::guess-line-break "t/data/utf8-osx.txt" (guess::utf8-keyword)) :cr)
  (is (ngn.text-io::guess-line-break "t/data/utf8-dos.txt" (guess::utf8-keyword)) :crlf))


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


  

(finalize)
