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
  		   (diag (concatenate 'string
  							  "unexpected condition caused!: "
  							  (format nil "~a" c)
  							  )))))))

(diag "ngn.text-io test")

(deftest-with-handler
  text-io.-read-text
  (macrolet ((test-with-stream (test txt expected)
			   `(let ((sin (make-string-input-stream ,txt)))
				  (,test (ngn.text-io::-read-text sin)
						 ,expected))))
	(test-with-stream is nil :does-not-exists)
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
