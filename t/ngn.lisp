#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn-test
  (:use :cl
        :ngn
        :cl-test-more))
(in-package :ngn-test)

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

(diag "ngn-test")

  
(run-test-all)

(finalize)
