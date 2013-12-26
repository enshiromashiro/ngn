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
  		   (fail "unexpected condition caused!: ~a~%" c))))))

(diag "*** ngn test ***")

(deftest-with-handler
	ngn.ngn
  (is (ngn:ngn '("test" "strings") '("temp") :html) '("temp"))
  (is (ngn:ngn '("test" "strings") nil :html) nil)
  (is (ngn:ngn nil '("test" "strings") :html) '("test" "strings"))
  (is (ngn:ngn nil nil :html) nil)
  (is (ngn:ngn '(":oneline タグ1"
				 ":block["
				 "タグ2"
				 ":block]")
			   '("#|oneline|#"
				 "#|block|#")
			   :html
			   :tag-hook (lambda (tags)
						   (mapcar (lambda (e)
									 (list (car e)
										   (if (consp (cadr e))
											   (cadr e)
											   (format nil "~a~a"  "*" (cadr e)))))
								   tags)))
	  '("*タグ1"
		"タグ2")))


(finalize)
