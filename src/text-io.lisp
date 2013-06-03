#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.text-io
  (:use :cl
		:ccl
		:cl-annot))
(in-package :ngn.text-io)

(cl-annot:enable-annot-syntax)


(defun guess-encoding (filepath)
  :utf-8)

(defun guess-line-termination (filepath)
  :windows)


@export
(defun read-text (filepath)
  (let* ((text)
		 (enc :utf-8)
		 (lt :windows))
	(with-open-file (in
					 filepath
					 :direction :input
					 :external-format (make-external-format :character-encoding enc
															:line-termination lt)
					 :if-does-not-exist nil)
	  (if (null in)
		  'does-not-exists
		  (flet ((get-line (fin)
				   (read-line fin nil 'eof)))
			(do ((line (get-line in) (get-line in)))
				((eq line 'eof) (reverse text))
			  (setf text (cons line text))))))))
  

