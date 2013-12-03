#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.text-io
  (:use :cl
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
					 :external-format enc
					 :if-does-not-exist nil)
	  (-read-text in))))

(defun -read-text (in)
  (if (null in)
	  :does-not-exists
	  (let ((text))
		(flet ((get-line (fin)
				 (read-line fin nil :eof)))
		  (do ((line (get-line in) (get-line in)))
			  ((eq line :eof) (reverse text))
			(setf text (cons line text)))))))

@export
(defun write-text (filepath text)
  (let* ((enc :utf-8)
		 (lt :windows))
	(with-open-file (out
					 filepath
					 :direction :output
					 :external-format enc
					 :if-exists nil)
	  (-write-text out text))))

(defun -write-text (out text)
  (if (null text)
	  nil
	  (progn
		(write-line (car text) out)
		(-write-text out (cdr text)))))
