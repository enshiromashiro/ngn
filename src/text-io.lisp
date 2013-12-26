#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.text-io
  (:use :cl
		:cl-annot)
  (:import-from :guess
				:ces-guess-from-vector))
(in-package :ngn.text-io)

(cl-annot:enable-annot-syntax)


(defmacro with-file-vector ((var size pathname enc) &body body)
  "create vector buffer _var_ consists of first _size_ bytes of _pathname_ file, and evaluate _body_.
*args
var: name of var of vecter
size: vector buffer size
pathname: target file
enc: available on *only CCL*. encoding of target file.
body: body forms"
  (let ((in (gensym)))
	`(let ((,var (make-array ,size :element-type '(unsigned-byte 8))))
	   (with-open-file
		   (,in ,pathname
				:direction :input
				:element-type '(unsigned-byte 8)
				,@(and enc `(:external-format ,enc)))
		 (read-sequence ,var ,in))
	   ,@body)))


(defun guess-encoding (pathname &optional (size 10000))
  "guess encding of _pathname_.
*args
pathname: pathname of the target file
size: vector buffer size"
  (with-file-vector (vec size pathname nil)
	;; this function cannot guess sjis *on clozure cl*.
	;; what can i do ... X(
	(ces-guess-from-vector vec :jp)))


(defun guess-line-break (pathname encoding &optional (size 10000))
  "guess line break marker of _pathname_, when its encoding is _encoding_.
*args
pathname: pathname of the target file
encoding: encoding of the target file
size: vector buffer size"
  (with-file-vector (vec size pathname encoding)
	(loop for n from 0 to size do 
		 (if (eq (code-char (aref vec n)) #\Return)  ; CR?
			 (if (eq (code-char (aref vec (1+ n))) #\Newline)
				 (return :crlf)  ; CRLF
				 (return :cr)))  ; LF
		 (if (eq (code-char (aref vec n)) #\Newline)  ; LF?
			 (return :lf)))))


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
