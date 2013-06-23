#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.generator
  (:use :cl
		:cl-annot)
  (:import-from :cl-ppcre
				:scan-to-strings)
  (:import-from :ngn.tag-parser
				:get-tag-data))
(in-package :ngn)

(cl-annot:enable-annot-syntax)

@export 
(defmethod generate (extension tags template)
  (format t "generic: generate~%"))

;;@export 
;;(defmethod generate ((extention (eq "html")) tags template)
;;  )  

(defun insert-tags-into-line (line tags tag-regex get-tag-name)
  (let ((matches (ppcre:all-matches tag-regex line))
		(inserted-list))
	(format t "~a  ~%" matches)
	(do ((pos (head-two matches) (head-two matches)))
		((null pos) inserted-list)
	  (let ((tag-data (get-tag-data 
					   (apply get-tag-name 
							  `(,(subseq line (car pos) (cadr pos)))) tags)))
		(insert-into-line line
						  (car pos)
						  (cadr pos)
						  tag-data))
	  (setf matches (cddr matches)))))

(defmethod insert-into-line (target-line start end (line string))
  (concatenate 'string
			   (subseq target-line 0 start)
			   line
			   (subseq target-line end (length target-line))))

(defmethod insert-into-line (target-line start end (text list))
  (if (null text)
	  target-line
	  (if (eq (length text) 1)
		  (insert-to-line target-line start end (car text))
		  (let ((inserted)
				(before (subseq target-line 0 start))
				(after (subseq target-line end (length target-line))))
			(push (concatenate 'string before (car text)) inserted)
			(let ((rest (cdr text)))
			  (dolist (l rest)
				(push l inserted))
			  (setf (car inserted) (concatenate 'string (car inserted) after)))
		  (reverse inserted)))))



(defun head-two (lis)
  (if (or (null lis)
		  (null (cdr lis)))
	  nil
	  (values (list (car lis) (cadr lis))
			  (cddr lis))))