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
(in-package :ngn)



@export 
(defmethod generate (extension tags template)
  )



(defmethod insert-to-line (target-line start end (line string))
  (concatenate 'string
			   (subseq target-line 0 start)
			   line
			   (subseq target-line end (length target-line))))

(defmethod insert-to-line (target-line start end (text list))
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
			  (push (concatenate 'string (car inserted) after) inserted))
		  (reverse inserted)))))

