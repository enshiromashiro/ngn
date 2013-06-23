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
				:get-tag-data)
  (:import-from :alexandria
				:flatten))
(in-package :ngn.generator)

(cl-annot:enable-annot-syntax)

@export 
(defmethod generate (extension tags template)
  (format t "generic: generate~%"))

@export 
(defmethod generate ((extention (eql 'html)) tags template)
  (let ((generated))
	(dolist (line template (reverse generated))
	  (push (insert-tags-into-line line tags "#\\|([a-z-]+)\\|#")
			generated))))

(defun insert-tags-into-line (line tags marker-regex)
  (let ((split (split-line line
						   (ppcre:all-matches marker-regex line))))
	(do ((i 0 (1+ i)))
		((>= i (length split)))
	  (let ((marker (multiple-value-list 
					 (ppcre:scan-to-strings marker-regex
											(nth i split)))))
		(if (not (null (car marker)))
			(setf (nth i split) 
				  (get-tag-data (svref (cadr marker) 0) tags)))))
	(apply #'concatenate 'string (flatten split))))

(defun split-line (line pos)
  (remove-empty-string 
   (-split-line line pos)))

(defun remove-empty-string (strs)
  (remove "" strs :test #'equal))

(defun -split-line (line pos)
  (if (null pos)
	  (list line)
	  (let ((prev-pos 0)
			(str-list))
		(do ((i 0 (1+ i)))
			((>= i (length pos)))
		  (setf str-list
				(cons (subseq line prev-pos (nth i pos))
					  str-list))
		  (setf prev-pos (nth i pos)))
		(reverse
		 (cons (subseq line prev-pos (length line))
			   str-list)))))

