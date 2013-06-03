#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.tag-parser
  (:use :cl)
  (:import-from :alexandria
				:emptyp)
  (:import-from :cl-ppcre
				:scan-to-strings))
(in-package :ngn.tag-parser)

(cl-annot:enable-annot-syntax)


(defconstant +tag-block-delimiter-start+ #\[)
(defconstant +tag-block-delimiter-end+ #\])

(defconstant +tag-identifer-valid-chars+ "[a-z0-9-]")
(defconstant +tag-regex-oneline+ 
  (concatenate 'string
			   ":("
			   +tag-identifer-valid-chars+
			   "+?)[ ]+(.+)$"))
(defconstant +tag-regex-block+
  (concatenate 'string
			   ":("
			   +tag-identifer-valid-chars+
			   "+?)(\\"
			   (string +tag-block-delimiter-start+)
			   "|\\"
			   (string +tag-block-delimiter-end+)
			   ")$"))


@export
(defun parse-tags (text)
  (append (parse-oneline-tags text)
		  (parse-block-tags text)))

(defun parse-oneline-tags (text)
  (let ((tags))
	(dolist (line text (reverse tags))
	  (let ((tag (parse-tag line +tag-regex-oneline+)))
		(if (not (null tag))
			(push tag tags))))))

(defun parse-block-tags (text &optional tag-name lines)
  (if (null text)
	  nil
	  (flet ((tag-p (tag delim)
			   (and (not (null tag)) (string= (cadr tag) delim))))
		(if (null tag-name)
			(let ((tag (parse-tag (car text) +tag-regex-block+)))
			  (if (tag-p tag +tag-block-delimiter-start+)
				  (parse-block-tags (cdr text) (car tag))
				  (parse-block-tags (cdr text))))
			(let ((tag (parse-tag (car text) +tag-regex-block+)))
			  (if (and (tag-p tag +tag-block-delimiter-end+)
					   (eq tag-name (gen-keyword (car tag))))
				  (cons (list tag-name lines)
						(parse-block-tags (cdr text)))
				  (parse-block-tags (cdr text) 
								   tag-name
								   (reverse (cons (car text)
												  (reverse lines))))))))))

(defun parse-tag (line tag-regex)
  (if (and (stringp line) (emptyp line))
	  nil
	  (multiple-value-bind (_ tag) (scan-to-strings tag-regex line)
		(if (null _)
			nil
			(list (gen-keyword (svref tag 0)) (svref tag 1))))))

(defun gen-keyword (name)
  (car (multiple-value-list (intern (string-upcase name)
									:keyword))))
