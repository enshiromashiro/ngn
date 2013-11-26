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
				:flatten
				:lastcar))
(in-package :ngn.generator)

(cl-annot:enable-annot-syntax)


(defvar *generators* nil)

;; add html generator
;; load *generator*
;; *generator*
;; 1. extension
;; 2. marker rege
;; 3. preproccess for oneline tag
;; 4. preproccess for block tag
(defvar *generators* nil)

;; html generator
(push (list :html 
			"#\\|([a-z0-9-]+)\\|#"
			#'identity
			(lambda (tag)
			  (list (car tag)
					(if (not (eq (length (cadr tag)) 1))
						(let ((proccessed))
						  (dolist (elm (cadr tag) (reverse proccessed))
							(push (concatenate 'string elm "<br>") proccessed)))
						(cadr tag)))))
	  *generators*)


;; (defmethod generate ((extension (eql 'html)) tags template)
;;   (format t "generate[~a]: generating...~%" extention)
;;   (let ((generated))
;; 	(dolist (line template (reverse generated))
;; 	  (push (insert-tags-into-line line tags "#\\|([a-z0-9-]+)\\|#")
;; 			generated))))

@export 
(defun generate (extension tags template)
  (let ((filetype (car (member extension *generators* :key #'car))))
	(if (null filetype)
		(progn
		  (format t "generate[~a]: no extension dispatched.~%" extension))
		(progn
		  (format t "generate[~a]: generating...~%" extension)
		  (let ((procced (preproc-tags tags
										   (nth 2 filetype)
										   (nth 3 filetype))))
			(insert-tags-into-lines template procced (nth 1 filetype)))))))


(defun preproc-tags (tags oneline-proc block-proc)
  (let ((new-tags))
	(dolist (tag tags (reverse new-tags))
	  (if (atom (cadr tag))
		  (push (funcall oneline-proc tag) new-tags)
		  (push (funcall block-proc tag) new-tags)))))

(defun insert-tags-into-lines (lines tags marker-regex)
  (let ((generated))
	(dolist (line lines (flatten (reverse generated)))
	  (push (insert-tags-into-line line tags marker-regex) generated))))


(defun insert-tags-into-line (line tags marker-regex)
  (if (zerop (length line))
	  ""
	  (let ((split (split-line line
							   (ppcre:all-matches marker-regex line))))
		(do ((i 0 (1+ i)))
			((>= i (length split)))
		  (let ((marker (multiple-value-list 
						 (ppcre:scan-to-strings marker-regex
												(nth i split)))))
			(if (not (null (car marker)))
				(progn
				  (setf (nth i split) 
						(get-tag-data (svref (cadr marker) 0) tags))))))
;;		(format t "split: ~s~%" split)
		(if (reduce (lambda (a b) (and a b)) (mapcar #'atom split))	   
			(apply #'concatenate 'string (flatten split))
			(flatten-block-tag split)))))

;; (defun flatten-line-tree (strlis &optional prev)
;;   (if (null strlis)
;; 	  (if (null prev)
;; 		  nil
;; 		  (concatenate 'string prev nil))
;; 	  (let ((-car (car strlis))
;; 			(-cdr (cdr strlis)))
;; 		(if (listp -car)
;; 			(let ((child (flatten-line-tree -car)))
;; 			  (cons (concatenate 'string prev (car child)) ; concatenate child line's head
;; 					(append (subseq child 1 (1- (length child)))
;; 							(flatten-line-tree -cdr (lastcar child)))))
;; 			(let ((-cadr (cadr strlis))
;; 				  (--car (concatenate 'string prev -car))) ; concatenate child line's last
;; 			  (if (and (listp -cadr) (not (null -cadr)))
;; 				  (flatten-line-tree -cdr --car)
;; 				  (cons --car (flatten-line-tree -cdr))))))))



(defun flatten-block-tag (strlis &optional (lis '("")))
;;		   (format t "~S~%" lis)
		   (if (null strlis)
			   lis
			   (let ((-car (car strlis))
					 (-cdr (cdr strlis)))
				 (if (atom -car)
					 (flatten-block-tag -cdr
								 (replace-last lis
											   (concatenate 'string (lastcar lis) -car)))
					 (flatten-block-tag -cdr (flatten-inner-list -car lis))))))

(defun flatten-inner-list (strlis lis)
  (flet ((concat (s1 s2)
		   (concatenate 'string s1 s2)))
	(case (length strlis)
	  (0 lis)
	  (1 (replace-last lis
				(concat (lastcar lis) (car strlis))))
	  (2 (append (replace-last lis (concat (lastcar lis) (car strlis)))
				 (last strlis)))
	  (otherwise (append (replace-last lis (concat (lastcar lis) (car strlis)))
						 (subseq strlis 1
								 (- (length strlis) 1))
						 (last strlis))))))

(defun replace-last (lis var)
  (let ((l (copy-tree lis)))
	(replace l (list var) :start1 (1- (length l)))
	l))



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

