;;; novel page generator
;;; author: subaru


;; (inpackage #:cl-user)

;; (defpackage #:ngn
;;   (:import-from #:cl-user)
;;   (:import-from #:cl-ppcre)
;;   (:export #:ngn))

(ql:quickload :cl-ppcre)

(defconstant +tag-block-delimiter-start+ #\[)
(defconstant +tag-block-delimiter-end+ #\])

(defconstant +tag-regex-oneline+ ":(.+?)[ ]+(.+)$")
(defconstant +tag-regex-block+
  (concatenate 'string
			   ":(.+?)(\\"
			   (string +tag-block-delimiter-start+)
			   "|\\"
			   (string +tag-block-delimiter-end+)
			   ")$"))


(defun guess-encoding (filepath)
  :utf-8)

(defun guess-line-termination (filepath)
  :windows)

(defun gen-keyword (name)
  (car (multiple-value-list (intern (string-upcase name)
									:keyword))))

(defun empty-string-p (str)
  (if (zerop (length str))
	  str
	  nil))

(defun scan-tag (line tag-regex)
  (if (and (stringp line) (empty-string-p line))
	  nil
	  (multiple-value-bind (_ tag) (cl-ppcre:scan-to-strings tag-regex line)
		(if (null _)
			nil
			(list (gen-keyword (svref tag 0)) (svref tag 1))))))

(defun scan-oneline-tags (text)
  (let ((tags))
	(dolist (line text (reverse tags))
	  (let ((tag (scan-tag line +tag-regex-oneline+)))
		(if (not (null tag))
			(push tag tags))))))

(defun scan-block-tags (text &optional tag-name lines)
  (if (null text)
	  nil
	  (flet ((tag-p (tag delim)
			   (and (not (null tag)) (string= (cadr tag) delim))))
		(if (null tag-name)
			(let ((tag (scan-tag (car text) +tag-regex-block+)))
			  (if (tag-p tag +tag-block-delimiter-start+)
				  (scan-block-tags (cdr text) (car tag))
				  (scan-block-tags (cdr text))))
			(let ((tag (scan-tag (car text) +tag-regex-block+)))
			  (if (and (tag-p tag +tag-block-delimiter-end+)
					   (eq tag-name (gen-keyword (car tag))))
				  (cons (list tag-name lines)
						(scan-block-tags (cdr text)))
				  (scan-block-tags (cdr text) 
								   tag-name
								   (reverse (cons (car text)
												  (reverse lines))))))))))

(defun scan-tags (text)
  (append (scan-oneline-tags text)
		  (scan-block-tags text)))

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
	  (if (eq in nil)
		  'does-not-exists
		  (flet ((get-line (fin)
				   (read-line fin nil 'eof)))
			(do ((line (get-line in) (get-line in)))
				((eq line 'eof) (reverse text))
			  (setf text (cons line text))))))))
  
