;;; novel page generator
;;; author: subaru


;; (inpackage #:cl-user)

;; (defpackage #:ngn
;;   (:import-from #:cl-user)
;;   (:import-from #:cl-ppcre)
;;   (:export #:ngn))

(ql:quickload :cl-ppcre)

(defconstant +regex-oneline-tag+ ":(.+?)[ ]+(.+)")
(defconstant +regex-block-tag+ ":(.+?)(\\[|\\])")


(defun guess-encoding (filepath)
  nil)

(defun guess-line-termination (filepath)
  nil)

(defun gen-keyword (name)
  (car (multiple-value-list (intern (string-upcase name)
									:keyword))))

(defun empty-string-p (str)
  (if (zerop (length str))
	  str
	  nil))

(defun block-p (tag)
  (if (null tag)
	  nil
	  (let ((data (cadr tag)))
		(or (string= data "[")
			(string= data "]")))))

(defun parse-oneline-tag (line)
  (multiple-value-bind (_ tags)
	  (cl-ppcre:scan-to-strings +regex-oneline-tag+ line)
	(coerce tags 'list)))

(defun parse-block-tag (line)
  (multiple-value-bind (_ tags)
	  (cl-ppcre:scan-to-strings +regex-block-tag+ line)
	(coerce tags 'list)))

(defun scan-tag (line)
  (if (empty-string-p line)
	  nil
	  (let ((btag (parse-block-tag line)))
		(if (null btag)
			(parse-oneline-tag line)
			btag))))


;; Experimental code.
;; TODO: test. refactoring.
(defun get-tag-data (text)
  (let ((data))
	(do ((i 0 (1+ i)))
		((>= i (length text)) (reverse data))
	  (let ((tag (scan-tag (nth i text))))
		(if (not (null tag))
			(if (block-p tag)
				(push (list (gen-keyword (car tag))
							(let ((d))
							  (incf i)
							  (do ((line (nth i text) (nth i text)))
								  ((block-p (scan-tag line)))
								(push line d)
								(incf i))
							  (reverse d)))
					  data)
				(push (list (gen-keyword (car tag))
							(cadr tag))
					  data)))))))


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
  
