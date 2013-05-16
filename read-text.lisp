;;; read-text.lisp


;; (inpackage #:cl-user)

;; (defpackage #:ngn
;;   (:import-from #:cl-user))

(defconstant +default-encoding :utf-8)

(defun guess-encoding (filepath)
  nil)

(defun guess-line-termination (filepath)
  nil)

;; It works only when filepath exists.
(defun read-text (filepath)
  (let* ((text)
		 (enc :utf-8)
		 (lt :windows)
		 (in (open filepath
				   :direction :input
				   :external-format (make-external-format :character-encoding enc
														  :line-termination lt)
				   :if-does-not-exist :error)))
	(if (eq in :error)
		in
		(flet ((get-line (fin)
				 (read-line fin nil 'eof)))
		  (do ((line (get-line in) (get-line in)))
			  ((eq line 'eof) (reverse text))
			(setf text (cons line text)))))))
