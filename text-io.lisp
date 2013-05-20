#|
  ngn: novel page generator

  This file is a part of ngn.
  Copyright (c) 2013 subaru45
|#

;; (inpackage #:cl-user)

;; (defpackage #:ngn
;;   (:import-from #:cl-user)
;;   (:import-from #:cl-ppcre)
;;   (:export #:ngn))


(defun guess-encoding (filepath)
  :utf-8)

(defun guess-line-termination (filepath)
  :windows)


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
	  (if (null in)
		  'does-not-exists
		  (flet ((get-line (fin)
				   (read-line fin nil 'eof)))
			(do ((line (get-line in) (get-line in)))
				((eq line 'eof) (reverse text))
			  (setf text (cons line text))))))))
  
