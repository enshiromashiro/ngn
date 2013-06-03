#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn
  (:use :cl
		:cl-annot)
  (:import-from :ngn.tag-parser
				:parse-tags)
  (:import-from :ngn.text-io
				:read-text))
(in-package :ngn)

(cl-annot:enable-annot-syntax)


;;(let ((text (read-text "hoge.txt")))
;;  (format t "~a~%" (parse-tags text)))
