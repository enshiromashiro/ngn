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
                :read-text
                :write-text)
  (:import-from :ngn.generator
                :generate)
  (:import-from :util
				:args))
(in-package :ngn)

(cl-annot:enable-annot-syntax)


;;(let ((text (read-text "hoge.txt")))
;;  (format t "~a~%" (parse-tags text)))

(defvar *usage*
  '("USAGE: ngn [DATAFILE] [TEMPLATE]"
    "Generate text file from DATAFILE and TEMPLATE (Current directory by default output directory)."))

(defvar *ngn*
  "ngn - novel page generator")

(defun print-usage ()
  (dolist (line *usage*)
    (format t "~a~%" line)))


(defun get-text (filepath description)
  (if (null filepath)
      (format t "cannot read ~a: '~a'.~%" description filepath)
      (let ((text (read-text filepath)))
        (if (or (null text) (eq text :does-not-exists))
            (format t "cannot read ~a: '~a'.~%" description filepath)
            (progn
              (format t "~a: '~a'~%" description filepath)
              text)))))

(defun determine-output-filepath (input-filepath temp-filepath)
  (let ((output-filepath (concatenate 'string
                              "./"
                              (pathname-name input-filepath)
                              "."
                              (pathname-type temp-filepath))))
    (format t "output file: ~a~%" output-filepath)
    output-filepath))


(defun ngn-main (data-filepath temp-filepath)
  (format t "~a~%" *ngn*)
  (let ((text (get-text data-filepath "input-file"))
        (temp (get-text temp-filepath "template-file")))
    (if (and (null text) (null temp))
        (print-usage)
        (write-text (determine-output-filepath data-filepath temp-filepath)
                    (generate (intern (string-upcase (pathname-type temp-filepath)) 'keyword)
                              (parse-tags text)
                              temp)))))


;; for clozure cl
@export
(defun call-main ()
  (let ((args (cdr (args))))
	(format t "[debug] args: ~s~%" args)
	(handler-case
		(ngn-main (nth 0 args) (nth 1 args))
	  (condition (c)
		(format t "error caused!: ~a~%" c)))))
