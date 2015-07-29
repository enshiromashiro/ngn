#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn-ext
  (:use :cl)
  (:import-from :ngn.parser
                :parse)
  (:import-from :inquisitor
                :detect-external-format)
  (:import-from :trivial-shell
                :exit)
  (:import-from :unix-options
                :&free
                :print-usage-summary
                :with-cli-options)
  (:export :ngn-extract
           :app))
(in-package :ngn-ext)


(defvar *app-name*
  "ngn-ext - ngn-tag extracter")

(defvar *usage-string*
  "Usage: ngn-ext TAGNAME
       ngn-ext TAGNAME FILE
Extract a tag fron FILE.

Options:~%~@{~a~%~}~%")

(defvar *usage-option-spec*
  '(((#\v "version") nil "Print version")
    ((#\h "help") nil "Print this help")))


(defun print-version ()
  (format t "~a [Version ~a]~%"
          *app-name*
          (slot-value (asdf:find-system :ngn) 'asdf:version)))

(defun print-usage ()
  (format t "~a~%" *app-name*)
  (print-usage-summary *usage-string* *usage-option-spec*))


(defvar *renderer-dirname* "renderer/")


(defun ngn-extract (tagname instream outstream)
  (let ((tags (parse instream))
        (tagsym (intern (string-upcase tagname))))
    (write-string (gethash tagsym tags) outstream)))

(defun app ()
  (with-cli-options ()
      (debug version help &free free)
    (cond (version (print-version)
                   (exit))
          (help (print-usage)
                (exit))
          ((zerop (length free)) (print-usage)
                                 (exit))
          (t (handler-case
                 (case (length free)
                   (1 (ngn-extract (first free) *standard-input* *standard-output*))
                   (2 (with-open-file (instream (second free)
                                       :direction :input
                                       :external-format (detect-external-format (pathname (second free)) :jp))
                        (ngn-extract (first free) instream *standard-output*)))
                   (t (print-usage)
                      (exit)))
               (condition (c)
                 (progn (format t "~%error caused!~%~a~%" c)
                        (exit 1)))))))
  (exit 0))
