#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn
  (:use :cl
        :cl-annot)
  (:import-from :ngn.parser
                :parse)
  (:import-from :ngn.renderer
                :render)
  (:import-from :inquisitor
                :detect-external-format)
  (:import-from :trivial-shell
                :exit)
  (:import-from :unix-options
                :&free
                :print-usage-summary
                :with-cli-options))
(in-package :ngn)

(enable-annot-syntax)


(defvar *app-name*
  "ngn - novel page generator")

(defvar *usage-string*
  "Usage: ngn TEMPLATE
       ngn TEMPLATE INFILE
       ngn TEMPLATE INFILE OUTFILE
Render a text page to OUTFILE based on TEMPLATE and INFILE.
If execute ngn with only TEMPLATE, ngn reads from stdin and
writes to stdout.
Output path is based on INFILE (or current directory).

Options:~%~@{~a~%~}~%")

(defvar *usage-option-spec*
  '((("debug") nil "Print debug messages")
    ((#\v "version") nil "Print version")
    ((#\h "help") nil "Print this help")))


(defun print-version ()
  (format t "~a [Version ~a]~%"
          *app-name*
          (slot-value (asdf:find-system :ngn) 'asdf:version)))

(defun print-usage ()
  (format t "~a~%" *app-name*)
  (print-usage-summary *usage-string* *usage-option-spec*))


(defparameter *debug* nil)
(defun dbg (fmt &rest rest)
  (when *debug*
    (apply #'format `(t ,fmt ,@rest))))


(defvar *renderer-dirname* "renderer/")


(defun runtime-pathname ()
  #+sbcl
  (make-pathname :name nil :type nil
                 :defaults sb-ext:*runtime-pathname*)
  #+ccl
  (truename (make-pathname :host "ccl")))

(defun make-renderer-path (tmppath &optional renderer-basepath)
  (merge-pathnames
   (concatenate 'string
                (string-downcase (pathname-type tmppath))
                "-renderer.lisp")
   (if (null renderer-basepath)
       *renderer-dirname*
       (merge-pathnames *renderer-dirname* renderer-basepath))))


@export
(defun ngn (tmpstream &optional instream outstream)
  (let* ((tmppath (truename tmpstream))
         (tags (parse instream))
         (renderer-path
          (make-renderer-path tmppath (runtime-pathname))))
    (render tags outstream tmpstream renderer-path)))

@export
(defun app ()
  (with-cli-options ()
      (debug version help &free free)
    (when debug
      (setf *debug* t))
    (cond (version (print-version)
                   (exit))
          (help (print-usage)
                (exit))
          ((zerop (length free)) (print-usage)
                                 (exit))
          (t (handler-case
                 (let ((ef (detect-external-format (pathname (first free)) :jp)))
                   (with-open-file (tmpstream (first free)
                                    :direction :input
                                    :external-format ef)
                     (case (length free)
                       (1 (ngn tmpstream *standard-input* *standard-output*))
                       (2 (with-open-file (instream (second free)
                                           :direction :input
                                           :external-format (detect-external-format (pathname (second free)) :jp))
                            (ngn tmpstream instream *standard-output*)))
                       (t (with-open-file (instream (second free)
                                           :direction :input
                                           :external-format
                                           (detect-external-format (pathname (second free)) :jp))
                            (with-open-file (outstream (third free)
                                             :direction :output
                                             :if-exists :supersede
                                             :external-format ef)
                              (ngn tmpstream instream outstream)))))))
               (condition (c)
                 (progn (format t "~%error caused!: ~a~%~%" c)
                        (exit 1))))))))
