#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn
  (:use :cl
        :cl-annot)
  (:import-from :unix-options
                :getopt
                :cli-options)
  (:import-from :ngn.tag-parser
                :parse-tags)
  (:import-from :ngn.text-io
                :read-text
                :write-text)
  (:import-from :ngn.generator
                :generate)
  (:import-from :util
                :quit
                :gen-keyword
                :dbg))
(in-package :ngn)

(cl-annot:enable-annot-syntax)


(defvar *cli-short-options* nil
  "command line short options.")

(defvar *cli-long-options* '("debug")
  "command line long options.")

(defvar *usage*
  '("USAGE: ngn [DATAFILE] [TEMPLATE]"
    "Generate text file from DATAFILE and TEMPLATE"
    "(Current directory by default output directory)."
    ""
    "option:"
    "--debug"
    "  print debugging messages."))

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


@export
(defun ngn (text temp type &key (tag-hook #'identity))
"ngn main procedure.
ngn: text, temp, type, post-proc-tag -> generated-text
*args
text: input file. a list of strings.
temp: template file. a list of strings.
type: file type of template. keyword.
tag-hook: tags -> tags. hook for extracted tags."
  (if (and (null text) (null temp))
      (print-usage)
      (let* ((tags (parse-tags text))
             (hooked (funcall tag-hook tags))
             (gen (generate type hooked temp)))
        (dbg `("parsed tags..." ,@tags))
        (if (not (tree-equal tags hooked :test #'equal))
            (dbg `("hooked tags..." ,@hooked)))
        (generate type hooked temp))))

@export
(defun app ()
  "toplevel-function"
  (multiple-value-bind (_ opts args)
      (getopt (cli-options) *cli-short-options* *cli-long-options*)
    (format t "~a~%" *ngn*)

    (if (member "debug" opts :test #'equal)
        (setf util:*debug* t))
    (dbg (format nil "args: ~s" (cli-options)))

    (let ((input-file (nth 0 args))
          (template-file (nth 1 args)))
      (if (null template-file)
          (print-usage)
          (handler-case
              (write-text (determine-output-filepath input-file template-file)
                          (ngn (get-text input-file "input-file")
                               (get-text template-file "template-file")
                               (gen-keyword (pathname-type template-file))))
            (condition (c)
              (progn
                (format t "~%error caused!: ~a~%~%" c)
                (quit 1))))))))
