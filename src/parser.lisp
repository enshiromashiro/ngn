#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.parser
  (:use :cl
        :cl-annot)
  (:import-from :cl-ppcre
                :scan-to-strings))
(in-package :ngn.parser)

(enable-annot-syntax)


(defvar *ngn-tag-char* #\:)
(defvar *ngn-comment-char* #\;)

(defvar *ngn-identifier* "([a-z0-9-]+)")
(defvar *ngn-tag-block*
  (concatenate 'string "^:" *ngn-identifier* "$"))
(defvar *ngn-tag-oneline*
  (concatenate 'string "^:" *ngn-identifier* " (.+)$"))
(defvar *ngn-tag-dummy*
  (concatenate 'string "^:" *ngn-identifier* ":$"))

(defun determine-line-type (line)
  "Determine a type of line"
  (let ((len (length line)))
    (if (eq len 0)
        :plain
        (case (char line 0)
          (#.*ngn-tag-char*
           (if (eq len 1)
               :ngn-error-too-short
               (case (char line 1)
                 (#.*ngn-comment-char* :ngn-escape-comment)
                 (#.*ngn-tag-char* :ngn-escape-tag)
                 (otherwise :ngn-tag))))
          (#.*ngn-comment-char* :ngn-comment)
          (otherwise :plain)))))

(defun scan-tag (regex line)
  (multiple-value-bind (_ tag)
      (ppcre:scan-to-strings regex line)
    (declare (ignore _))
    (when (< 0 (length tag))
      tag)))

(defun parse-tag (line)
  "Determine a tag type of line.
This function expect a line is distinguished a :ngn-tag by determine-line-type function."
  (let ((oneline (scan-tag *ngn-tag-oneline* line))
        (blocktag (scan-tag *ngn-tag-block* line))
        (dummy (scan-tag *ngn-tag-dummy* line)))
    (cond (oneline (cons :oneline oneline))
          (blocktag (cons :block blocktag))
          (dummy (cons :dummy dummy))
          (t (error "invalid ngn syntax")))))

(defun parse-line (line)
  (let ((type (determine-line-type line)))
    (ecase type
      (:plain line)
      (:ngn-comment nil)
      (:ngn-escape-comment (concatenate 'string ";" (subseq line 2)))
      (:ngn-escape-tag (concatenate 'string ":" (subseq line 2)))
      (:ngn-tag (parse-tag line))
      (:ngn-error-too-short (error "invalid ngn syntax")))))


(defun trim-empty-lines (lines)
  "Trim empty lines from head and tail of _lines_.
If _lines_ is nil, then returns nil."
  (flet ((not-empty-position (lines from-end-p)
             (position "" lines
                       :test-not #'equal
                       :from-end from-end-p)))
    (let ((begin (not-empty-position lines nil))
          (end (not-empty-position lines t)))
      (cond ((and (null begin) (null end)) nil)
            ((and (eq begin 0) (eq end (1- (length lines)))) lines)
            (t (subseq lines begin (1+ end)))))))

@export
(defun parse (stream)
  (let ((hash (make-hash-table))
        (lines)
        (tag))
    (labels ((set-tag (tag lines)
               (setf (gethash (make-symbol (string-upcase (svref (cdr tag) 0)))
                              hash) lines))
             (push-lines ()
               (unless (or (null lines) (null tag))
                 (set-tag tag (nreverse lines)))))
      (loop
         for line = (read-line stream nil :eof) then (read-line stream nil :eof)
         until (eq line :eof)
         for parsed = (parse-line line) then (parse-line line)
         finally (push-lines)
         do (etypecase parsed
              (null nil)
              (cons (flet ((tag-eq (type)
                             (eq type (car parsed))))
                      (push-lines)
                      (setf lines nil
                            tag nil)
                      (when (tag-eq :oneline)
                        (set-tag parsed (svref (cdr parsed) 1)))
                      (when (tag-eq :block)
                        (setf tag parsed))))
              (string (when (and (not (null tag)) (eq :block (car tag)))
                        (push parsed lines)))))
      hash)))


