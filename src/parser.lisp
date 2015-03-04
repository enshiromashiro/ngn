#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.parser
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:export :parse))
(in-package :ngn.parser)


(defconstant +ngn-tag-char+ #\:)
(defconstant +ngn-comment-char+ #\;)

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
        (let ((ch0 (char line 0)))
          (cond
            ((eq ch0 +ngn-tag-char+)
             (if (eq len 1)
                 :ngn-error-too-short
                 (let ((ch1 (char line 1)))
                   (cond
                     ((eq ch1 +ngn-comment-char+) :ngn-escape-comment)
                     ((eq ch1 +ngn-tag-char+) :ngn-escape-tag)
                     (t :ngn-tag)))))
             ((eq ch0 +ngn-comment-char+) :ngn-comment)
             (t :plain))))))

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
  (let ((line-type (determine-line-type line)))
    (ecase line-type
      (:plain line)
      (:ngn-comment nil)
      (:ngn-escape-comment (concatenate 'string ";" (subseq line 2)))
      (:ngn-escape-tag (concatenate 'string ":" (subseq line 2)))
      (:ngn-tag (parse-tag line))
      (:ngn-error-too-short (error "invalid ngn syntax")))))


(defun trim-empty-lines (lines)
  "Trim empty lines from head and tail of _lines_.
If _lines_ is nil, then returns nil."
  (when lines
    (flet ((not-empty-position (lines from-end-p)
             (position "" lines
                       :test-not #'equal
                       :from-end from-end-p)))
      (let ((begin (not-empty-position lines nil))
            (end (not-empty-position lines t)))
        (cond ((and (null begin) (null end)) '(""))
              ((and (eq begin 0) (eq end (1- (length lines)))) lines)
              (t (subseq lines begin (1+ end))))))))

(defun parse (stream)
  "Parse the ngn-syntax text lines from input stream _stream_.
The ngn-syntax are mainly consist of three elements.

The elements are __tag__, __comment__ and __plain text__.
And ngn-syntax has two escape sequences.

## Tag
__Tag__ is mainly concept of ngn.
It is a name of data following the __tag__.

__Tag__ begins with colon (':') only at line head.
Tag-name, is called _tag-identifier_, must be consist of
lower alphabets (a-z), digits (0-9) and hyphen (-).

There are three kind of __tag__, those are __oneline__, __block__ and __dummy__

### Oneline Tag
__Oneline tag__ is used naming _one-line_ data, for example,
title, author's name, date and other __short__ text data.

Syntax is below:

    :[tag-identifier] [data]

Note that oneline-tag needs only one space after tag-identifier.
If two spaces after tag-identifier, the second space is included data.

### Block Tag
__Block tag__ is used naming _multi-line_ data, for example,
article, novel, postscript and other __long__ text data.
The end of __block tag__ is appearance of next tag or EOF.

Note that empty lines an head and tail of text data, will _trim_.

Syntax is below:

    :[tag-identifier]
    line1...
    line2...
    ...

### Dummy Tag
__Dummy tag__ is used hiding following text data (ex. memo) or
forcing end previous __block tag__.

Syntax is below:

    :[tag-identifier]:
    line1...
    line2...
    ...

## Comment
__Comment__ is used describing comment in __block__ data.

Syntax is below:

    ;[comment-string]

## Escape sequence
If you want to write ':' or ';' at head of line,
use below escape sequences:

### for colon (:)
    ::[string]

is read as

    :[string]

### for semicolon (;)
    :;[string]

is read as

    ;[string]

"
  (let ((hash (make-hash-table))
        (lines)
        (tag))
    (labels ((set-tag (tag lines)
               (setf (gethash (intern (string-upcase (svref (cdr tag) 0)))
                              hash) lines))
             (push-lines ()
               (unless (or (null lines) (null tag))
                 (set-tag tag
                          (reduce (lambda (a b)
                                    (concatenate 'string b (string #\newline) a))
                                  (trim-empty-lines lines))))))
      (loop
         for line = (read-line stream nil :eof) then (read-line stream nil :eof)
         until (eq line :eof)
         for parsed = (parse-line line) then (parse-line line)
         finally (when (and (not (null tag)) (eq :block (car tag)))
                   (push "" lines))
                 (push-lines)
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


