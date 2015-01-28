#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.tag-parser
  (:use :cl
        :cl-annot)
  (:import-from :alexandria
                :emptyp)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :util
                :gen-keyword))
(in-package :ngn.tag-parser)

(enable-annot-syntax)


(defconstant +tag-block-delimiter-end+ #\])

(defconstant +tag-identifer-valid-chars+ "[a-z0-9-]")
(defconstant +tag-regex-oneline+ 
  (concatenate 'string
               "^:("
               +tag-identifer-valid-chars+
               "+?)[ ]+(.+)$"))
(defconstant +tag-regex-block+
  (concatenate 'string
               "^:("
               +tag-identifer-valid-chars+
               "+?)(\\"
               (string +tag-block-delimiter-start+)
               "|\\"
               (string +tag-block-delimiter-end+)
               ")$"))


@export
;; whoa...
(defun get-tag-data (tag-name tags)
  (let ((data)
        (rev-tags (reverse tags)))
    (dolist (tag rev-tags data)
      (if (eq (gen-keyword tag-name) (car tag))
          (setf data (cadr tag))))))


@export
(defun parse-tags (text)
  (append (parse-oneline-tags text)
          (parse-block-tags text)))

(defun parse-oneline-tags (text)
  (let ((tags))
    (dolist (line text (reverse tags))
      (let ((tag (parse-oneline-tag line)))
        (if (not (null tag))
            (push tag tags))))))

(defun parse-block-tags (text &optional tag-name lines)
  (if (null text)
      (if (not (null tag-name))
          :parse-error-at-eof
          nil)
      (flet ((tag-p (tag delim)
               (and (not (null tag)) (string= (cadr tag) delim))))
        (let ((tag (parse-block-tag (car text))))
          (if (null tag-name)
              (if (tag-p tag +tag-block-delimiter-start+)
                  (parse-block-tags (cdr text) (car tag))
                  (parse-block-tags (cdr text)))
              (if (and (tag-p tag +tag-block-delimiter-end+)
                       (eq tag-name (gen-keyword (car tag))))
                  (cons (list tag-name lines)
                        (parse-block-tags (cdr text)))
                  (parse-block-tags (cdr text) 
                                    tag-name
                                    (if (null (parse-oneline-tag (car text)))
                                        (reverse (cons (car text)
                                                       (reverse lines)))
                                        lines))))))))

(defun parse-block-tag (line)
  (parse-tag line +tag-regex-block+))

(defun parse-oneline-tag (line)
  (parse-tag line +tag-regex-oneline+))

(defun parse-tag (line tag-regex)
  (if (and (stringp line) (emptyp line))
      nil
      (multiple-value-bind (_ tag) (scan-to-strings tag-regex line)
        (if (< (length tag) 2)
            nil
            (list (gen-keyword (svref tag 0)) (svref tag 1))))))
