#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer
  (:use :cl
        :cl-annot)
  (:import-from :ngn.render-dsl
                :eval-tags)
  (:import-from :cl-ppcre
                :regex-replace-all
                :scan)
  (:import-from :inquisitor
                :detect-external-format))
(in-package :ngn.renderer)

(cl-annot:enable-annot-syntax)


(defvar *renderer-dirname* "render/")

(defun make-renderer-path
    (tmppath &optional (runtime-path *runtime-pathname*))
  (merge-pathnames
   (concatenate 'string
                (string-downcase (pathname-type tmppath))
                "-renderer.lisp")
   (merge-pathnames *renderer-dirname* runtime-path)))


(defun make-marker-regex (name)
  (etypecase name
    (string (format nil "#\\|~a\\|#" name))
    (symbol (format nil "#\\|~a\\|#" (string-downcase name)))))

(defun included-markers (tags line)
  (let ((markers))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (scan (make-marker-regex k) line)
                 (pushnew k markers)))
             tags)
    markers))

(defun embed-into-template (tags in out)
  (flet ((readline () (read-line in nil :eof)))
    (loop for line = (readline) then (readline)
       for outline = line then line
       until (eq line :eof)
       do (dolist (tag (included-markers tags line))
            (setf outline
                  (regex-replace-all (make-marker-regex tag)
                                     outline
                                     (gethash tag tags)))
            (write-line outline out)))))


@export
(defun render (tags tmppath out-stream)
  "render text from _tags_ and _tmppath_ to _out-stream_ with `renderer`.
_tag_ is a hashtable. _tmppath_ is a pathname.
`renderer` is a file include tag"
  (let ((ef (detect-external-format tmppath :jp)))
    (with-open-file (in-stream tmppath
                     :direction :input
                     :external-format ef)
      (embed-into-template (eval-tags tags (make-renderer-path tmppath))
                           in-stream out-stream))))
