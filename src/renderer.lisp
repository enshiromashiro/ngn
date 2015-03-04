#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer
  (:use :cl)
  (:import-from :ngn.render-dsl
                :eval-tags)
  (:import-from :cl-ppcre
                :regex-replace-all
                :scan)
  (:import-from :inquisitor
                :detect-external-format)
  (:export :render))
(in-package :ngn.renderer)


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
    (loop
       for line = (readline)
       for outline = line then line
       until (eq line :eof)
       do (let ((markers (included-markers tags line)))
            (if (null markers)
                (write-line line out)
                (dolist (tag (included-markers tags line))
                  (setf outline
                        (regex-replace-all (make-marker-regex tag)
                                           outline
                                           (gethash tag tags)))
                  (write-line outline out)))))))


(defun render (tags outstream tmpstream rndrpath)
  "render text from _tags_ and _tmpstream_ to _outstream_ with `renderer` at _rndpath_.
_tag_ is a hashtable. 
`renderer` is a file include functions called by render-dsl module"
  (embed-into-template (eval-tags tags rndrpath) tmpstream outstream))
