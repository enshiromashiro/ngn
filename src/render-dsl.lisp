#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.render-dsl
  (:use :cl
        :cl-annot))
(in-package :ngn.render-dsl)

(cl-annot:enable-annot-syntax)


;;;; these functions are defined by user
;;;; * render-per-line: line => line
;;;; * render-ruby: text ruby => text
;;;; * render-bold: text => text
;;;; * render-italic: text => text
;;;; * render-bold-italic: text => text
;;;; * render-underline: text => text
;;;; * render-header: text level => text
;;;; * render-quote: text level => text


(defvar +render-header-level-max+ 4)
(defvar +render-quote-level-max+ 4)


;;;; utilities
(defun ngn-error (fmt &rest args)
  (error (apply #'format
                `(nil ,(format nil "[ngn] ~a" fmt) ,@args))))

(defmacro with-read-char (&body body)
  `(flet ((readch () (read-char stream nil :eof)))
    ,@body))

(defmacro with-string-output-stream ((var) &body body)
  `(let ((,var (make-string-output-stream)))
     (unwind-protect
          ,@body
       (close ,var))))

(defun read-to (char stream)
  (with-output-to-string (out)
    (loop
       for c = (peek-char nil stream nil :eof)
       until (or (eq c char) (eq c :eof))
       do (write-char (read-char stream) out))))

(defun get-level (char stream)
  (let ((level 0))
    (with-string-output-stream (out)
      (with-read-char
        (loop
           for c = (readch)
           unless (eq c char) do (unread-char c stream)
           while (eq c char)
           do (incf level)
             (write-char c out))
        (values level (get-output-stream-string out))))))

(defmacro with-level ((levvar strvar char stream) &body body)
  `(multiple-value-bind (,levvar ,strvar) (get-level ,char ,stream)
     ,@body))


;;;; reader
(defun paren-reader (stream)
  (if (eq #\( (peek-char nil stream))
      (with-string-output-stream (out)
        (with-read-char
          (read-char stream)
          (loop
             for c = (readch)
             if (eq c :eof) do (ngn-error "unexpected EOF")
             until (eq c #\))
             do (write-char c out))
          (get-output-stream-string out)))
      (ngn-error "\"~a\" is not open-paren" (peek-char nil stream))))

(defun element-reader (stream)
  (let ((kind (format nil "~a~a" (read-char stream) (read-char stream))))
    (cond ((string= kind "rb") ; for ruby
           (render-ruby (paren-reader stream) (paren-reader stream)))
          ((string= kind "em") ; for emphasis
           (render-emphasis (paren-reader stream)))
          ((string= kind "bd") ; for bold
           (render-bold (paren-reader stream)))
          ((string= kind "it") ; for italic
           (render-italic (paren-reader stream)))
          ((string= kind "bi") ; for bold-italic
           (render-bold-italic (paren-reader stream)))
          ((string= kind "ul") ; for underline
           (render-underline (paren-reader stream)))
          (t (ngn-error "'~a' is not ngn-element" kind)))))

(defun sharp-reader (stream linehead-p)
  (cond ((alpha-char-p (peek-char nil stream))
         (element-reader stream))
        (t (with-level (lev str #\# stream)
             (if (and linehead-p (< lev +render-header-level-max+))
                 (if (eq (read-char stream) #\space)
                     (render-header (read-to #\newline stream) (1+ lev))
                     (format nil "~a " str))
                 (format nil "#~a" str))))))

(defun greater-reader (stream linehead-p)
  (if linehead-p
      (with-level (lev str #\> stream)
        (if (< lev +render-quote-level-max+)
            (if (eq (read-char stream) #\")
                (prog1 (render-quote (read-to #\" stream) (1+ lev))
                       (read-char stream nil))
                (format nil "~a\"" str))
            (format nil ">~a" str)))
      ">"))


;;;; dsl reader
(defun read-dsl (stream)
  (let ((linum 1)
        (linehead? t))
    (declare (ignore linum))
    (with-output-to-string (out)
      (with-read-char
        (loop
           for c = (readch)
           until (eq c :eof)
           do (case c
                (#\# (write-string (sharp-reader stream linehead?) out))
                (#\> (write-string (greater-reader stream linehead?) out))
                (otherwise (write-char c out)))
           when (eq c #\newline) do (incf linum) (setf linehead? t)
           else do (setf linehead? nil))))))


(defun render-tag (str)
  (flet ((multiline (str)
           (with-input-from-string (in str)
             (with-output-to-string (out)
               (loop
                  for line = (read-line in nil :eof)
                  until (eq line :eof)
                  do (write-line (render-per-line line) out))))))
    (with-input-from-string (in (if (find #\newline str)
                                    (multiline str)
                                    str))
      (read-dsl in))))


@export
(defun eval-tags (tags renderer-path)
  (let ((*package* (find-package :ngn.render-dsl)))
    (require (intern (string-upcase (pathname-name renderer-path)))
             renderer-path))
  (let ((new-tags (make-hash-table)))  
    (maphash (lambda (k v) (setf (gethash k new-tags) (render-tag v)))
             tags)
    new-tags))
