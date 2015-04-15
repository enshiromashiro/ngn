#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.dsl
  (:use :cl)
  (:export :render-tags))
(in-package :ngn.dsl)


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


;;;; syntax error notification
(defparameter *line-number* nil)
(defparameter *tag-name* nil)

(defun init-linum ()
  (setf *line-number* 1))
(defun init-tag-name ()
  (setf *tag-name* nil))

(defun incf-linum ()
  (incf *line-number*))
(defun set-tag-name (name)
  (setf *tag-name* name))

(define-condition ngn-syntax-error (condition)
  ((linum :initform *line-number*
          :reader syntax-error-linum)
   (tagname :initform *tag-name*
            :reader syntax-error-tagname)
   (message :initform ""
            :initarg :msg
            :reader syntax-error-message))
  (:report (lambda (condition stream)
             (format stream
                     "[ngn]: syntax error at line ~a of \"~a\" tag.~%~a~%"
                     (syntax-error-linum condition)
                     (syntax-error-tagname condition)
                     (syntax-error-message condition)))))

(defun syntax-error (msg)
  (error (make-condition 'ngn-syntax-error :msg msg)))

(defun unexpected-xxx-error (obj char)
  (syntax-error
   (format nil "Unexpected ~a while looking for '~a'" obj char)))

(defun unexpected-eof-error (char)
  (unexpected-xxx-error "EOF" char))


;;;; utilities
(defun read-to (char stream)
  (with-output-to-string (out)
    (loop
       for c = (peek-char nil stream nil :eof)
       until (or (eq c char) (eq c :eof))
       do (write-char (read-char stream) out))))

(defun get-level (char stream)
  (let* ((level 0)
         (str (with-output-to-string (out)
                (loop
                   for c = (peek-char nil stream nil :eof)
                   while (eq c char)
                   do (incf level)
                      (write-char c out)
                      (read-char stream nil :eof)))))
    (values level str)))

(defmacro with-level ((levvar strvar char stream) &body body)
  `(multiple-value-bind (,levvar ,strvar) (get-level ,char ,stream)
     ,@body))


;;;; sharp reader
(defun bracket-reader (stream)
  (if (eq #\[ (peek-char nil stream nil :eof))
      (with-output-to-string (out)
        (read-char stream)
        (loop
           for c = (read-char stream nil :eof)
           when (eq c #\newline) do (unexpected-xxx-error "NEWLINE" #\])
           when (eq c :eof) do (unexpected-eof-error #\])
           until (eq c #\])
           do (write-char c out)))
      (syntax-error (format nil "'~a' is not '['"
                            (peek-char nil stream nil "EOF")))))

(defun element-reader (stream)
  (let ((kind (format nil "~a~a" (read-char stream) (read-char stream))))
    (cond ((string= kind "rb") ; for ruby
           (render-ruby (bracket-reader stream) (bracket-reader stream)))
          ((string= kind "em") ; for emphasis
           (render-emphasis (bracket-reader stream)))
          ((string= kind "bd") ; for bold
           (render-bold (bracket-reader stream)))
          ((string= kind "it") ; for italic
           (render-italic (bracket-reader stream)))
          ((string= kind "bi") ; for bold-italic
           (render-bold-italic (bracket-reader stream)))
          ((string= kind "ul") ; for underline
           (render-underline (bracket-reader stream)))
          (t (syntax-error (format nil "'~a' is not ngn-element" kind))))))

(defun sharp-reader (stream linehead-p)
  (read-char stream)
  (cond ((alpha-char-p (peek-char nil stream))
         (element-reader stream))
        (t (with-level (lev str #\# stream)
             (if (and linehead-p (< lev +render-header-level-max+))
                 (let ((ch (read-char stream)))
                   (if (eq ch #\space)
                       (render-header (read-to #\newline stream) (1+ lev))
                       (format nil "#~a~a" str ch)))
                 (format nil "#~a" str))))))


;;;; greater reader
(defun line-reader (line)
  (with-input-from-string (in line)
    (with-output-to-string (out)
      (loop
         for c = (peek-char nil in nil :eof)
         until (eq c :eof)
         do (case c
              (#\# (write-string (sharp-reader in nil) out))
              (otherwise (write-char (read-char in) out)))))))

(defun greater-reader (stream linehead-p)
  (if linehead-p
      (with-output-to-string (out)
        (let ((now-level)
              (lines)
              (remain-str))
          (flet ((quote-level-p (level)
                   (and (< 0 level)
                        (<= level +render-quote-level-max+)))
                 (push-line ()
                   (push (line-reader (read-line stream)) lines)
                   (incf-linum))
                 (render-lines ()
                   (render-quote (format nil "~{~a~^~%~}" (nreverse lines))
                                 now-level)))
            (loop 
               for lvlist = (multiple-value-list (get-level #\> stream))
               for level = (first lvlist)
               for gtstr = (second lvlist)
               for firstqt? = t then nil
               while (quote-level-p level)
               finally (unless (null lines)
                         (write-string (render-lines) out))
                       (unless (quote-level-p level)
                         (write-string gtstr out))
                       (when remain-str
                         (write-string remain-str out))
               do (when (null now-level)
                    (setf now-level level))
                 (if (eq #\space (peek-char nil stream nil :eof))
                     (progn
                       (read-char stream)
                       (if (eq now-level level)
                           (push-line)
                           (progn
                             (write-line (render-lines) out)
                             (setf now-level level
                                   lines nil)
                             (push-line))))
                     (setf remain-str
                           (if firstqt?
                               (format nil "~a" gtstr)
                               (format nil "~%~a" gtstr))))))))
      (string (read-char stream nil ""))))


;;;; dsl reader
(defun dsl-reader (stream)
  (with-output-to-string (out)
    (loop
       for c = (peek-char nil stream nil :eof)
       for linehead? = t then (if prev-newline? t nil)
       with prev-newline? = nil
       when prev-newline? do (incf-linum)
       until (eq c :eof)
       do (setf prev-newline? (eq c #\newline))
         (case c
           (#\# (write-string (sharp-reader stream linehead?) out))
           (#\> (write-string (greater-reader stream linehead?) out))
           (otherwise (write-char (read-char stream) out))))))


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
      (dsl-reader in))))


(defparameter *render-hook* #'identity)

(defun render-tags (tags renderer-path)
  (let ((*package* (find-package :ngn.dsl)))
    (load renderer-path))
  (let ((new-tags (make-hash-table)))
    (maphash (lambda (k v)
               (init-linum)
               (set-tag-name k)
               (setf (gethash k new-tags) (render-tag v))
               (init-tag-name))
             tags)
    (when (functionp *render-hook*)
      (funcall *render-hook* new-tags))
    new-tags))
