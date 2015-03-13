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
  (if (eq #\[ (peek-char nil stream))
      (with-string-output-stream (out)
        (with-read-char
          (read-char stream)
          (loop
             for c = (readch)
             if (eq c :eof) do (ngn-error "unexpected EOF")
             until (eq c #\])
             do (write-char c out))
          (get-output-stream-string out)))
      (ngn-error "\"~a\" is not open-bracket" (peek-char nil stream))))

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


;;;; greater reader
(defun line-reader (line)
  (with-input-from-string (in line)
    (with-output-to-string (out)
      (loop
         for c = (read-char in nil :eof)
         until (eq c :eof)
         do (case c
              (#\# (write-string (sharp-reader in nil) out))
              (otherwise (write-char c out)))))))

(defun greater-reader (stream linehead-p)
  (if linehead-p
      (with-output-to-string (out)
        (let ((now-level)
              (lines))
          (flet ((quote-level-p (level)
                   (and (< 0 level)
                        (<= level +render-quote-level-max+)))
                 (push-line ()
                   (push (line-reader (read-line stream)) lines))
                 (render-lines ()
                   (write-line
                    (render-quote (format nil "~{~a~%~}" (nreverse lines))
                                  now-level)
                    out)))
            (loop 
               for lvlist = (multiple-value-list (get-level #\> stream))
               for level = (1+ (first lvlist)) then (first lvlist)
               for gtstr = (second lvlist)
               while (quote-level-p level)
               finally (unless (null lines)
                         (render-lines))
                       (unless (quote-level-p level)
                         (write-string gtstr out))
               do (when (null now-level)
                    (setf now-level level))
                 (if (eq #\space (peek-char nil stream nil :eof))
                     (progn
                       (read-char stream)
                       (if (eq now-level level)
                           (push-line)
                           (progn
                             (render-lines)
                             (setf now-level level
                                   lines nil)
                             (push-line))))
                     (format out "~a~a" gtstr (read-char stream nil "")))))))
      ">"))


;;;; dsl reader
(defun dsl-reader (stream)
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
      (dsl-reader in))))


(defun render-tags (tags renderer-path)
  (let ((*package* (find-package :ngn.render-dsl)))
    (load renderer-path))
  (let ((new-tags (make-hash-table)))  
    (maphash (lambda (k v) (setf (gethash k new-tags) (render-tag v)))
             tags)
    new-tags))