#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.error
  (:use :cl)
  (:export :init-linum
           :incf-linum
           :init-tag-name
           :set-tag-name

           :ngn-syntax-error

           :dsl-syntax-error
           :unexpected-xxx-error
           :unexpected-eof-error))
(in-package :ngn.error)


;;;; syntax error notification
(defparameter *line-number* 1)
(defparameter *tag-name* nil) ; for dsl

(defun init-linum ()
  (setf *line-number* 1))
(defun incf-linum ()
  (incf *line-number*))

(defun init-tag-name ()
  (setf *tag-name* nil))
(defun set-tag-name (name)
  (setf *tag-name* name))


;;;; syntax error for parser
(define-condition ngn-syntax-error (condition)
  ((linum :initform *line-number*
          :reader syntax-error-linum)
   (message :initform ""
            :initarg :msg
            :reader syntax-error-message))
  (:report (lambda (condition stream)
             (format stream
                     "[parser] syntax error at line ~a of input.~%~a~%"
                     (syntax-error-linum condition)
                     (syntax-error-message condition)))))

(defun ngn-syntax-error (msg)
  (error (make-condition 'ngn-syntax-error :msg msg)))


;;;; syntax error for DSL
(define-condition dsl-syntax-error (ngn-syntax-error)
  ((tagname :initform *tag-name*
            :reader syntax-error-tagname))
  (:report (lambda (condition stream)
             (format stream
                     "[DSL] syntax error at line ~a of \"~a\" tag.~%~a~%"
                     (syntax-error-linum condition)
                     (syntax-error-tagname condition)
                     (syntax-error-message condition)))))

(defun dsl-syntax-error (msg)
  (error (make-condition 'dsl-syntax-error :msg msg)))

(defun unexpected-xxx-error (obj char)
  (dsl-syntax-error
   (format nil "Unexpected ~a while looking for '~a'" obj char)))

(defun unexpected-eof-error (char)
  (unexpected-xxx-error "EOF" char))


