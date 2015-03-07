;;;; ngn renderer template
;;;; In this file, you can use functions and macros in CL-USER package


(require :cl-ppcre)

;; per-line rendering
(defun render-per-line (line)
  (if (ppcre:scan "^#+ " line)
      line
      (format nil "~a$" line)))

;; ruby rendering
(defun render-ruby (text ruby)
  (format nil "rb-~a~a" text ruby))

;; emphasis rendering
(defun render-emphasis (text)
  (format nil "em-~a" text))

;; bold rendering
(defun render-bold (text)
  (format nil "bd-~a" text))

;; italic rendering
(defun render-italic (text)
  (format nil "it-~a" text))

;; bold-italic rendering
(defun render-bold-italic (text)
  (format nil "bi-~a" text))

;; underline rendering
(defun render-underline (text)
  (format nil "ul-~a" text))

;; header rendering
(defun render-header (text level)
  (format nil "hd~a-~a" level text))

;; quote rendering
(defun render-quote (text level)
  (format nil "qt~a-~a" level text))

