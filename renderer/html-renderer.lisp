;;;; ngn renderer for html
;;;; In this file, you can use functions and macros in CL-USER package


(require :cl-ppcre)

;; per-line rendering
(defun render-per-line (line)
  (if (ppcre:scan "^#+ " line)
      line
      (format nil "~a<br>" line)))

;; ruby rendering
(defun render-ruby (text ruby)
  (format nil "<ruby>~a<rp>（</rp><rt>~a</rt><rp>）</rp></ruby>" text ruby))

;; emphasis rendering
(defun render-emphasis (text)
  (format nil "<span class=\"emphasis\">~a</span>" text))

;; bold rendering
(defun render-bold (text)
  (format nil "<span class=\"bold\">~a</span>" text))

;; italic rendering
(defun render-italic (text)
  (format nil "<span class=\"italic\">~a</span>" text))

;; bold-italic rendering
(defun render-bold-italic (text)
  (format nil "<span class=\"bold-italic\">~a</span>" text))

;; underline rendering
(defun render-underline (text)
  (format nil "<span class=\"underline\">~a</span>" text))

;; header rendering
(defun render-header (text level)
  (format nil "<h2 class=\"header~a\">~a</h2>" level text))

;; quote rendering
(defun render-quote (text level)
  (format nil "<p class=\"quote~a\">~%~a~%</p>~%" level text))

