;;;; ngn renderer for pixiv
;;;; In this file, you can use functions and macros in CL-USER package

(require :cl-ppcre)

;; per-line rendering
(defun render-per-line (line)
  line)

;; ruby rendering
(defun render-ruby (text ruby)
  (format nil "[[rb:~a > ~a]]" text ruby))

;; emphasis rendering
(defun render-emphasis (text)
  text)

;; bold rendering
(defun render-bold (text)
  text)

;; italic rendering
(defun render-italic (text)
  text)

;; bold-italic rendering
(defun render-bold-italic (text)
  text)

;; underline rendering
(defun render-underline (text)
  text)

;; header rendering
(defun render-header (text level)
  (declare (ignore level))
  (format nil "[chapter:~a]" text))

;; quote rendering
(defun render-quote (text level)
  (with-input-from-string (in text)
    (with-output-to-string (out)
      (flet ((readline () (read-line in nil :eof)))
        (loop
           for line = (readline)
           until (eq line :eof)
           do (dotimes (i level)
                (write-string "　" out))
              (write-line line out))))))

