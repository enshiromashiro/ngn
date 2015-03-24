;;;; ngn renderer for html
;;;; In this file, you can use functions and macros in CL-USER package


(require :cl-ppcre)


;;; for auto-indexing
(defparameter *-index-* nil)
(defparameter *header-number* 1)

(defstruct index-elem
  id text)

(defun %add-leaf (elem level tree &optional (lev 1))
  (if (eq level lev)
      (if (atom tree)
          (list tree elem)
          (append tree (list elem)))
      (if (atom tree)
          (list tree
                (%add-leaf elem level nil (1+ lev)))
          (append (list (car tree))
                  (butlast (cdr tree))
                  (list (%add-leaf elem level
                                   (car (last (cdr tree))) (1+ lev)))))))

(defun add-header (level text)
  (setf *-index-*
        (%add-leaf (make-index-elem :id *header-number*
                                    :text text)
                   level
                   *-index-*))
  (incf *header-number*))

(defun render-elem (index-elem &optional (end? t))
  (with-output-to-string (out)
    (format out "<li>")
    (format out "<a href=\"#sec~a\">~a</a>"
            (index-elem-id index-elem)
            (index-elem-text index-elem))
    (when end?
      (format out "</li>"))
    (format out "~%")))

(defun %render-index (index &optional (level 0))
  (with-output-to-string (out)
    (unless (null index)
      (let ((tab (format nil "~~~a,0T" (* 2 level))))
        (flet ((%format (fmt &rest rest)
                 (apply #'format
                        `(,out ,(concatenate 'string tab fmt) ,@rest))))
          (%format "<ol>~%")
          (loop
             for e in index
             unless (listp e) do
               (%format (render-elem e))
             else do
               (%format "~a~a</li>~%"
                        (if (null (car e))
                            (format nil "<li>~%")
                            (render-elem (car e) nil))
                        (%render-index (cdr e) (1+ level))))
          (%format "</ol>~%"))))))

(defun render-index (index)
  (%render-index (cdr index)))

(defun add-index (hash)
  (setf (gethash '-index- hash)
        (render-index *-index-*)))

(setf *render-hook* #'add-index)



;;; render functions
;; per-line rendering
(defun render-per-line (line)
  (if (ppcre:scan "^#\{1,4\} " line)
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
  (prog1
      (format nil "<h~a><a name=\"sec~a\">~a</a></h~a>"
              level *header-number* text level)
    (add-header level text)))

;; quote rendering
(defun render-quote (text level)
  (format nil "<p class=\"quote~a\">~%~a~%</p>~%" level text))

