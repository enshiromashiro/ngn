#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.renderer-test
  (:use :cl
        :ngn.parser
        :prove))
(in-package :ngn.renderer-test)


(plan 3)


(subtest "Testing make-marker-regex"
  (flet ((call-it (name)
           (ngn.renderer::make-marker-regex name)))

    (is (call-it "test") "#\\|test\\|#")
    (is (call-it "TEST") "#\\|test\\|#")
    (is (call-it 'test) "#\\|test\\|#")
    (is (call-it '|test|) "#\\|test\\|#")))


(subtest "Testing included-markers"
  (flet ((call-it (tags line)
           (ngn.renderer::included-markers tags line)))
    (let ((hash (make-hash-table)))
      (setf (gethash 'body hash) "本文")

      (is (call-it hash "#|body|#") '(body))
      (is (call-it hash "#|TITLE|#") '())
      (is (call-it hash "#|title|# #|body|#") '(body)))

    (let ((hash (make-hash-table)))
      (setf (gethash 'body hash) "本文")
      (setf (gethash 'title hash) "題名")

      (is (call-it hash "#|body|#") '(body))
      (is (call-it hash "#|TITLE|#") '())
      (is (call-it hash "#|title|# #|body|#") '(body title)))))


(subtest "Testing #'embed-into-template"
  (flet ((call-it (str tags)
           (with-input-from-string (in str)
             (with-output-to-string (out)
               (ngn.renderer::embed-into-template tags in out)))))

    (let ((hash (make-hash-table)))
      (setf (gethash 'title hash) "題名")
      (setf (gethash 'body hash) "本文")

      (is (call-it "#|title|# - #|body|#" hash)
          "題名 - 本文")

      (is (call-it "<html><head>
<title>#|title|#</title>
</head>
<body>
<h1>#|titl|# - これはタイポ</h1>
#|body|#
</body></html>" hash)
          "<html><head>
<title>題名</title>
</head>
<body>
<h1>#|titl|# - これはタイポ</h1>
本文
</body></html>
"))))


(finalize)
