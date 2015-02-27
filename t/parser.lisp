#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.parser-test
  (:use :cl
        :ngn.parser
        :prove))
(in-package :ngn.parser-test)


(plan nil)


(deftest determine-line-type
  (is (ngn.parser::determine-line-type "") :plain)

  (is (ngn.parser::determine-line-type ":") :ngn-error-too-short)
  (is (ngn.parser::determine-line-type "::not tag") :ngn-escape-tag)
  (is (ngn.parser::determine-line-type ":;not comment") :ngn-escape-comment)
  (is (ngn.parser::determine-line-type ":tag-name") :ngn-tag)

  (is (ngn.parser::determine-line-type ";") :ngn-comment)
  (is (ngn.parser::determine-line-type ";str") :ngn-comment)

  (is (ngn.parser::determine-line-type "str") :plain))


(deftest parse-tag
  (is (ngn.parser::parse-tag ":tag")
      '(:block . #("tag")) :test #'equalp)
  (is-error (ngn.parser::parse-tag ":tag ") 'error) ; *invalid syntax*
  (is (ngn.parser::parse-tag ":tag data")
      '(:oneline . #("tag" "data")) :test #'equalp)
  (is-error (ngn.parser::parse-tag "::") 'error) ; *invalid syntax*
  (is (ngn.parser::parse-tag ":tag:")
 '(:dummy . #("tag")) :test #'equalp)
    
  ;; for unexpected args
  (is-error (ngn.parser::parse-tag ":") 'error) ; *invalid syntax*
  (is-error (ngn.parser::parse-tag ":;not-tag") 'error) ; escape
  (is-error (ngn.parser::parse-tag "::not-tag") 'error)) ; essape


(deftest parse-line
  (is (ngn.parser::parse-line "") "")
  (is (ngn.parser::parse-line "str") "str")

  (is (ngn.parser::parse-line "; comment") nil)
  (is (ngn.parser::parse-line ":;") ";")
  (is (ngn.parser::parse-line "::") ":")

  (is (ngn.parser::parse-line ":test")
      '(:block . #("test")) :test #'equalp)
  (is (ngn.parser::parse-line ":test data")
      '(:oneline . #("test" "data")) :test #'equalp)
  (is (ngn.parser::parse-line ":test-:")
      '(:dummy . #("test-")) :test #'equalp)

  (is-error (ngn.parser::parse-line ":BLOCK") 'error)
  (is-error (ngn.parser::parse-line ":block_") 'error)

  (is-error (ngn.parser::parse-line ":onelineA data") 'error)
  (is-error (ngn.parser::parse-line ":oneline_ data") 'error)

  (is-error (ngn.parser::parse-line ":dummyA:") 'error)
  ; (is-error (ngn.parser::parse-line ":dummy :") 'error)
  (is (ngn.parser::parse-line ":dummy :")
      '(:oneline . #("dummy" ":")) :test #'equalp)

  (is-error (ngn.parser::parse-line ":") 'error))



(defun hash-eq (hash kv-pairs)
  (and (eq (hash-table-count hash) (length kv-pairs))
       (or (null kv-pairs)
           (loop
              for pair in kv-pairs
              always
                (destructuring-bind (key value) pair
                  (multiple-value-bind (v p) (gethash key hash)
                    (and p (string= v value))))))))

(defun test-parse (str kv-pairs)
  (with-input-from-string (in str)
    (apply #'hash-eq (list (ngn.parser:parse in) kv-pairs))))

(deftest parse
  (ok (test-parse "" nil)))



(run-test-all)

(finalize)
