#|
  This file is a part of ngn project.
  Copyright (c) 2013 subaru45
|#

(in-package :cl-user)
(defpackage ngn.parser-test
  (:use :cl
        :ngn.error
        :ngn.parser
        :prove))
(in-package :ngn.parser-test)


(plan 5)

(defmacro tests-with-syntax-error (linum msg &body body)
  `(handler-case
       (progn
         (init-linum)
         ,@body)
     (ngn-syntax-error (c)
       (let ((linum-got (ngn.error::syntax-error-linum c))
             (msg-got (ngn.error::syntax-error-message c)))
         (is linum-got ,linum)
         (is msg-got ,msg)))
     (condition (c)
       (fail (format nil "unexpected error ~a" c)))
     (:no-error (v)
       (declare (ignore v))
       (fail "no error occurs"))))


(subtest "Testing determine-line-type"
  (is (ngn.parser::determine-line-type "") :plain)

  (is (ngn.parser::determine-line-type ":") :ngn-error-too-short)
  (is (ngn.parser::determine-line-type "::not tag") :ngn-escape-tag)
  (is (ngn.parser::determine-line-type ":;not comment") :ngn-escape-comment)
  (is (ngn.parser::determine-line-type ":tag-name") :ngn-tag)

  (is (ngn.parser::determine-line-type ";") :ngn-comment)
  (is (ngn.parser::determine-line-type ";str") :ngn-comment)

  (is (ngn.parser::determine-line-type "str") :plain))


(subtest "Testing parse-tag"
  (is (ngn.parser::parse-tag ":tag")
      '(:block . #("tag")) :test #'equalp)
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':tag '"
    (ngn.parser::parse-tag ":tag ")) ; *invalid syntax*
  (is (ngn.parser::parse-tag ":tag data")
      '(:oneline . #("tag" "data")) :test #'equalp)
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: '::'"
    (ngn.parser::parse-tag "::")) ; *invalid syntax*
  (is (ngn.parser::parse-tag ":tag:")
 '(:dummy . #("tag")) :test #'equalp)
    
  ;; for unexpected args
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':'"
    (ngn.parser::parse-tag ":")) ; *invalid syntax*
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':;not-tag'"
    (ngn.parser::parse-tag ":;not-tag")) ; escape
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: '::not-tag'"
    (ngn.parser::parse-tag "::not-tag"))) ; essape


(subtest "Testing parse-line"
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

  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':BLOCK'"
    (ngn.parser::parse-line ":BLOCK"))
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':block_'"
    (ngn.parser::parse-line ":block_"))

  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':onelineA data'"
    (ngn.parser::parse-line ":onelineA data"))
  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':oneline_ data'"
    (ngn.parser::parse-line ":oneline_ data"))

  (tests-with-syntax-error
    1 "Invalid tag syntax in this line: ':dummyA:'"
    (ngn.parser::parse-line ":dummyA:"))
  ; (is-error (ngn.parser::parse-line ":dummy :") 'error)
  (is (ngn.parser::parse-line ":dummy :")
      '(:oneline . #("dummy" ":")) :test #'equalp)

  (tests-with-syntax-error
    1 "No tag name in this line: ':'"
    (ngn.parser::parse-line ":")))


(subtest "Testing trim-empty-line"
  (is (ngn.parser::trim-empty-lines nil) nil)
  (is (ngn.parser::trim-empty-lines '("")) '(""))
  (is (ngn.parser::trim-empty-lines '("a" "b")) '("a" "b"))
  (is (ngn.parser::trim-empty-lines '("" "a")) '("a"))
  (is (ngn.parser::trim-empty-lines '("a" "")) '("a"))
  (is (ngn.parser::trim-empty-lines '("" "a" "")) '("a")))


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

(subtest "Testing parse"
  (subtest "null or ignored strings only"
    (ok (test-parse "" nil))
    (ok (test-parse "
" nil))
    (ok (test-parse "Katamari Damacy" nil))
    (ok (test-parse "We ♥ Katamari
" nil)))
 
  (subtest "comment"
    (ok (test-parse "; Katamari on the Rock" nil))
    (ok (test-parse "; Katamari on the Swing
" nil))
    (ok (test-parse "; Katamari on the Funk
; Katamari Dancing" nil)))

  (subtest "tags"
    (ok (test-parse ":title みんな大好き塊魂"
                    '((title "みんな大好き塊魂"))))

    (ok (test-parse ":body

エブリデイ エブリバディ
君と王様のレインボー (yes!)
愛のメッセージ

"
                    '((body "エブリデイ エブリバディ
君と王様のレインボー (yes!)
愛のメッセージ")))))

  (subtest "block tag with empty lines"
    (ok (test-parse ":body" '((body ""))))
    (ok (test-parse ":body
"
                    '((body ""))))
    (ok (test-parse ":body

"
                    '((body "")))))

  (subtest "block tag with null data"
    (ok (test-parse ":body
:title test"
                    '((body "")
                      (title "test"))))
    (ok (test-parse ":body
:ps"
                    '((body "")
                      (ps ""))))
    (ok (test-parse ":body
:dummy:"
                    '((body "")))))

  (subtest "escape sequences"
    (ok (test-parse ":;ignored" nil))
    (ok (test-parse "::ignored" nil))
    (ok (test-parse ":body
:;not-comment"
                    '((body ";not-comment"))))
    (ok (test-parse ":body
::not-blocktag"
                    '((body ":not-blocktag"))))
    (ok (test-parse ":body
:; not-comment
::not-blocktag"
                    '((body "; not-comment
:not-blocktag")))))

  (subtest "ignoring dummy"
    (ok (test-parse ":body

ナナーナナナナナーナーナーナ
塊魂ー

:dummy:

おそろいのTシャツ(Yeah!)
手編みのマフラーと(Oh yeah!)

:body2

固めて転がして I love you
いつでも Slime for you

"
                    '((body "ナナーナナナナナーナーナーナ
塊魂ー")
                      (body2 "固めて転がして I love you
いつでも Slime for you")))))

  (subtest "note that this is regarded oneline tag"
    (ok (test-parse ":not-dummy :" '((not-dummy ":")))))

  (subtest "invalid syntax"
    (tests-with-syntax-error
      1 "Invalid tag syntax in this line: ':title_name test'"
      (test-parse ":title_name test" nil) 'error)))


(finalize)
