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


(deftest trim-empty-line
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

(deftest parse
  (diag "null or ignored strings only")
  (ok (test-parse "" nil))
  (ok (test-parse "
" nil))
  (ok (test-parse "Katamari Damacy" nil))
  (ok (test-parse "We ♥ Katamari
" nil))
 
  (diag "comment")
  (ok (test-parse "; Katamari on the Rock" nil))
  (ok (test-parse "; Katamari on the Swing
" nil))
  (ok (test-parse "; Katamari on the Funk
; Katamari Dancing" nil))

  (diag "tags")
  (ok (test-parse ":title みんな大好き塊魂"
                  '((title "みんな大好き塊魂"))))


  (ok (test-parse ":body

エブリデイ エブリバディ
君と王様のレインボー (yes!)
愛のメッセージ

"
                  '((body "エブリデイ エブリバディ
君と王様のレインボー (yes!)
愛のメッセージ"))))

  (diag "block tag with empty lines")
  (ok (test-parse ":body" '((body ""))))
  (ok (test-parse ":body
"
                  '((body ""))))
  (ok (test-parse ":body

"
                  '((body ""))))

  (diag "block tag with null data")
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
                  '((body ""))))

  (diag "ignoring dummy")
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
いつでも Slime for you"))))

  (diag "note that this is regarded oneline tag")
  (ok (test-parse ":not-dummy :" '((not-dummy ":"))))

  (diag "invalid syntax")
  (is-error (test-parse ":title_name test" nil) 'error))



(run-test-all)

(finalize)
