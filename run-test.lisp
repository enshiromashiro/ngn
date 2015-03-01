;;;; How to run test
;;;; 1. load this file to Impl

(require :asdf)
(require :trivial-shell)

(require :ngn)
(asdf:test-system :ngn)

(trivial-shell:exit)

