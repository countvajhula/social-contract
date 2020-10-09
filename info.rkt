#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "cover"
                     "cover-coveralls"
                     "sandbox-lib"))
(define compile-omit-paths '("dev" "coverage" "tests"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc"))
(define pkg-desc "Common contracts.")
(define version "0.0")
(define pkg-authors '(countvajhula))
