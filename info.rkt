#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "mischief"
               "version-case"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "collections-doc"
                     "cover"
                     "cover-coveralls"
                     "sandbox-lib"
                     "adjutor"))
(define pkg-desc "Common contracts.")
(define version "0.0")
(define pkg-authors '(countvajhula))
