#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "functional-lib"
               "megaparsack-lib"
               "megaparsack-parser-tools"
               "parser-tools-lib"
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
(define pkg-desc "High-level, composable syntax for describing contracts.")
(define version "2.0")
(define pkg-authors '(countvajhula))
