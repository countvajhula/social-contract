#lang racket

(require megaparsack
         megaparsack/parser-tools/lex)

(require "contract-parser.rkt"
         "provide-parser.rkt")

(provide spec/p)

(define spec/p
  (or/p (try/p provide/p)
        (try/p contract/p)))
