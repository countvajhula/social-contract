#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         racket/function)

(require "base-parsers.rkt"
         "contract-parser.rkt")

(provide provide/p)

(define contracted-function/p
  (do (token/p 'OPEN-PAREN)
      [id <- (identifier/p)]
    [ctcs <- (many/p (or/p (try/p contract/p)
                           symex/p))]
    (token/p 'CLOSE-PAREN)
    (pure (list* id ctcs))))

(define contract-out/p
  (do (token/p 'OPEN-PAREN)
      [form-name <- (identifier/p 'contract-out)]
    [a <- (many/p contracted-function/p)]
    (token/p 'CLOSE-PAREN)
    (pure (list* form-name a))))

(define provide/p
  (do (token/p 'OPEN-PAREN)
      [form-name <- (identifier/p 'provide)]
    [as <- (many/p
            (or/p (try/p contract-out/p)
                  symex/p))]
    (token/p 'CLOSE-PAREN)
    (pure (list* form-name as))))
