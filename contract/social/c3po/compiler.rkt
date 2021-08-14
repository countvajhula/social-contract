#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         racket/format
         syntax/parse/define
         mischief/shorthand
         version-case)

(require "lexer.rkt"
         "parser.rkt")

(provide upcompile)

;; this represents a single "pass" of the compiler
(define/contract (compile-pass src parser)
  (-> string? parser? any/c)
  (parse-result!
   (parse-tokens parser
                 (lex-source src))))

;; upcompile performs multiple compiler passes until the input
;; converges to steady state. Each pass performs a full cycle
;; from string to string so that the subsequent pass can use
;; the lexer afresh and is essentially independent
(define/contract (upcompile src parser)
  (-> string? procedure? string?)
  (let ([result (with-handlers ([exn:fail? (λ (e) src)])
                  (~a (compile-pass src parser)))])
    (if (equal? src result)
        result
        (upcompile result parser))))
