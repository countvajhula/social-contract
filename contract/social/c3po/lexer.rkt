#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(module+ test
  (require rackunit
           rackunit/text-ui))

(provide lex
         lex-source)

#|
Grammar:
CONTRACT = IDENTIFIER
         | LITERAL
         | OPEN-PAREN ARROW CONTRACT ... CLOSE-PAREN
         | OPEN-PAREN IDENTIFIER CONTRACT ... CLOSE-PAREN

Terminals:
OPEN-PAREN
CLOSE-PAREN
IDENTIFIER
KEYWORD
LITERAL
ARROW
|#

(define-tokens contract (IDENTIFIER LITERAL KEYWORD))
(define-empty-tokens contract-empty
  (OPEN-PAREN CLOSE-PAREN ELLIPSIS))

(define-lex-abbrev content-character
  ;; non-whitespace and not a structural character like ( or )
  (:& (:or alphabetic numeric symbolic punctuation graphic)
      (complement (:or "(" ")" "[" "]"))))

(define contract-lexer
  (lexer-src-pos ["..." (token-ELLIPSIS)]
                 [(:or "(" "[") (token-OPEN-PAREN)]
                 [(:or ")" "]") (token-CLOSE-PAREN)]
                 [(:: "#:" (:* content-character))
                  (token-KEYWORD (read (open-input-string lexeme)))]
                 [(:: (:+ (:or (:/ #\0 #\9) #\# #\" #\'))
                      (:* content-character))
                  (token-LITERAL (read (open-input-string lexeme)))]
                 [(:+ content-character)
                  (token-IDENTIFIER (string->symbol lexeme))]
                 [(eof) eof]  ; to e.g. use with apply-lexer from brag/support
                 [whitespace (void)]))

(define (lex str lexer)
  (let ([port (open-input-string str)])
    (let loop ([token (lexer port)])
      (cond [(eof-object? (position-token-token token)) null]
            [(void? (position-token-token token)) (loop (lexer port))]
            [else (cons token (loop (lexer port)))]))))

(define (lex-source src)
  (lex src contract-lexer))

(module+ test
  (define lexer-tests
    (test-suite
     "lexer tests"
     (check-equal? (position-token-token (first (lex-source "("))) 'OPEN-PAREN)
     (check-equal? (token-name (position-token-token (first (lex-source "5")))) 'LITERAL)
     (check-equal? (token-name (position-token-token (first (lex-source "#:key")))) 'KEYWORD)
     (check-equal? (token-name (position-token-token (first (lex-source "->")))) 'IDENTIFIER)))

  (void (run-tests lexer-tests)))
