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
         | OPEN-PAREN CONTRACT ... DOT ARROW DOT CONTRACT CLOSE-PAREN
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
  (OPEN-PAREN CLOSE-PAREN ELLIPSIS DOT))

(define contract-lexer
  (lexer-src-pos ["..." (token-ELLIPSIS)]
                 [(:or "(" "[") (token-OPEN-PAREN)]
                 [(:or ")" "]") (token-CLOSE-PAREN)]
                 [(:: "#:"
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z)
                               #\^ #\% #\~ #\- #\: #\< #\>
                               #\+ #\* #\$ #\@ #\! #\& #\=
                               #\_ #\/ #\? #\# #\\
                               #\.)))
                  (token-KEYWORD (read (open-input-string lexeme)))]
                 [(:: (:+ (:or (:/ #\0 #\9) #\# #\" #\'))
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z)
                               #\^ #\% #\~ #\- #\: #\< #\>
                               #\+ #\* #\$ #\@ #\! #\& #\=
                               #\_ #\/ #\? #\# #\\
                               #\.)))
                  (token-LITERAL (read (open-input-string lexeme)))]
                 [(:or (:: #\.
                           (:+ (:or (:/ #\a #\z) (:/ #\A #\Z)
                                    #\^ #\% #\~ #\- #\: #\< #\>
                                    #\+ #\* #\$ #\@ #\! #\& #\=
                                    #\_ #\/ #\? #\# #\\
                                    #\.)))
                       (:: (:+ (:or (:/ #\a #\z) (:/ #\A #\Z)
                                    #\^ #\% #\~ #\- #\: #\< #\>
                                    #\+ #\* #\$ #\@ #\! #\& #\=
                                    #\_ #\/ #\? #\# #\\))
                           (:* (:or (:/ #\a #\z) (:/ #\A #\Z)
                                    #\^ #\% #\~ #\- #\: #\< #\>
                                    #\+ #\* #\$ #\@ #\! #\& #\=
                                    #\_ #\/ #\? #\# #\\
                                    #\.))))
                  (token-IDENTIFIER (string->symbol lexeme))]
                 [#\. (token-DOT)]
                 [(eof) eof] ; to e.g. use with apply-lexer from brag/support
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
     (check-equal? (token-name (position-token-token (first (lex-source "->")))) 'IDENTIFIER)
     (check-equal? (position-token-token (first (lex-source "."))) 'DOT)))

  (void (run-tests lexer-tests)))
