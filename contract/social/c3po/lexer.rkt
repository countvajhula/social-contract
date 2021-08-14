#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

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
LITERAL
ARROW
|#

(define-tokens contract (IDENTIFIER LITERAL))
(define-empty-tokens contract-empty
  (OPEN-PAREN CLOSE-PAREN ELLIPSIS))

(define contract-lexer
  (lexer-src-pos ["..." (token-ELLIPSIS)]
                 [(:or "(" "[") (token-OPEN-PAREN)]
                 [(:or ")" "]") (token-CLOSE-PAREN)]
                 [(:: (:+ (:or (:/ #\0 #\9) #\# #\" #\'))
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z)
                               #\^ #\% #\~ #\- #\: #\< #\>
                               #\+ #\* #\$ #\@ #\! #\& #\=
                               #\_ #\/ #\? #\# #\\
                               #\.)))
                  (token-LITERAL (read (open-input-string lexeme)))]
                 [(:: (:+ (:or (:/ #\a #\z) (:/ #\A #\Z)
                               #\^ #\% #\~ #\- #\: #\< #\>
                               #\+ #\* #\$ #\@ #\! #\& #\=
                               #\_ #\/ #\? #\# #\\
                               #\.))
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z)
                               #\^ #\% #\~ #\- #\: #\< #\>
                               #\+ #\* #\$ #\@ #\! #\& #\=
                               #\_ #\/ #\? #\# #\\
                               #\.)))
                  (token-IDENTIFIER (string->symbol lexeme))]
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
