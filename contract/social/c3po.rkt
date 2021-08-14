#lang racket

(require megaparsack
         megaparsack/text
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         parser-tools/lex
         racket/function
         (prefix-in : parser-tools/lex-sre)
         racket/format
         syntax/parse/define
         mischief/shorthand
         version-case)

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(provide translate)

(module+ test
  (require rackunit
           rackunit/text-ui))

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

(define (identifier/p [name #f])
  (guard/p (token/p 'IDENTIFIER)
           (λ (result)
             (or (not name)
                 (eq? name result)))))

(define (literal/p [name #f])
  (guard/p (token/p 'LITERAL)
           (λ (result)
             (or (not name)
                 (eq? name result)))))

(define arrow/p
  (identifier/p '->))

;; We prefer to reduce all contracts before going to next level
;; so that we don't need to retain lower-level special handling
;; at higher levels and can always assume the inputs are in
;; reduced / minimal form
(define reducible-maybe/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'maybe/c)
    [a <- contract/p]
    (literal/p '#f)
    (token/p 'CLOSE-PAREN)
    (pure (list 'maybe/c a))))

(define free-maybe/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'or/c)
    [a <- contract/p]
    [default <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'maybe/c a default))))

(define maybe/p
  (or/p (try/p free-maybe/p)
        (try/p reducible-maybe/p)))

(define function/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? 'any/c a)
             (eq? 'any/c b))
        (pure 'function/c)
        (pure (list 'function/c a b)))))

(define predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c a)
        (pure 'predicate/c)
        (pure (list 'predicate/c a)))))

(define encoder/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    (identifier/p 'any/c)
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c a)
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "can't encode to any/c")))
        (pure (list 'encoder/c a)))))

(define decoder/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    (identifier/p 'any/c)
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c a)
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "can't decode from any/c")))
        (pure (list 'decoder/c a)))))

(define hash-function/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'encoder/c)
    (identifier/p 'fixnum?)
    (token/p 'CLOSE-PAREN)
    (pure 'hash-function/c)))

(define classifier/p
  ;; This is very specific atm, expecting sequenceof and sequence?
  ;; but should ideally accept e.g. listof and list?
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'encoder/c) ; note: does not recognize classifying by any/c
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    (identifier/p 'sequence?)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    (identifier/p 'sequence?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'classifier/c a))))

(define generic-sequence/p
  (or/p (try/p (do (token/p 'OPEN-PAREN)
                   (identifier/p 'sequenceof)
                 (identifier/p 'any/c)
                 (token/p 'CLOSE-PAREN)))
        (try/p (identifier/p 'sequence?))))

(define generic-map/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    (identifier/p 'function/c)
    generic-sequence/p
    generic-sequence/p
    (token/p 'CLOSE-PAREN)
    (pure 'map/c)))

(define specific-map/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'function/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [d <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a c) (equal? b d))
        (if (equal? a b)
            (pure (list 'map/c a))
            (pure (list 'map/c a b)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "map function contracts don't match sequence element type"))))))

(define map/p
  (or/p (try/p generic-map/p)
        (try/p specific-map/p)))

;; filter depends on both binary-function as well as predicate
;; while binary-constructor depends only on the former. As a result,
;; by the time we're in a position to parse a filter, it has already
;; been parsed as binary-constructor. Every filter has the signature
;; of a constructor, so we just parse at that level here instead of
;; at the level of binary-function
;; Yet, the actual parsing in the social contract macro doesn't go via
;; binary-constructor/c – this is a conscious choice since even though
;; it would work because the signatures match, it is a different idea
;; and there is no "construction" happening in a filter operation
(define generic-filter/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-constructor/c)
    (identifier/p 'predicate/c)
    generic-sequence/p
    (token/p 'CLOSE-PAREN)
    (pure 'filter/c)))

(define specific-filter/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-constructor/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'predicate/c)
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'filter/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "filter function contract doesn't match sequence element type"))))))

(define filter/p
  (or/p (try/p generic-filter/p)
        (try/p specific-filter/p)))

(define generic-reducer/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    generic-sequence/p
    (identifier/p 'any/c)
    (token/p 'CLOSE-PAREN)
    (pure 'reducer/c)))

(define specific-reducer/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'reducer/c a))
        (pure (list 'reducer/c a b)))))

(define reducer/p
  (or/p (try/p generic-reducer/p)
        (try/p specific-reducer/p)))

(define thunk/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c target)
        (pure 'thunk/c)
        (pure (list 'thunk/c target)))))

(define self-map/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (not (eq? 'any/c a)))
        (pure (list 'self-map/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

(define functional/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'self-map/c)
    (identifier/p 'procedure?)
    (token/p 'CLOSE-PAREN)
    (pure 'functional/c)))

(define free-binary-function/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'binary-function/c a b c))))

(define reducible-binary-function/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(andmap (curry eq? 'any/c)
                   (list a b c))
           (pure 'binary-function/c)]
          [(and (equal? a b)
                (eq? 'any/c c))
           (pure (list 'binary-function/c a))]
          [(equal? a b)
           (pure (list 'binary-function/c a c))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "irreducible")))])))

(define binary-function/p
  (or/p (try/p free-binary-function/p)
        (try/p reducible-binary-function/p)))

(define homogeneous-binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (cond [(eq? 'any/c a)
           (pure 'binary-predicate/c)]
          [else (pure (list 'binary-predicate/c a))])))

(define heterogeneous-binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'binary-predicate/c a b))))

(define binary-predicate/p
  (or/p (try/p homogeneous-binary-predicate/p)
        (try/p heterogeneous-binary-predicate/p)))

(define binary-composition-A/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'binary-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define binary-composition-B/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b) (equal? b c))
        (pure (list 'binary-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define binary-composition/p
  (or/p (try/p binary-composition-A/p)
        (try/p binary-composition-B/p)))

(define binary-constructor/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (equal? a b) (equal? b c))
           (fail/p (message (srcloc #f #f #f #f #f)
                            b
                            (list "contracts cannot all be the same")))]
          [(equal? a c)
           (pure (list 'binary-constructor/c '#:order ''bab
                       b a))]
          [(equal? b c)
           (pure (list 'binary-constructor/c a b))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "output contract does not match unrepeated input contract")))])))

(define variadic-constructor-abb/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (not (equal? a b))
                (equal? b c))
           (pure (list 'variadic-constructor/c a b))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "input contracts match or output contract does not match unrepeated input contract")))])))

(define variadic-constructor-bab/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (not (equal? a b))
                (equal? a c))
           (pure (list 'variadic-constructor/c '#:order ''bab
                       b a))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "input contracts match or output contract does not match an input contract")))])))

(define variadic-constructor/p
  (or/p (try/p variadic-constructor-abb/p)
        (try/p variadic-constructor-bab/p)))

(define variadic-binary-tail/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-function/c a (list 'tail b) c))))

(define variadic-binary/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-function/c a b c))))

(define variadic-simple/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(andmap (curry eq? 'any/c)
                   (list a b))
           (pure 'variadic-function/c)]
          [(eq? 'any/c b)
           (pure (list 'variadic-function/c a))]
          [else (pure (list 'variadic-function/c a b))])))

(define variadic-function/p
  (or/p (try/p variadic-binary-tail/p)
        (try/p variadic-binary/p)
        (try/p variadic-simple/p)))

(define variadic-binary-predicate-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c a (list 'tail b)))))

(define variadic-binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    [b <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c a b))))

(define variadic-simple-predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (cond [(eq? 'any/c a)
           (pure 'variadic-predicate/c)]
          [else (pure (list 'variadic-predicate/c a))])))

(define variadic-predicate/p
  (or/p (try/p variadic-binary-predicate-tail/p)
        (try/p variadic-binary-predicate/p)
        (try/p variadic-simple-predicate/p)))

(define variadic-binary-composition-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (equal? b c)
             (not (eq? 'any/c a)))
        (pure (list 'variadic-composition/c a a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define variadic-binary-composition/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (equal? b c)
             (not (eq? 'any/c a)))
        (pure (list 'variadic-composition/c a a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define variadic-simple-composition/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'variadic-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define variadic-composition/p
  (or/p (try/p variadic-binary-composition-tail/p)
        (try/p variadic-binary-composition/p)
        (try/p variadic-simple-composition/p)))

(define named-contract-specification/p
  ;; for e.g. (values integer? integer?)
  ;; and even social contracts like (function/c integer? integer?)
  (do (token/p 'OPEN-PAREN)
      [name <- (identifier/p)]
    [ctcs <- (many*/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (pure (list* name ctcs))))

(define basic-contract/p
  (or/p (try/p (literal/p))
        (try/p (identifier/p))
        (try/p function/p)
        (try/p thunk/p)
        (try/p self-map/p)
        (try/p binary-function/p)
        (try/p variadic-function/p)
        (try/p predicate/p)
        (try/p binary-predicate/p)
        (try/p variadic-predicate/p)
        (try/p reducer/p)
        (try/p encoder/p)
        (try/p decoder/p)
        (try/p hash-function/p)
        (try/p maybe/p)
        (try/p binary-composition/p)
        (try/p variadic-composition/p)
        (try/p classifier/p)
        (try/p map/p)
        (try/p filter/p)
        (try/p functional/p)
        (try/p binary-constructor/p)
        (try/p variadic-constructor/p)
        (try/p named-contract-specification/p)))

(define star-contract/p
  (do (token/p 'OPEN-PAREN)
      [arrowstar <- (identifier/p '->*)]
    (token/p 'OPEN-PAREN)
    [required <- (many*/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    [optional <- (many*/p contract/p)]
    (token/p 'CLOSE-PAREN)
    [output <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list arrowstar required optional output))))

(define contract/p
  (or/p (try/p basic-contract/p)
        (try/p star-contract/p)))

(define (lex-contract ctc)
  (lex ctc contract-lexer))

(define contracted-function/p
  (do (token/p 'OPEN-PAREN)
      [id <- (identifier/p)]
    [ctc <- (or/p (try/p contract/p)
                  symex/p)]
    (token/p 'CLOSE-PAREN)
    (pure (list id ctc))))

(define contract-out/p
  (do (token/p 'OPEN-PAREN)
      [form-name <- (identifier/p 'contract-out)]
    [a <- (many*/p contracted-function/p)]
    (token/p 'CLOSE-PAREN)
    (pure (list* form-name a))))

(define form/p
  (do (token/p 'OPEN-PAREN)
      [a <- (many*/p symex/p)]
    (token/p 'CLOSE-PAREN)
    (pure a)))

(define symex/p
  (or/p (try/p (literal/p))
        (try/p (identifier/p))
        (try/p form/p)))

(define provide/p
  (do (token/p 'OPEN-PAREN)
      [form-name <- (identifier/p 'provide)]
    [as <- (many*/p
            (or/p (try/p contract-out/p)
                  symex/p))]
    (token/p 'CLOSE-PAREN)
    (pure (list* form-name as))))

;; this represents a single "pass" of the compiler
(define/contract (compile-pass src parser)
  (-> string? parser? any/c)
  (parse-result!
   (parse-tokens parser
                 (lex-contract src))))

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

(define (~translate src parser)
  (read
   (open-input-string
    (upcompile src
               parser))))

(define-syntax-parse-rule (translate-provide pf)
  (~translate (~a 'pf) provide/p))

(define-syntax-parse-rule (translate ctc)
  (~translate (~a 'ctc) contract/p))

(module+ test
  (define c3po-tests
    (test-suite
     "c3po tests"

     (test-suite
      "identifier"
      (check-equal? (translate abc) 'abc)
      (check-equal? (translate function/c) 'function/c))

     (test-suite
      "function/c"
      (check-equal? (translate (-> a b)) '(function/c a b))
      (check-equal? (translate (-> any/c any/c)) 'function/c))

     (test-suite
      "thunk/c"
      (check-equal? (translate (-> a)) '(thunk/c a))
      (check-equal? (translate (-> any/c)) 'thunk/c))

     (test-suite
      "binary-function/c"
      (check-equal? (translate (-> a b c)) '(binary-function/c a b c))
      (check-equal? (translate (-> a a b)) '(binary-function/c a b))
      (check-equal? (translate (-> a a any/c)) '(binary-function/c a))
      (check-equal? (translate (-> any/c any/c any/c)) 'binary-function/c))

     (test-suite
      "variadic-function/c"
      (check-equal? (translate (-> a ... b c)) '(variadic-function/c a (tail b) c))
      (check-equal? (translate (-> a b ... c)) '(variadic-function/c a b c))
      (check-equal? (translate (-> a ... b)) '(variadic-function/c a b))
      (check-equal? (translate (-> a ... any/c)) '(variadic-function/c a))
      (check-equal? (translate (-> any/c ... any/c)) 'variadic-function/c))

     (test-suite
      "maybe/c"
      (check-equal? (translate (or/c number? void?)) '(maybe/c number? void?))
      (check-equal? (translate (or/c number? #f)) '(maybe/c number?)))

     (test-suite
      "self-map/c"
      (check-equal? (translate (-> a a)) '(self-map/c a)))

     (test-suite
      "functional/c"
      (check-equal? (translate (-> procedure? procedure?)) 'functional/c))

     (test-suite
      "predicate/c"
      (check-equal? (translate (-> a boolean?)) '(predicate/c a))
      (check-equal? (translate (-> any/c boolean?)) 'predicate/c))

     (test-suite
      "binary-predicate/c"
      (check-equal? (translate (-> a b boolean?)) '(binary-predicate/c a b))
      (check-equal? (translate (-> a a boolean?)) '(binary-predicate/c a))
      (check-equal? (translate (-> any/c any/c boolean?)) 'binary-predicate/c))

     (test-suite
      "variadic-predicate/c"
      (check-equal? (translate (-> a ... b boolean?)) '(variadic-predicate/c a (tail b)))
      (check-equal? (translate (-> a b ... boolean?)) '(variadic-predicate/c a b))
      (check-equal? (translate (-> a ... boolean?)) '(variadic-predicate/c a))
      (check-equal? (translate (-> any/c ... boolean?)) 'variadic-predicate/c))

     (test-suite
      "encoder/c"
      (check-equal? (translate (-> any/c number?)) '(encoder/c number?))
      (check-equal? (translate (-> any/c string?)) '(encoder/c string?)))

     (test-suite
      "decoder/c"
      (check-equal? (translate (-> number? any/c)) '(decoder/c number?))
      (check-equal? (translate (-> string? any/c)) '(decoder/c string?)))

     (test-suite
      "hash-function/c"
      (check-equal? (translate (-> any/c fixnum?)) 'hash-function/c))

     (test-suite
      "binary-composition/c"
      (check-equal? (translate (-> a a a)) '(binary-composition/c a)))

     (test-suite
      "variadic-composition/c"
      (check-equal? (translate (-> a ... a)) '(variadic-composition/c a))
      (check-equal? (translate (-> a a ... a)) '(variadic-composition/c a a))
      (check-equal? (translate (-> a ... a a)) '(variadic-composition/c a a)))

     (test-suite
      "binary-constructor/c"
      (check-equal? (translate (-> a b b)) '(binary-constructor/c a b))
      (check-equal? (translate (-> b a b)) '(binary-constructor/c #:order 'bab a b)))

     (test-suite
      "variadic-constructor/c"
      (check-equal? (translate (-> a ... b b)) '(variadic-constructor/c a b))
      (check-equal? (translate (-> b a ... b)) '(variadic-constructor/c #:order 'bab a b)))

     (test-suite
      "classifier/c"
      (check-equal? (translate (-> (-> any/c number?) sequence? (sequenceof sequence?))) '(classifier/c number?))
      (check-equal? (translate (-> (-> any/c string?) sequence? (sequenceof sequence?))) '(classifier/c string?)))

     (test-suite
      "map/c"
      (check-equal? (translate (-> (-> number? string?) (sequenceof number?) (sequenceof string?))) '(map/c number? string?))
      (check-equal? (translate (-> (-> any/c any/c) (sequenceof any/c) (sequenceof any/c))) 'map/c))

     (test-suite
      "filter/c"
      (check-equal? (translate (-> (-> number? boolean?) (sequenceof number?) (sequenceof number?))) '(filter/c number?))
      (check-equal? (translate (-> (-> any/c boolean?) (sequenceof any/c) (sequenceof any/c))) 'filter/c))

     (test-suite
      "reducer/c"
      (check-equal? (translate (-> (sequenceof number?) string?)) '(reducer/c number? string?))
      (check-equal? (translate (-> (sequenceof number?) number?)) '(reducer/c number?))
      ;; TODO: could also parse sequence? listof and list? here
      ;; and similarly for all sequence-related contracts
      (check-equal? (translate (-> (sequenceof any/c) any/c)) 'reducer/c))

     (test-suite
      "composition"
      (check-equal? (translate (-> (-> a b) (-> a)))
                    '(function/c (function/c a b) (thunk/c a)))
      (check-equal? (translate (-> (-> a b) (-> a b)))
                    '(self-map/c (function/c a b)))
      (check-equal? (translate (-> any/c (-> a a)))
                    '(encoder/c (self-map/c a))))

     (test-suite
      "pass-through"
      ;; the compiler should have "pass through" behavior for
      ;; unrecognized contract specifications, and also contract
      ;; combinators like (values ...)
      (check-equal? (translate (values integer? integer?))
                    '(values integer? integer?))
      (check-equal? (translate (blah 1 2))
                    '(blah 1 2)))

     (test-suite
      "simplify social contracts"
      (check-equal? (translate (function/c number? number?))
                    '(self-map/c number?))
      (check-equal? (translate (function/c any/c number?))
                    '(encoder/c number?))
      (check-equal? (translate (function/c number? any/c))
                    '(decoder/c number?))
      (check-equal? (translate (function/c any/c fixnum?))
                    'hash-function/c)
      (check-equal? (translate (function/c number? boolean?))
                    '(predicate/c number?))
      (check-equal? (translate (function/c any/c boolean?))
                    'predicate/c)
      (check-equal? (translate (function/c procedure? procedure?))
                    'functional/c)
      (check-equal? (translate (self-map/c procedure?))
                    'functional/c)
      (check-equal? (translate (binary-function/c number? number? number?))
                    '(binary-composition/c number?))
      (check-equal? (translate (binary-function/c number? number? boolean?))
                    '(binary-predicate/c number?))
      (check-equal? (translate (binary-function/c number? list? list?))
                    '(binary-constructor/c number? list?))
      (check-equal? (translate (binary-function/c list? number? list?))
                    '(binary-constructor/c #:order 'bab number? list?))
      (check-equal? (translate (variadic-function/c number? number?))
                    '(variadic-composition/c number?))
      (check-equal? (translate (variadic-function/c number? boolean?))
                    '(variadic-predicate/c number?))
      (check-equal? (translate (variadic-function/c number? (tail list?) list?))
                    '(variadic-constructor/c number? list?))
      (check-equal? (translate (variadic-function/c list? number? list?))
                    '(variadic-constructor/c #:order 'bab number? list?))
      (check-equal? (translate (maybe/c number? #f))
                    '(maybe/c number?)))))

  (void (run-tests c3po-tests)))
