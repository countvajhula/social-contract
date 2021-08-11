#lang racket

(require megaparsack
         megaparsack/text
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         parser-tools/lex
         racket/function
         (prefix-in : parser-tools/lex-sre)
         racket/format)

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
  (OPEN-PAREN CLOSE-PAREN ARROW ELLIPSIS))

(define contract-lexer
  (lexer-src-pos [(:: (:+ (:or (:/ #\a #\z) (:/ #\A #\Z)))
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z) #\/ #\- #\?))) ; TODO: ensure coverage
                  (token-IDENTIFIER (string->symbol lexeme))]
                 [(:: (:+ (:or (:/ #\0 #\9) #\# #\"))
                      (:* (:or (:/ #\a #\z) (:/ #\A #\Z))))
                  (token-LITERAL (read (open-input-string lexeme)))]
                 [(eof) eof] ; to e.g. use with apply-lexer from brag/support
                 ["(" (token-OPEN-PAREN)]
                 [")" (token-CLOSE-PAREN)]
                 ["->" (token-ARROW)]
                 ["..." (token-ELLIPSIS)]
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

(define maybe/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'or/c)
    [a <- contract/p]
    [default <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (eq? #f default)
        (pure (list 'maybe/c a))
        (pure (list 'maybe/c a default)))))

(define function/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a 'any/c)
             (eq? b 'any/c))
        (pure 'function/c)
        (pure (list 'function/c a b)))))

(define predicate/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c a)
        (pure 'predicate/c)
        (pure (list 'predicate/c a)))))

(define encoder/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
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
      (token/p 'ARROW)
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
      (token/p 'ARROW)
    (identifier/p 'any/c)
    (identifier/p 'fixnum?)
    (token/p 'CLOSE-PAREN)
    (pure 'hash-function/c)))

(define classifier/p
  ;; could probably leverage phrase structure in contract
  ;; parsers here, e.g. reuse encoder/c. Also, this is
  ;; very specific atm, expecting sequenceof and sequence?
  ;; but should ideally accept e.g. listof and list?
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    (token/p 'OPEN-PAREN)
    (token/p 'ARROW)
    (identifier/p 'any/c)
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    (identifier/p 'sequence?)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    (identifier/p 'sequence?)
    (token/p 'CLOSE-PAREN)
    (if (eq? 'any/c a)
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "can't encode to any/c")))
        (pure (list 'classifier/c a)))))

(define map/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    (token/p 'OPEN-PAREN)
    (token/p 'ARROW)
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
    (if (and (eq? a c) (eq? b d))
        (if (and (eq? a b) (eq? a 'any/c))
            (pure 'map/c)
            (pure (list 'map/c a b)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "map function contracts don't match sequence element type"))))))

(define filter/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    (token/p 'OPEN-PAREN)
    (token/p 'ARROW)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a b) (eq? b c))
        (if (eq? a 'any/c)
            (pure 'filter/c)
            (pure (list 'filter/c a)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "filter function contract doesn't match sequence element type"))))))

(define reducer/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    (token/p 'OPEN-PAREN)
    (identifier/p 'sequenceof)
    [a <- contract/p]
    (token/p 'CLOSE-PAREN)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (eq? a b) (eq? a 'any/c))
           (pure 'reducer/c)]
          [(and (eq? a b)) (pure (list 'reducer/c a))]
          [else (pure (list 'reducer/c a b))])))

(define thunk/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (eq? target 'any/c)
        (pure 'thunk/c)
        (pure (list 'thunk/c target)))))

(define self-map/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (not (eq? a 'any/c)))
        (pure (list 'self-map/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

(define functional/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    (identifier/p 'procedure?)
    (identifier/p 'procedure?)
    (token/p 'CLOSE-PAREN)
    (pure 'functional/c)))

(define binary-function/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(andmap (curry eq? 'any/c)
                   (list a b c))
           (pure 'binary-function/c)]
          [(and (eq? a b)
                (eq? 'any/c c))
           (pure (list 'binary-function/c a))]
          [(eq? a b)
           (pure (list 'binary-function/c a c))]
          [else (pure (list 'binary-function/c a b c))])))

(define binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (cond [(andmap (curry eq? 'any/c)
                   (list a b))
           (pure 'binary-predicate/c)]
          [(eq? a b)
           (pure (list 'binary-predicate/c a))]
          [else (pure (list 'binary-predicate/c a b))])))

(define binary-composition/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a b)
             (eq? b c)
             (not (eq? 'any/c a))) ; don't accept this as composition
        (pure (list 'binary-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

(define binary-constructor/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(eq? a b)
           (fail/p (message (srcloc #f #f #f #f #f)
                            b
                            (list "identical contracts - use binary-function or binary-composition instead")))]
          [(eq? a c)
           (pure (list 'binary-constructor/c '#:order ''bab
                       b a))]
          [(eq? b c)
           (pure (list 'binary-constructor/c a b))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "output contract does not match unrepeated input contract")))])))

(define variadic-constructor-abb/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (not (eq? a b))
                (eq? b c))
           (pure (list 'variadic-constructor/c a b))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "input contracts match or output contract does not match unrepeated input contract")))])))

(define variadic-constructor-bab/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(and (not (eq? a b))
                (eq? a c))
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
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-function/c a (list 'tail b) c))))

(define variadic-binary/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-function/c a b c))))

(define variadic-simple/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
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
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c a (list 'tail b)))))

(define variadic-binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c a b))))

(define variadic-simple-predicate/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
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
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a b)
             (eq? b c)
             (not (eq? 'any/c a)))
        (pure (list 'variadic-composition/c a a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

(define variadic-binary-composition/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a b)
             (eq? b c)
             (not (eq? 'any/c a)))
        (pure (list 'variadic-composition/c a a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

(define variadic-simple-composition/p
  (do (token/p 'OPEN-PAREN)
      (token/p 'ARROW)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (eq? a b)
             (not (eq? 'any/c a)))
        (pure (list 'variadic-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "identical contracts"))))))

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
    (pure (list* name ctcs))))

(define contract/p
  (or/p (try/p (literal/p))
        (try/p (identifier/p))
        (try/p classifier/p)
        (try/p map/p)
        (try/p filter/p)
        (try/p reducer/p)
        (try/p maybe/p)
        (try/p functional/p)
        (try/p self-map/p)
        (try/p thunk/p)
        (try/p predicate/p)
        (try/p binary-predicate/p)
        (try/p variadic-predicate/p)
        (try/p binary-composition/p)
        (try/p variadic-composition/p)
        (try/p binary-constructor/p)
        (try/p variadic-constructor/p)
        (try/p hash-function/p)
        (try/p encoder/p)
        (try/p decoder/p)
        (try/p function/p)
        (try/p binary-function/p)
        (try/p variadic-function/p)
        (try/p named-contract-specification/p)))

(define (translate ctc)
  (parse-result!
   (parse-tokens contract/p
                 (lex (~a ctc) contract-lexer))))

;; TODO: a function translate-provide that accepts a
;; provide spec and translates the contract-out subform
;; also, maybe both this as well as translate should be
;; macros to avoid issues with quoting

;; TODO?: also parse non-minimal specifications like
;; (binary-function/c number? number? number?)
;; so that they lint to (binary-composition/c number?)
;; just a few one-off rules here may be fine

;; TODO: what is this weirdness:
;; (translate '(-> (function/c number? string?) (sequenceof string?) (sequenceof number?)))
;; => '(thunk/c (function/c number? string?))
;; instead of (binary-function/c (function/c number? string?) (sequenceof string?) (sequenceof number?))
;; and this:
;; (translate '(-> (-> number? string?) (sequenceof string?) (sequenceof number?)))
;; => '(function/c (function/c number? string?) (sequenceof string?))
;; instead of (binary-function/c (function/c number? string?) (sequenceof string?) (sequenceof number?))

(module+ test
  (define c3po-tests
    (test-suite
     "c3po tests"

     (test-suite
      "identifier"
      (check-equal? (translate 'abc) 'abc)
      (check-equal? (translate 'function/c) 'function/c))

     (test-suite
      "function/c"
      (check-equal? (translate '(-> a b)) '(function/c a b))
      (check-equal? (translate '(-> any/c any/c)) 'function/c))

     (test-suite
      "thunk/c"
      (check-equal? (translate '(-> a)) '(thunk/c a))
      (check-equal? (translate '(-> any/c)) 'thunk/c))

     (test-suite
      "self-map/c"
      (check-equal? (translate '(-> a a)) '(self-map/c a)))

     (test-suite
      "functional/c"
      (check-equal? (translate '(-> procedure? procedure?)) 'functional/c))

     (test-suite
      "binary-function/c"
      (check-equal? (translate '(-> a b c)) '(binary-function/c a b c))
      (check-equal? (translate '(-> a a b)) '(binary-function/c a b))
      (check-equal? (translate '(-> a a any/c)) '(binary-function/c a))
      (check-equal? (translate '(-> any/c any/c any/c)) 'binary-function/c))

     (test-suite
      "variadic-function/c"
      (check-equal? (translate '(-> a ... b c)) '(variadic-function/c a (tail b) c))
      (check-equal? (translate '(-> a b ... c)) '(variadic-function/c a b c))
      (check-equal? (translate '(-> a ... b)) '(variadic-function/c a b))
      (check-equal? (translate '(-> a ... any/c)) '(variadic-function/c a))
      (check-equal? (translate '(-> any/c ... any/c)) 'variadic-function/c))

     (test-suite
      "predicate/c"
      (check-equal? (translate '(-> a boolean?)) '(predicate/c a))
      (check-equal? (translate '(-> any/c boolean?)) 'predicate/c))

     (test-suite
      "binary-predicate/c"
      (check-equal? (translate '(-> a b boolean?)) '(binary-predicate/c a b))
      (check-equal? (translate '(-> a a boolean?)) '(binary-predicate/c a))
      (check-equal? (translate '(-> any/c any/c boolean?)) 'binary-predicate/c))

     (test-suite
      "variadic-predicate/c"
      (check-equal? (translate '(-> a ... b boolean?)) '(variadic-predicate/c a (tail b)))
      (check-equal? (translate '(-> a b ... boolean?)) '(variadic-predicate/c a b))
      (check-equal? (translate '(-> a ... boolean?)) '(variadic-predicate/c a))
      (check-equal? (translate '(-> any/c ... boolean?)) 'variadic-predicate/c))

     (test-suite
      "encoder/c"
      (check-equal? (translate '(-> any/c number?)) '(encoder/c number?))
      (check-equal? (translate '(-> any/c string?)) '(encoder/c string?)))

     (test-suite
      "decoder/c"
      (check-equal? (translate '(-> number? any/c)) '(decoder/c number?))
      (check-equal? (translate '(-> string? any/c)) '(decoder/c string?)))

     (test-suite
      "hash-function/c"
      (check-equal? (translate '(-> any/c fixnum?)) 'hash-function/c))

     (test-suite
      "maybe/c"
      (check-equal? (translate '(or/c number? void?)) '(maybe/c number? void?))
      (check-equal? (translate '(or/c number? #f)) '(maybe/c number?)))

     (test-suite
      "binary-composition/c"
      (check-equal? (translate '(-> a a a)) '(binary-composition/c a)))

     (test-suite
      "variadic-composition/c"
      (check-equal? (translate '(-> a ... a)) '(variadic-composition/c a))
      (check-equal? (translate '(-> a a ... a)) '(variadic-composition/c a a))
      (check-equal? (translate '(-> a ... a a)) '(variadic-composition/c a a)))

     (test-suite
      "binary-constructor/c"
      (check-equal? (translate '(-> a b b)) '(binary-constructor/c a b))
      (check-equal? (translate '(-> b a b)) '(binary-constructor/c #:order 'bab a b)))

     (test-suite
      "variadic-constructor/c"
      (check-equal? (translate '(-> a ... b b)) '(variadic-constructor/c a b))
      (check-equal? (translate '(-> b a ... b)) '(variadic-constructor/c #:order 'bab a b)))

     (test-suite
      "classifier/c"
      (check-equal? (translate '(-> (-> any/c number?) sequence? (sequenceof sequence?))) '(classifier/c number?))
      (check-equal? (translate '(-> (-> any/c string?) sequence? (sequenceof sequence?))) '(classifier/c string?)))

     (test-suite
      "map/c"
      (check-equal? (translate '(-> (-> number? string?) (sequenceof number?) (sequenceof string?))) '(map/c number? string?))
      (check-equal? (translate '(-> (-> any/c any/c) (sequenceof any/c) (sequenceof any/c))) 'map/c))

     (test-suite
      "filter/c"
      (check-equal? (translate '(-> (-> number? boolean?) (sequenceof number?) (sequenceof number?))) '(filter/c number?))
      (check-equal? (translate '(-> (-> any/c boolean?) (sequenceof any/c) (sequenceof any/c))) 'filter/c))

     (test-suite
      "reducer/c"
      (check-equal? (translate '(-> (sequenceof number?) string?)) '(reducer/c number? string?))
      (check-equal? (translate '(-> (sequenceof number?) number?)) '(reducer/c number?))
      ;; TODO: could also parse sequence? listof and list? here
      ;; and similarly for all sequence-related contracts
      (check-equal? (translate '(-> (sequenceof any/c) any/c)) 'reducer/c))

     (test-suite
      "composition"
      (check-equal? (translate '(-> (-> a b) (-> a)))
                    '(function/c (function/c a b) (thunk/c a)))
      (check-equal? (translate '(-> (-> a b) (-> a b)))
                    '(self-map/c (function/c a b)))
      (check-equal? (translate '(-> any/c (-> a a)))
                    '(encoder/c (self-map/c a))))
     (test-suite
      "miscellaneous"
      (check-equal? (translate '(values integer? integer?))
                    '(values integer? integer?)))))

  (void (run-tests c3po-tests)))
