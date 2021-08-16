#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         racket/function
         racket/match)

(require "base-parsers.rkt")

(provide contract/p)

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

;; note that we don't need to handle infix arrow contracts separately
;; because they get converted to prefix arrow form at the reader
;; level prior to evaluation
(define (arrow-contract/p n)
  ;; n is the number of inputs -- the input arity
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [doms <- (repeat/p n contract/p)]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (append doms (list target)))))

(define (variadic-arrow-contract/p n-pre n-post)
  ;; n-pre is the number of non-variadic inputs preceding, i.e. not including,
  ;; the variadic argument. n-post is the number of non-variadic inputs
  ;; following (and not including) the variadic argument
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [pre-doms <- (repeat/p n-pre contract/p)]
    [var-dom <- contract/p]
    (token/p 'ELLIPSIS)
    [post-doms <- (repeat/p n-post contract/p)]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (append pre-doms (list var-dom) post-doms (list target)))))

(define function/p
  (do [sig <- (arrow-contract/p 1)]
      (match-let ([(list a b) sig])
        (if (and (eq? 'any/c a)
                 (eq? 'any/c b))
            (pure 'function/c)
            (pure (list 'function/c a b))))))

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
    sequence/p
    (parametric-sequence/p sequence/p)
    (pure (list 'classifier/c a))))

(define sequence/p
  (or/p (try/p (identifier/p 'sequence?))
        (try/p (identifier/p 'list?))
        (try/p (identifier/p 'vector?))))

(define parametric-sequence-identifier/p
  (or/p (try/p (identifier/p 'sequenceof))
        (try/p (identifier/p 'listof))
        (try/p (identifier/p 'vectorof))))

(define (parametric-sequence/p [ctc/p contract/p])
  (do (token/p 'OPEN-PAREN)
      parametric-sequence-identifier/p
    [a <- ctc/p]
    (token/p 'CLOSE-PAREN)
    (pure (list 'sequenceof a)))) ; all parametric sequences get converted to sequenceof, for now

(define generic-sequence/p
  (or/p (try/p (parametric-sequence/p (identifier/p 'any/c)))
        (try/p sequence/p)))

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
    [c <- (parametric-sequence/p)]
    [d <- (parametric-sequence/p)]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a (second c)) (equal? b (second d)))
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
    [b <- (parametric-sequence/p)]
    (token/p 'CLOSE-PAREN)
    (if (equal? a (second b))
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
    [a <- (parametric-sequence/p)]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? (second a) b)
        (pure (list 'reducer/c b))
        (pure (list 'reducer/c (second a) b)))))

(define reducer/p
  (or/p (try/p generic-reducer/p)
        (try/p specific-reducer/p)))

(define thunk/p
  (do [sig <- (arrow-contract/p 0)]
      (match-let ([(list target) sig])
        (if (eq? 'any/c target)
            (pure 'thunk/c)
            (pure (list 'thunk/c target))))))

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
  (do [sig <- (arrow-contract/p 2)]
      (match-let ([(list a b c) sig])
        (pure (list 'binary-function/c a b c)))))

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
  (do [sig <- (variadic-arrow-contract/p 0 1)]
      (match-let ([(list a b c) sig])
        (pure (list 'variadic-function/c a (list 'tail b) c)))))

(define variadic-binary/p
  (do [sig <- (variadic-arrow-contract/p 1 0)]
      (match-let ([(list a b c) sig])
        (pure (list 'variadic-function/c a b c)))))

(define variadic-simple/p
  (do [sig <- (variadic-arrow-contract/p 0 0)]
      (match-let ([(list a b) sig])
        (cond [(andmap (curry eq? 'any/c)
                       (list a b))
               (pure 'variadic-function/c)]
              [(eq? 'any/c b)
               (pure (list 'variadic-function/c a))]
              [else (pure (list 'variadic-function/c a b))]))))

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

(define star-contract-basic/p
  (do (token/p 'OPEN-PAREN)
      [arrowstar <- (identifier/p '->*)]
    (token/p 'OPEN-PAREN)
    [required <- (many*/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    [optional <- (many*/p (or/p (try/p (keyword/p)) contract/p))]
    (token/p 'CLOSE-PAREN)
    [output <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list arrowstar required optional output))))

(define star-contract-with-rest/p
  (do (token/p 'OPEN-PAREN)
      [arrowstar <- (identifier/p '->*)]
    (token/p 'OPEN-PAREN)
    [required <- (many*/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    [optional <- (many*/p (or/p (try/p (keyword/p)) contract/p))]
    (token/p 'CLOSE-PAREN)
    (keyword/p '#:rest)
    [rest-ctc <- contract/p]
    [output <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list arrowstar required optional '#:rest rest-ctc output))))

(define star-contract/p
  (or/p (try/p star-contract-basic/p)
        (try/p star-contract-with-rest/p)))

(define contract/p
  (or/p (try/p basic-contract/p)
        (try/p star-contract/p)))
