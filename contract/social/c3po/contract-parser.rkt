#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative
         racket/function
         racket/match)

(require "base-parsers.rkt"
         "private/util.rkt")

(provide contract/p)

(define arrow/p
  (identifier/p '->))

(define ellipsis/p
  (do (token/p 'ELLIPSIS)
      (pure '...)))

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

(define nonempty/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'and/c)
    [a <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'not/c)
    (identifier/p 'empty?)
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure (list 'nonempty/c a))))

;; note that we don't need to handle infix arrow contracts separately
;; because they get converted to prefix arrow form at the reader
;; level prior to evaluation
(define function/p
  (do (token/p 'OPEN-PAREN)
      arrow/p
    [doms <- (many/p
              ;; parse all contracts except the last one
              (do (try/p
                   (lookahead/p (many/p #:min 2
                                        (or/p (try/p contract/p)
                                              (try/p ellipsis/p)))))
                  (or/p (try/p contract/p)
                        (try/p ellipsis/p))))]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (= 1 (length doms))
             (eq? (first doms)
                  'any/c)
             (eq? target
                  'any/c))
        (pure 'function/c)
        (pure (append (list 'function/c) doms (list target))))))

(define (function-with-arity/p n)
  ;; n is the number of inputs -- the input arity
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [doms <- (repeat/p n contract/p)]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (append doms (list target)))))

(define variadic-function-components/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [pre-doms <- (many/p #:min 0
                         (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                             contract/p))]
    [var-dom <- contract/p]
    (token/p 'ELLIPSIS)
    [post-doms <- (many/p #:min 0
                          (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                              contract/p))]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list pre-doms var-dom post-doms target))))

(define predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [doms <- (many/p #:min 1
                     (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                         contract/p))]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (if (and (= 1 (length doms))
             (eq? 'any/c (first doms)))
        (pure 'predicate/c)
        (pure (append (list 'predicate/c) doms)))))

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

(define (parametric-contract/p p)
  (do (token/p 'OPEN-PAREN)
      [c <- contract/p]
    [d <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? p d)
        (pure (list c d))
        (fail/p (message (srcloc #f #f #f #f #f)
                         c
                         (list "parametric output contract doesn't match input"))))))

(define lift-simple/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    [b <- (parametric-contract/p a)]
    (token/p 'CLOSE-PAREN)
    (if (equal? a (second b))
        (pure (list 'lift/c a (first b)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         a
                         (list "parametric output contract doesn't match input"))))))

(define lift-binary/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- (or/p (try/p (parametric-contract/p a))
                (try/p (parametric-contract/p b)))]
    (token/p 'CLOSE-PAREN)
    (cond [(equal? a (second c))
           (pure (list 'lift/c a (first c) (list 'tail b)))]
          [(equal? b (second c))
           (pure (list 'lift/c b (first c) (list 'head a)))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                            a
                            (list "parametric output contract doesn't match input")))])))

(define lift-with-head/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [args <- (many/p #:min 1
                     (do (try/p (lookahead/p (many/p #:min 3 contract/p)))
                         contract/p))]
    [a <- contract/p]
    [b <- (parametric-contract/p a)]
    (token/p 'CLOSE-PAREN)
    (pure (list 'lift/c a (first b) (list* 'head args)))))

(define lift-with-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    [args <- (many/p #:min 1
                     (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                         contract/p))]
    [b <- (parametric-contract/p a)]
    (token/p 'CLOSE-PAREN)
    (pure (list 'lift/c a (first b) (list* 'tail args)))))

(define lift/p
  (or/p (try/p lift-simple/p)
        (try/p lift-binary/p)
        (try/p lift-with-head/p)
        (try/p lift-with-tail/p)))

(define hash-function/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'encoder/c)
    (identifier/p 'fixnum?)
    (token/p 'CLOSE-PAREN)
    (pure 'hash-function/c)))

(define classifier/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'map/c)
    [a <- (identifier/p 'any/c)]
    [b <- generic-sequence/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    (token/p 'OPEN-PAREN)
    (identifier/p 'encoder/c) ; note: does not recognize classifying by any/c
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure (list 'classifier/c c))))

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
  (do (or/p (try/p (parametric-sequence/p (identifier/p 'any/c)))
            (try/p sequence/p))
      (pure (list 'sequenceof 'any/c))))

;; (define any-sequence/p
;;   (or/p (try/p generic-sequence/p)
;;         (try/p (parametric-sequence/p))))

(define map-simple/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    [b <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (eq? 'any/c (second a)))
        (pure 'map/c)
        (pure (list 'map/c (second a) (second b))))))

(define map-with-head/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [args <- (many/p #:min 1
                     (do (try/p (lookahead/p (many/p #:min 3 contract/p)))
                         contract/p))]
    [a <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    [b <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (if (eq? 'any/c (second a))
            (pure (list 'map/c (list* 'head args)))
            (pure (list 'map/c (second a) (list* 'head args))))
        (pure (list 'map/c (second a) (second b) (list* 'head args))))))

(define map-with-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    [args <- (many/p #:min 1
                     (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                         contract/p))]
    [b <- (or/p (try/p generic-sequence/p)
                (try/p (parametric-sequence/p)))]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (if (eq? 'any/c (second a))
            (pure (list 'map/c (list* 'tail args)))
            (pure (list 'map/c (second a) (list* 'tail args))))
        (pure (list 'map/c (second a) (second b) (list* 'tail args))))))

(define map/p
  (or/p (try/p map-simple/p)
        (try/p map-with-head/p)
        (try/p map-with-tail/p)))

(define generic-mapper/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'map/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    (identifier/p 'function/c)
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure 'mapper/c)))

(define specific-mapper/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'map/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    (token/p 'OPEN-PAREN)
    (identifier/p 'function/c)
    [c <- contract/p]
    [d <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a c)
             (equal? b d))
        (pure (list 'mapper/c a b))
        (fail/p (message (srcloc #f #f #f #f #f)
                         (list a b c d)
                         (list "map function contracts don't match sequence element type"))))))

(define mapper/p
  (or/p (try/p generic-mapper/p)
        (try/p specific-mapper/p)))

(define generic-filter/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'map/c)
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    (identifier/p 'predicate/c)
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure 'filter/c)))

(define specific-filter/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'map/c)
    [a <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    (token/p 'OPEN-PAREN)
    (identifier/p 'predicate/c)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
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
  (do [sig <- (function-with-arity/p 0)]
      (match-let ([(list target) sig])
        (if (eq? 'any/c target)
            (pure 'thunk/c)
            (pure (list 'thunk/c target))))))

(define self-map-simple/p
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

(define parametrized-self-map-general/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [a <- contract/p]
    [doms <- (many/p #:min 0
                     (do (try/p (lookahead/p (many/p #:min 3 contract/p)))
                         contract/p))]
    [b <- contract/p]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (cond [(all-equal? (append (list a b target) doms))
           (fail/p (message (srcloc #f #f #f #f #f)
                            b
                            (list "contracts cannot all be the same")))]
          [(and (equal? b target)
                (not (eq? 'any/c b)))
           (pure (list 'self-map/c target (list* 'head a doms)))]
          [(and (equal? a target)
                (not (eq? 'any/c a)))
           (pure (list 'self-map/c target (list* 'tail (append doms (list b)))))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 target
                                 (list "output contract does not match first or last input contract, or matching contract is any/c which is not acceptable")))])))

(define parametrized-self-map-binary/p
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
          [(and (equal? a c)
                (not (eq? 'any/c a)))
           (pure (list 'self-map/c a (list 'tail b)))]
          [(and (equal? b c)
                (not (eq? 'any/c a)))
           (pure (list 'self-map/c b (list 'head a)))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 c
                                 (list "output contract does not match unrepeated input contract, or matching contract is any/c which is not acceptable")))])))

(define self-map/p
  (or/p (try/p self-map-simple/p)
        (try/p parametrized-self-map-general/p)
        (try/p parametrized-self-map-binary/p)))

(define functional/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'self-map/c)
    (identifier/p 'procedure?)
    (token/p 'CLOSE-PAREN)
    (pure 'functional/c)))

(define free-binary-function/p
  (do [sig <- (function-with-arity/p 2)]
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
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "irreducible")))])))

(define binary-function/p
  (or/p (try/p free-binary-function/p)
        (try/p reducible-binary-function/p)))

;; we probably should not allow binary operations to accept
;; any/c, so that this path shouldn't be hit
(define homogeneous-binary-predicate/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-operation/c)
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
    (if (and (equal? a b) ; should only happen on any/c
             (eq? 'any/c a))
        (pure 'binary-predicate/c)
        (pure (list 'binary-predicate/c a b)))))

(define binary-predicate/p
  (or/p (try/p homogeneous-binary-predicate/p)
        (try/p heterogeneous-binary-predicate/p)))

(define binary-operation-a/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-function/c)
    [a <- contract/p]
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (equal? a b)
             (not (eq? 'any/c a)))
        (if (eq? 'any/c c)
            (pure (list 'binary-operation/c a))
            (pure (list 'binary-operation/c a c)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "input contracts are not identical"))))))

(define binary-operation-b/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'operation/c)
    (number/p 2)
    [ctcs <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (pure (append (list 'binary-operation/c) ctcs))))

(define binary-operation/p
  (or/p (try/p binary-operation-a/p)
        (try/p binary-operation-b/p)))

(define binary-composition-typical/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-operation/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'binary-composition/c a))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define binary-composition-with-head/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-operation/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'binary-composition/c a (list* 'head args)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define binary-composition-with-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-operation/c)
    [a <- contract/p]
    [b <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (equal? a b)
        (pure (list 'binary-composition/c a (list* 'tail args)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define binary-composition-predicates/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'binary-predicate/c)
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (pure (list 'binary-composition/c 'boolean?))))

(define binary-composition/p
  (or/p (try/p binary-composition-typical/p)
        (try/p binary-composition-with-head/p)
        (try/p binary-composition-with-tail/p)
        (try/p binary-composition-predicates/p)))

(define operation/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'function/c)
    [doms <- (many/p #:min 2
                     (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                         contract/p))]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (and (all-equal? doms)
             (eq? 'any/c (first doms)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         doms
                         (list "any/c cannot be treated as homogeneous")))
        (cond [(all-equal? doms)
               (if (eq? 'any/c target)
                   (pure (list 'operation/c (length doms) (first doms)))
                   (pure (list 'operation/c (length doms) (first doms) target)))]
              [(let ([run-length (leading-run-length doms)])
                 (and (> run-length 1) run-length))
               =>
               (λ (run-length)
                 (if (eq? 'any/c target)
                     (pure (list 'operation/c
                                 run-length
                                 (first doms)
                                 (list* 'tail (drop doms run-length))))
                     (pure (list 'operation/c
                                 run-length
                                 (first doms)
                                 target
                                 (list* 'tail (drop doms run-length))))))]
              [(let ([run-length (leading-run-length (reverse doms))])
                 (and (> run-length 1) run-length))
               =>
               (λ (run-length)
                 (if (eq? 'any/c target)
                     (pure (list 'operation/c
                                 run-length
                                 (last doms)
                                 (list* 'head (take doms (- (length doms) run-length)))))
                     (pure (list 'operation/c
                                 run-length
                                 (last doms)
                                 target
                                 (list* 'head (take doms (- (length doms) run-length)))))))]
              [else (fail/p (message (srcloc #f #f #f #f #f)
                                     doms
                                     (list "neither leading nor trailing input contracts are identical")))]))))

(define composition-simple/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'operation/c)
    [n <- (number/p)]
    [source <- contract/p]
    [target <- contract/p]
    (token/p 'CLOSE-PAREN)
    (if (equal? source target)
        (pure (list 'composition/c n source))
        (fail/p (message (srcloc #f #f #f #f #f)
                         (list source target)
                         (list "input and output contracts are not identical"))))))

(define composition-with-head/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'operation/c)
    [n <- (number/p)]
    [source <- contract/p]
    [target <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (equal? source target)
        (pure (list 'composition/c n source (list* 'head args)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         (list source target)
                         (list "input and output contracts are not identical"))))))

(define composition-with-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'operation/c)
    [n <- (number/p)]
    [source <- contract/p]
    [target <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (equal? source target)
        (pure (list 'composition/c n source (list* 'tail args)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         (list source target)
                         (list "input and output contracts are not identical"))))))

(define composition/p
  (or/p (try/p composition-simple/p)
        (try/p composition-with-head/p)
        (try/p composition-with-tail/p)))

(define variadic-constructor-abb/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    [c <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
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
    [c <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    [b <- contract/p]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (cond [(and (not (equal? a b))
                (equal? b c))
           (pure (list 'variadic-constructor/c '#:order ''bab
                       a b))]
          [else (fail/p (message (srcloc #f #f #f #f #f)
                                 b
                                 (list "input contracts match or output contract does not match an input contract")))])))

(define variadic-constructor/p
  (or/p (try/p variadic-constructor-abb/p)
        (try/p variadic-constructor-bab/p)))

(define variadic-function/p
  (do [sig <- variadic-function-components/p]
      (match-let ([(list pre var post target) sig])
        (cond [(and (not (null? pre)) (null? post))
               (if (eq? 'any/c target)
                   (pure (list 'variadic-function/c var (list* 'head pre)))
                   (pure (list 'variadic-function/c var target (list* 'head pre))))]
              [(and (not (null? post)) (null? pre))
               (if (eq? 'any/c target)
                   (pure (list 'variadic-function/c var (list* 'tail post)))
                   (pure (list 'variadic-function/c var target (list* 'tail post))))]
              [(and (null? pre) (null? post))
               (if (eq? 'any/c target)
                   (if (eq? 'any/c var)
                       (pure 'variadic-function/c)
                       (pure (list 'variadic-function/c var)))
                   (pure (list 'variadic-function/c var target)))]
              [else (fail/p (message (srcloc #f #f #f #f #f)
                                     sig
                                     (list "variadic function has both head and tail")))]))))

(define variadic-predicate-with-tail-a/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c a (list* 'tail args)))))

(define variadic-predicate-with-tail-b/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'predicate/c)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (if (null? args)
        (pure (list 'variadic-predicate/c a))
        (pure (list 'variadic-predicate/c a (list* 'tail args))))))

(define variadic-predicate-with-head-a/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [b <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (pure (list 'variadic-predicate/c b (list* 'head args)))))

(define variadic-predicate-with-head-b/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'predicate/c)
    [args <- (many/p #:min 0
                     (do (try/p (lookahead/p (many/p #:min 2 contract/p)))
                         contract/p))]
    [b <- contract/p]
    (token/p 'ELLIPSIS)
    (token/p 'CLOSE-PAREN)
    (if (null? args)
        (pure (list 'variadic-predicate/c b))
        (pure (list 'variadic-predicate/c b (list* 'head args))))))

(define variadic-simple-predicate-a/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [a <- contract/p]
    (identifier/p 'boolean?)
    (token/p 'CLOSE-PAREN)
    (cond [(eq? 'any/c a)
           (pure 'variadic-predicate/c)]
          [else (pure (list 'variadic-predicate/c a))])))

(define variadic-simple-predicate-b/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'predicate/c)
    [a <- contract/p]
    (token/p 'ELLIPSIS)
    (token/p 'CLOSE-PAREN)
    (cond [(eq? 'any/c a)
           (pure 'variadic-predicate/c)]
          [else (pure (list 'variadic-predicate/c a))])))

(define variadic-predicate/p
  (or/p (try/p variadic-predicate-with-tail-a/p)
        (try/p variadic-predicate-with-head-a/p)
        (try/p variadic-simple-predicate-a/p)
        (try/p variadic-predicate-with-tail-b/p)
        (try/p variadic-predicate-with-head-b/p)
        (try/p variadic-simple-predicate-b/p)))

(define variadic-composition-with-tail/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'tail)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (and (equal? b c)
             (not (eq? 'any/c b)))
        (pure (list 'variadic-composition/c b (list* 'tail args)))
        (fail/p (message (srcloc #f #f #f #f #f)
                         b
                         (list "contracts are not identical"))))))

(define variadic-composition-with-head/p
  (do (token/p 'OPEN-PAREN)
      (identifier/p 'variadic-function/c)
    [b <- contract/p]
    [c <- contract/p]
    (token/p 'OPEN-PAREN)
    (identifier/p 'head)
    [args <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'CLOSE-PAREN)
    (if (and (equal? b c)
             (not (eq? 'any/c b)))
        (pure (list 'variadic-composition/c b (list* 'head args)))
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
  (or/p (try/p variadic-composition-with-tail/p)
        (try/p variadic-composition-with-head/p)
        (try/p variadic-simple-composition/p)))

(define named-contract-specification/p
  ;; for e.g. (values integer? integer?)
  ;; and even social contracts like (function/c integer? integer?)
  (do (token/p 'OPEN-PAREN)
      [name <- (identifier/p)]
    [ctcs <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (pure (list* name ctcs))))

(define basic-contract/p
  (or/p (try/p (literal/p))
        (try/p (identifier/p))
        (try/p function/p)
        (try/p thunk/p)
        (try/p operation/p)
        (try/p map/p)
        (try/p self-map/p)
        (try/p binary-function/p)
        (try/p binary-operation/p)
        (try/p composition/p)
        (try/p variadic-function/p)
        (try/p predicate/p)
        (try/p binary-predicate/p)
        (try/p variadic-predicate/p)
        (try/p reducer/p)
        (try/p encoder/p)
        (try/p decoder/p)
        (try/p lift/p)
        (try/p hash-function/p)
        (try/p maybe/p)
        (try/p nonempty/p)
        (try/p binary-composition/p)
        (try/p variadic-composition/p)
        (try/p classifier/p)
        (try/p mapper/p)
        (try/p filter/p)
        (try/p functional/p)
        (try/p variadic-constructor/p)
        (try/p named-contract-specification/p)))

(define star-contract-basic/p
  (do (token/p 'OPEN-PAREN)
      [arrowstar <- (identifier/p '->*)]
    (token/p 'OPEN-PAREN)
    [required <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    [optional <- (many/p (or/p (try/p (keyword/p)) contract/p))]
    (token/p 'CLOSE-PAREN)
    [output <- contract/p]
    (token/p 'CLOSE-PAREN)
    (pure (list arrowstar required optional output))))

(define star-contract-with-rest/p
  (do (token/p 'OPEN-PAREN)
      [arrowstar <- (identifier/p '->*)]
    (token/p 'OPEN-PAREN)
    [required <- (many/p contract/p)]
    (token/p 'CLOSE-PAREN)
    (token/p 'OPEN-PAREN)
    [optional <- (many/p (or/p (try/p (keyword/p)) contract/p))]
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
