#lang racket/base

(require racket/contract
         racket/match
         (only-in data/collection
                  sequenceof
                  sequence?))

(provide
 (contract-out [function/c (case->
                            (-> contract?)
                            (-> contract? contract? contract?))]
               [self-map/c (self-map/c contract?)]
               [thunk/c (->* () (contract?) contract?)]
               [binary-function/c (->* ()
                                       (contract?
                                        (maybe/c contract?)
                                        (maybe/c contract?))
                                       contract?)]
               [variadic-function/c (->* ()
                                         (contract? (maybe/c contract?))
                                         contract?)]
               [binary-variadic-function/c (->* ()
                                                (contract?
                                                 (maybe/c contract?)
                                                 (maybe/c contract?)
                                                 #:tail? (maybe/c contract?))
                                                contract?)]
               [predicate/c (->* ()
                                 (contract?)
                                 contract?)]
               [binary-predicate/c (->* ()
                                        (contract?
                                         (maybe/c contract?))
                                        contract?)]
               [variadic-predicate/c (->* ()
                                          (contract?)
                                          contract?)]
               [binary-variadic-predicate/c (->* ()
                                                 (contract?
                                                  (maybe/c contract?)
                                                  #:tail? (maybe/c contract?))
                                                 contract?)]
               [encoder/c (self-map/c contract?)]
               [decoder/c (self-map/c contract?)]
               [hash-function/c (thunk/c contract?)]
               [maybe/c (->* (contract?)
                             (contract?)
                             contract?)]
               [binary-composition/c (self-map/c contract?)]
               [variadic-composition/c (self-map/c contract?)]
               [binary-variadic-composition/c (self-map/c contract?)]
               [classifier/c (->* ()
                                  (contract?)
                                  contract?)]
               [map/c (->* ()
                           (contract? (maybe/c contract?))
                           contract?)]
               [filter/c (->* ()
                              (contract?)
                              contract?)]
               [reducer/c (->* ()
                               (contract? (maybe/c contract?))
                               contract?)]
               [functional/c (->* ()
                                  (contract?)
                                  contract?)]
               [binary-constructor/c (->* (contract? contract?)
                                          (#:order (one-of/c 'abb 'bab))
                                          contract?)]
               [variadic-constructor/c (->* (contract? contract?)
                                            (#:order (one-of/c 'abb 'bab))
                                            contract?)]))

(define function/c
  (case-lambda
    [() (-> any/c any/c)]
    [(source/c target/c)
     (-> source/c target/c)]))

(define (self-map/c type/c)
  (function/c type/c type/c))

(define (thunk/c [target/c any/c])
  (-> target/c))

(define (binary-function/c [a/c any/c]
                           [b/c #f]
                           [target/c #f])
  (let ([b/c (or b/c a/c)]
        [target/c (or target/c a/c)])
    (-> a/c b/c target/c)))

(define (variadic-function/c [source/c any/c]
                             [target/c #f])
  (let ([target/c (or target/c source/c)])
    (-> source/c ... target/c)))

(define (binary-variadic-function/c [a/c any/c]
                                    [b/c #f]
                                    [target/c #f]
                                    #:tail? [tail? #f])
  (let ([b/c (or b/c a/c)]
        [target/c (or target/c a/c)])
    (if tail?
        (-> a/c ... b/c target/c)
        (-> a/c b/c ... target/c))))

(define (predicate/c [on-type/c any/c])
  (function/c on-type/c boolean?))

(define (binary-predicate/c [a/c any/c]
                            [b/c #f])
  (binary-function/c a/c b/c boolean?))

(define (variadic-predicate/c [source/c any/c])
  (variadic-function/c source/c boolean?))

(define (binary-variadic-predicate/c [a/c any/c]
                                     [b/c #f]
                                     #:tail? [tail? #f])
  (binary-variadic-function/c #:tail? tail?
                              a/c b/c boolean?))

(define (encoder/c as-type/c)
  (function/c any/c as-type/c))

(define (decoder/c from-type/c)
  (function/c from-type/c any/c))

(define (hash-function/c)
  (encoder/c fixnum?))

(define (maybe/c type/c [default/c #f])
  (or/c type/c default/c))

(define (binary-composition/c type/c)
  (binary-function/c type/c type/c type/c))

(define (variadic-composition/c type/c)
  (variadic-function/c type/c type/c))

(define (binary-variadic-composition/c type/c)
  (binary-variadic-function/c type/c))

(define (classifier/c [by-type/c any/c])
  (binary-function/c (encoder/c by-type/c)
                     sequence?
                     (sequenceof sequence?)))

(define (map/c [source/c any/c]
               [target/c #f])
  (let ([target/c (or target/c source/c)])
    (binary-function/c (function/c source/c target/c)
                       (sequenceof source/c)
                       (sequenceof target/c))))

(define (filter/c [of-type/c any/c])
  (binary-function/c (predicate/c of-type/c)
                     (sequenceof of-type/c)
                     (sequenceof of-type/c)))

(define (reducer/c [type/c any/c]
                   [target/c #f])
  (let ([target/c (or target/c type/c)])
    (function/c (sequenceof type/c) target/c)))

(define (functional/c [procedure/c procedure?])
  (self-map/c procedure/c))

(define (binary-constructor/c primitive/c
                              composite/c
                              #:order [order 'abb])
  (match order
    ['abb (binary-function/c primitive/c composite/c composite/c)]
    ['bab (binary-function/c composite/c primitive/c composite/c)]))

(define (variadic-constructor/c primitive/c
                                composite/c
                                #:order [order 'abb])
  (match order
    ['abb (binary-variadic-function/c #:tail? #t primitive/c composite/c composite/c)]
    ['bab (binary-variadic-function/c composite/c primitive/c composite/c)]))
