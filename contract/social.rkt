#lang racket/base

(require racket/contract
         (only-in data/collection
                  sequenceof))

(provide
 (contract-out [function/c (->* () (contract? contract?) contract?)]
               [self-map/c (self-map/c contract?)]
               [thunk/c (->* () (contract?) contract?)]
               [binary-function/c (-> contract? contract? contract? contract?)]
               [variadic-function/c (binary-composition/c contract?)]
               [binary-variadic-function/c (-> contract? contract? contract? contract?)]
               [predicate/c (->* () (contract?) contract?)]
               [encoder/c (self-map/c contract?)]
               [decoder/c (self-map/c contract?)]
               [hash-function/c (thunk/c contract?)]
               [maybe/c (->* (contract?) (contract?) contract?)]
               [binary-composition/c (self-map/c contract?)]
               [variadic-composition/c (self-map/c contract?)]
               [reducer/c (self-map/c contract?)]
               [functional/c (->* () (contract?) contract?)]
               [binary-constructor/c (binary-composition/c contract?)]
               [binary-constructor-abb/c (binary-composition/c contract?)]
               [binary-constructor-bab/c (binary-composition/c contract?)]
               [classifier/c (->* () (contract?) contract?)]
               [variadic-constructor/c (binary-composition/c contract?)]
               [variadic-constructor-abb/c (binary-composition/c contract?)]
               [variadic-constructor-bab/c (binary-composition/c contract?)]))

(define (function/c [source/c any/c] [target/c any/c])
  (-> source/c target/c))

(define (thunk/c [target/c any/c])
  (-> target/c))

(define (binary-function/c a/c b/c target/c)
  (-> a/c b/c target/c))

(define (variadic-function/c source/c target/c)
  (-> source/c ... target/c))

(define (binary-variadic-function/c a/c b/c target/c)
  (-> a/c b/c ... target/c))

(define (predicate/c [on-type/c any/c])
  (function/c on-type/c boolean?))

(define (encoder/c as-type/c)
  (function/c any/c as-type/c))

(define (decoder/c from-type/c)
  (function/c from-type/c any/c))

(define (hash-function/c)
  (encoder/c fixnum?))

(define (maybe/c contract [default/c #f])
  (or/c contract default/c))

(define (binary-composition/c type/c)
  (binary-function/c type/c type/c type/c))

(define (variadic-composition/c type/c)
  (variadic-function/c type/c type/c))

(define (reducer/c type/c)
  (function/c (sequenceof type/c) type/c))

(define (self-map/c type/c)
  (function/c type/c type/c))

(define (functional/c [procedure/c procedure?])
  (self-map/c procedure/c))

(define (binary-constructor-abb/c primitive/c composite/c)
  (binary-function/c primitive/c composite/c composite/c))

(define (binary-constructor-bab/c primitive/c composite/c)
  (binary-function/c composite/c primitive/c composite/c))

(define binary-constructor/c binary-constructor-abb/c)

(define (classifier/c [by-type/c any/c])
  (binary-function/c (encoder/c by-type/c)
                     list?
                     (listof list?)))

(define (variadic-constructor-abb/c primitive/c composite/c)
  (binary-variadic-function/c primitive/c primitive/c composite/c))

(define (variadic-constructor-bab/c primitive/c composite/c)
  (binary-variadic-function/c composite/c primitive/c composite/c))

(define variadic-constructor/c variadic-constructor-abb/c)
