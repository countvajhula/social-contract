#lang racket/base

(require racket/contract
         (only-in data/collection
                  sequenceof))

(provide
 (contract-out [function/c (-> contract? contract? contract?)]
               [binary-function/c (-> contract? contract? contract? contract?)]
               [variadic-function/c (-> contract? contract? contract?)]
               [encoder/c (-> contract? contract?)]
               [decoder/c (-> contract? contract?)]
               [maybe/c (->* (contract?) (contract?) contract?)]
               [binary-composition/c (-> contract? contract?)]
               [variadic-composition/c (-> contract? contract?)]
               [reducer/c (-> contract? contract?)]
               [self-map/c (-> contract? contract?)]
               [functional/c (->* () (contract?) contract?)]
               [binary-constructor/c (-> contract? contract? contract?)]
               [variadic-constructor/c (-> contract? contract? contract?)]
               [variadic-comparison-predicate/c (-> contract? contract?)]
               [variadic-comparison-selection/c (-> contract? contract?)]))

(define (function/c source/c target/c)
  (-> source/c target/c))

(define (binary-function/c a/c b/c target/c)
  (-> a/c b/c target/c))

(define (variadic-function/c source/c target/c)
  (-> source/c ... target/c))

(define (encoder/c as-type)
  (function/c any/c as-type))

(define (decoder/c from-type)
  (function/c from-type any/c))

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

(define (binary-constructor/c primitive/c composite/c)
  (binary-function/c primitive/c composite/c composite/c))

(define (variadic-constructor/c primitive/c composite/c)
  (variadic-function/c primitive/c composite/c))

(define (variadic-comparison/c type/c return/c)
  ;; TODO: improve to ensure that arguments are type/c
  ;; (rather than any/c) when no key is provided
  (->* (any/c)
       (#:key (maybe/c (encoder/c type/c)))
       #:rest list?
       return/c))

(define (variadic-comparison-predicate/c type/c)
  (variadic-comparison/c type/c boolean?))

(define (variadic-comparison-selection/c type/c)
  (variadic-comparison/c type/c any/c))
