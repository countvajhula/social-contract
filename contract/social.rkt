#lang racket/base

(require racket/contract
         racket/match
         syntax/parse/define
         version-case
         (only-in mischief/shorthand define-alias)
         (for-syntax racket/base)
         (only-in data/collection
                  sequenceof
                  sequence?))

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)]
 [else])

(provide
 function/c
 thunk/c
 self-map/c
 binary-function/c
 variadic-function/c
 predicate/c
 binary-predicate/c
 variadic-predicate/c
 encoder/c
 decoder/c
 hash-function/c
 maybe/c
 binary-composition/c
 variadic-composition/c
 classifier/c
 map/c
 filter/c
 reducer/c
 (contract-out [functional/c (->* ()
                                  (contract?)
                                  contract?)]
               [binary-constructor/c (->* (contract? contract?)
                                          (#:order (one-of/c 'abb 'bab))
                                          contract?)]
               [variadic-constructor/c (->* (contract? contract?)
                                            (#:order (one-of/c 'abb 'bab))
                                            contract?)]))

(define-syntax-parser function/c
  [(_ source/c target/c) #'(-> source/c target/c)]
  [(_) #'(-> any/c any/c)] ; backwards compat - remove later
  [_ #'(-> any/c any/c)])

(define-syntax-parse-rule (self-map/c type/c)
  (function/c type/c type/c))

(define-syntax-parser thunk/c
  [(_ (~optional target/c #:defaults ([target/c #'any/c])))
   #'(-> target/c)])

(define-syntax-parser binary-function/c
  [(_ a/c b/c target/c) #'(-> a/c b/c target/c)]
  [(_) #'(-> any/c any/c any/c)] ; backwards compat - remove later
  [_ #'(-> any/c any/c any/c)])

;; TODO: support any number of contracts preceding
;; the variadic argument.
;; maybe DSL-ify it so that we can indicate the arity
;; of distinct contracts beforehand, so that they can
;; default to the right _number_ of them, and can be
;; left unspecified
(define-syntax-parser variadic-function/c
  [(_ a/c ((~datum tail) b/c) target/c)
   #:with ··· (quote-syntax ...)
   #'(-> a/c ··· b/c target/c)]
  [(_ a/c b/c target/c)
   #:with ··· (quote-syntax ...)
   #'(-> a/c b/c ··· target/c)]
  [(_ source/c target/c)
   #:with ··· (quote-syntax ...)
   #'(-> source/c ··· target/c)]
  [(_)
   #:with ··· (quote-syntax ...) ; backwards compat - remove later
   #'(-> any/c ··· any/c)]
  [_
   #:with ··· (quote-syntax ...)
   #'(-> any/c ··· any/c)])

;; review variadic as the need presents itself

(define-syntax-parser predicate/c
  [(_ on-type/c) #'(function/c on-type/c boolean?)]
  [(_) #'(function/c any/c boolean?)]
  [_ #'(function/c any/c boolean?)])

(define-syntax-parser binary-predicate/c
  [(_ a/c b/c) #'(binary-function/c a/c b/c boolean?)]
  [(_ on-type/c) #'(binary-function/c on-type/c on-type/c boolean?)]
  [(_) #'(binary-function/c any/c any/c boolean?)] ; backwards compat - remove later
  [_ #'(binary-function/c any/c any/c boolean?)])

(define-syntax-parser variadic-predicate/c
  [(_ a/c ((~datum tail) b/c))
   #'(variadic-function/c a/c (tail b/c) boolean?)]
  [(_ a/c b/c)
   #'(variadic-function/c a/c b/c boolean?)]
  [(_ source/c) #'(variadic-function/c source/c boolean?)]
  [(_) #'(variadic-function/c any/c boolean?)] ; backwards compat - remove later
  [_ #'(variadic-function/c any/c boolean?)])

(define-syntax-parse-rule (encoder/c as-type/c)
  (function/c any/c as-type/c))

(define-syntax-parse-rule (decoder/c from-type/c)
  (function/c from-type/c any/c))

(define-syntax-parser hash-function/c
  [(_) #'(encoder/c fixnum?)] ; backwards compat - remove later
  [_ #'(encoder/c fixnum?)])

(define-syntax-parser maybe/c
  [(_ type/c (~optional default/c #:defaults ([default/c #'#f])))
   #'(or/c type/c default/c)])

(define-syntax-parse-rule (binary-composition/c type/c)
  (binary-function/c type/c type/c type/c))

(define-syntax-parser variadic-composition/c
  [(_ type/c) #'(variadic-function/c type/c type/c)]
  [(_ type/c _) #'(variadic-function/c type/c type/c type/c)]) ; support minimum required arity instead?

(define-syntax-parser classifier/c
  [(_ (~optional by-type/c #:defaults ([by-type/c #'any/c])))
   #'(binary-function/c (encoder/c by-type/c)
                        sequence?
                        (sequenceof sequence?))])

(define-syntax-parser map/c
  [(_ source/c target/c) #'(binary-function/c (function/c source/c target/c)
                                              (sequenceof source/c)
                                              (sequenceof target/c))]
  [(_ source/c) #'(binary-function/c (self-map/c source/c)
                                     (sequenceof source/c)
                                     (sequenceof source/c))]
  [(_) #'(binary-function/c function/c
                            sequence?
                            sequence?)] ; backward compat - remove later
  [_ #'(binary-function/c function/c
                          sequence?
                          sequence?)])

(define-syntax-parser filter/c
  [(_ (~optional of-type/c #:defaults ([of-type/c #'any/c])))
   #'(binary-function/c (predicate/c of-type/c)
                        (sequenceof of-type/c)
                        (sequenceof of-type/c))])

(define-syntax-parser reducer/c
  [(_ type/c target/c) #'(function/c (sequenceof type/c)
                                     target/c)]
  [(_ type/c) #'(function/c (sequenceof type/c) type/c)]
  [(_) #'(function/c sequence? any/c)] ; backward compat - remove later
  [_ #'(function/c sequence? any/c)])

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
    ['abb (variadic-function/c #:tail? #t primitive/c composite/c composite/c)]
    ['bab (variadic-function/c composite/c primitive/c composite/c)]))
