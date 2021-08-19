#lang racket/base

(require racket/contract
         racket/match
         syntax/parse/define
         version-case
         (only-in mischief/shorthand define-alias)
         (for-syntax racket/base)
         (only-in data/collection
                  sequenceof
                  sequence?
                  empty?))

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)]
 [else])

(provide function/c
         thunk/c
         self-map/c
         binary-function/c
         variadic-function/c
         predicate/c
         binary-predicate/c
         variadic-predicate/c
         encoder/c
         decoder/c
         lift/c
         hash-function/c
         maybe/c
         nonempty/c
         binary-composition/c
         variadic-composition/c
         classifier/c
         map/c
         filter/c
         reducer/c
         functional/c
         parametrized-self-map/c
         binary-constructor/c
         variadic-constructor/c)

(define-syntax-parser function/c
  [(_ source/c target/c) #'(-> source/c target/c)]
  [_:id #'(-> any/c any/c)])

(define-syntax-parse-rule (self-map/c type/c)
  (function/c type/c type/c))

(define-syntax-parser thunk/c
  [(_ target/c) #'(-> target/c)]
  [_:id #'(-> any/c)])

(define-syntax-parser binary-function/c
  [(_ a/c b/c target/c) #'(-> a/c b/c target/c)]
  [(_ type/c) #'(-> type/c type/c any/c)]
  [(_ type/c target/c) #'(-> type/c type/c target/c)]
  [_:id #'(-> any/c any/c any/c)])

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
  [(_ source/c)
   #:with ··· (quote-syntax ...)
   #'(-> source/c ··· any/c)]
  [_:id
   #:with ··· (quote-syntax ...)
   #'(-> any/c ··· any/c)])

(define-syntax-parser predicate/c
  [(_ on-type/c) #'(function/c on-type/c boolean?)]
  [_:id #'(predicate/c any/c)])

(define-syntax-parser binary-predicate/c
  [(_ a/c b/c) #'(binary-function/c a/c b/c boolean?)]
  [(_ on-type/c) #'(binary-predicate/c on-type/c on-type/c)]
  [_:id #'(binary-predicate/c any/c)])

(define-syntax-parser variadic-predicate/c
  [(_ a/c ((~datum tail) b/c))
   #'(variadic-function/c a/c (tail b/c) boolean?)]
  [(_ a/c b/c)
   #'(variadic-function/c a/c b/c boolean?)]
  [(_ source/c) #'(variadic-function/c source/c boolean?)]
  [_:id #'(variadic-predicate/c any/c)])

(define-syntax-parse-rule (encoder/c as-type/c)
  (function/c any/c as-type/c))

(define-syntax-parse-rule (decoder/c from-type/c)
  (function/c from-type/c any/c))

(define-syntax-parse-rule (lift/c pure/c functor/c)
  (function/c pure/c (functor/c pure/c)))

(define-syntax-parser hash-function/c
  [_:id #'(encoder/c fixnum?)])

(define-syntax-parser maybe/c
  [(_ type/c default/c) #'(or/c type/c default/c)]
  [(_ type/c) #'(or/c type/c #f)])

(define-syntax-parse-rule (nonempty/c type/c)
  (and/c type/c (not/c empty?)))

(define-syntax-parse-rule (binary-composition/c type/c)
  (binary-function/c type/c type/c type/c))

(define-syntax-parser variadic-composition/c
  [(_ type/c) #'(variadic-function/c type/c type/c)]
  [(_ type/c _) #'(variadic-function/c type/c type/c type/c)]) ; support minimum required arity instead?

(define-syntax-parser classifier/c
  [(_ by-type/c) #'(binary-function/c (encoder/c by-type/c)
                                      sequence?
                                      (sequenceof sequence?))]
  [_:id #'(classifier/c any/c)])

(define-syntax-parser map/c
  [(_ source/c target/c) #'(binary-function/c (function/c source/c target/c)
                                              (sequenceof source/c)
                                              (sequenceof target/c))]
  [(_ source/c) #'(map/c source/c source/c)]
  [_:id #'(map/c any/c any/c)])

(define-syntax-parser filter/c
  [(_ type/c) #'(parametrized-self-map/c (predicate/c type/c)
                                         (sequenceof type/c))]
  [_:id #'(filter/c any/c)])

(define-syntax-parser reducer/c
  [(_ type/c target/c) #'(function/c (sequenceof type/c)
                                     target/c)]
  [(_ type/c) #'(reducer/c type/c type/c)]
  [_:id #'(reducer/c any/c)])

(define-syntax-parser functional/c
  [_:id #'(self-map/c procedure?)])

(define-syntax-parser parametrized-self-map/c
  [(_ (~seq #:order (~datum 'abb)) arg/c type/c)
   #'(binary-function/c arg/c type/c type/c)]
  [(_ (~seq #:order (~datum 'bab)) arg/c type/c)
   #'(binary-function/c type/c arg/c type/c)]
  [(_ arg/c type/c)
   ;; default to abb order
   #'(parametrized-self-map/c #:order 'abb arg/c type/c)])

(define-alias binary-constructor/c parametrized-self-map/c)

(define-syntax-parser variadic-constructor/c
  [(_ (~seq #:order (~datum 'abb)) primitive/c composite/c)
   #'(variadic-function/c primitive/c (tail composite/c) composite/c)]
  [(_ (~seq #:order (~datum 'bab)) primitive/c composite/c)
   #'(variadic-function/c composite/c primitive/c composite/c)]
  [(_ primitive/c composite/c)
   ;; default to abb order
   #'(variadic-constructor/c #:order 'abb primitive/c composite/c)])
