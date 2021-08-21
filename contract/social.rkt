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
         operation/c
         binary-operation/c
         predicate/c
         binary-predicate/c
         variadic-predicate/c
         encoder/c
         decoder/c
         lift/c
         hash-function/c
         maybe/c
         nonempty/c
         composition/c
         binary-composition/c
         variadic-composition/c
         classifier/c
         map/c
         filter/c
         reducer/c
         functional/c
         binary-constructor/c
         variadic-constructor/c)

(begin-for-syntax
  (define (repeat n v)
    (if (= 0 n)
        null
        (cons v (repeat (sub1 n) v)))))

(define-syntax-parser function/c
  [(_ type/c ...) #'(-> type/c ...)]
  [_:id #'(-> any/c any/c)])

(define-syntax-parser self-map/c
  [(_ type/c ((~datum head) arg/c ...))
   #'(function/c arg/c ... type/c type/c)]
  [(_ type/c ((~datum tail) arg/c ...))
   #'(function/c type/c arg/c ... type/c)]
  [(_ type/c)
   #'(function/c type/c type/c)])

(define-syntax-parser thunk/c
  [(_ target/c) #'(function/c target/c)]
  [_:id #'(function/c any/c)])

(define-syntax-parser binary-function/c
  [(_ a/c b/c target/c) #'(function/c a/c b/c target/c)]
  [_:id #'(function/c any/c any/c any/c)])

(define-syntax-parser operation/c
  [(_ n:number source/c ((~datum head) params ...))
   #'(operation/c n source/c any/c (head params ...))]
  [(_ n:number source/c ((~datum tail) params ...))
   #'(operation/c n source/c any/c (tail params ...))]
  [(_ n:number source/c target/c)
   (datum->syntax this-syntax
                  (append (list 'function/c)
                          (repeat (syntax->datum #'n) #'source/c)
                          (list #'target/c)))]
  [(_ n:number source/c target/c ((~datum head) arg/c ...))
   (datum->syntax this-syntax
                  (append (list 'function/c)
                          (syntax->list #'(arg/c ...))
                          (repeat (syntax->datum #'n) #'source/c)
                          (list #'target/c)))]
  [(_ n:number source/c target/c ((~datum tail) arg/c ...))
   (datum->syntax this-syntax
                  (append (list 'function/c)
                          (repeat (syntax->datum #'n) #'source/c)
                          (syntax->list #'(arg/c ...))
                          (list #'target/c)))]
  [(_ n:number source/c)
   #'(operation/c n source/c any/c)])

(define-syntax-parser binary-operation/c
  [(_ source/c ((~datum head) arg/c ...))
   #'(binary-operation/c source/c any/c (head arg/c ...))]
  [(_ source/c ((~datum tail) arg/c ...))
   #'(binary-operation/c source/c any/c (tail arg/c ...))]
  [(_ source/c target/c)
   #'(binary-function/c source/c source/c target/c)]
  [(_ source/c target/c ((~datum head) arg/c ...))
   #'(operation/c 2 source/c target/c (head arg/c ...))]
  [(_ source/c target/c ((~datum tail) arg/c ...))
   #'(operation/c 2 source/c target/c (tail arg/c ...))]
  [(_ source/c)
   #'(binary-operation/c source/c any/c)])

(define-syntax-parser variadic-function/c
  [(_ source/c target/c ((~datum tail) arg/c ...))
   #:with ··· (quote-syntax ...)
   #'(function/c source/c ··· arg/c ... target/c)]
  [(_ source/c target/c ((~datum head) arg/c ...))
   #:with ··· (quote-syntax ...)
   #'(function/c arg/c ... source/c ··· target/c)]
  [(_ source/c target/c)
   #:with ··· (quote-syntax ...)
   #'(function/c source/c ··· target/c)]
  [(_ source/c)
   #:with ··· (quote-syntax ...)
   #'(function/c source/c ··· any/c)]
  [_:id
   #:with ··· (quote-syntax ...)
   #'(function/c any/c ··· any/c)])

(define-syntax-parser predicate/c
  [(_ on-type/c ...) #'(function/c on-type/c ... boolean?)]
  [_:id #'(predicate/c any/c)])

(define-syntax-parser binary-predicate/c
  [(_ a/c b/c) #'(binary-function/c a/c b/c boolean?)]
  [(_ on-type/c) #'(binary-predicate/c on-type/c on-type/c)]
  [_:id #'(binary-predicate/c any/c)])

(define-syntax-parser variadic-predicate/c
  [(_ a/c ((~datum tail) arg/c ...))
   #'(variadic-function/c a/c boolean? (tail arg/c ...))]
  [(_ a/c ((~datum head) arg/c ...))
   #'(variadic-function/c a/c boolean? (head arg/c ...))]
  [(_ source/c) #'(variadic-function/c source/c boolean?)]
  [_:id #'(variadic-predicate/c any/c)])

(define-syntax-parse-rule (encoder/c as-type/c)
  (function/c any/c as-type/c))

(define-syntax-parse-rule (decoder/c from-type/c)
  (function/c from-type/c any/c))

(define-syntax-parser lift/c
  [(_ pure/c functor/c)
   #'(function/c pure/c (functor/c pure/c))]
  [(_ pure/c functor/c ((~datum head) arg/c ...))
   #'(function/c arg/c ... pure/c (functor/c pure/c))]
  [(_ pure/c functor/c ((~datum tail) arg/c ...))
   #'(function/c pure/c arg/c ... (functor/c pure/c))])

(define-syntax-parser hash-function/c
  [_:id #'(encoder/c fixnum?)])

(define-syntax-parser maybe/c
  [(_ type/c default/c) #'(or/c type/c default/c)]
  [(_ type/c) #'(or/c type/c #f)])

(define-syntax-parse-rule (nonempty/c type/c)
  (and/c type/c (not/c empty?)))

(define-syntax-parser composition/c
  [(_ n:number type/c ((~datum head) arg/c ...))
   #'(operation/c n type/c type/c (head arg/c ...))]
  [(_ n:number type/c ((~datum tail) arg/c ...))
   #'(operation/c n type/c type/c (tail arg/c ...))]
  [(_ n:number type/c)
   #'(operation/c n type/c type/c)])

(define-syntax-parser binary-composition/c
  [(_ type/c ((~datum head) arg/c ...))
   #'(binary-operation/c type/c type/c (head arg/c ...))]
  [(_ type/c ((~datum tail) arg/c ...))
   #'(binary-operation/c type/c type/c (tail arg/c ...))]
  [(_ type/c) #'(binary-operation/c type/c type/c)])

(define-syntax-parser variadic-composition/c
  [(_ type/c)
   #'(variadic-function/c type/c type/c)]
  [(_ type/c ((~datum head) arg/c ...))
   #'(variadic-function/c type/c type/c (head arg/c ...))]
  [(_ type/c ((~datum tail) arg/c ...))
   #'(variadic-function/c type/c type/c (tail arg/c ...))])

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
  [(_ type/c) #'(self-map/c (sequenceof type/c)
                            (head (predicate/c type/c)))]
  [_:id #'(filter/c any/c)])

(define-syntax-parser reducer/c
  [(_ type/c target/c) #'(function/c (sequenceof type/c)
                                     target/c)]
  [(_ type/c) #'(reducer/c type/c type/c)]
  [_:id #'(reducer/c any/c)])

(define-syntax-parser functional/c
  [_:id #'(self-map/c procedure?)])

(define-syntax-parser binary-constructor/c
  [(_ (~seq #:order (~datum 'abb)) arg/c type/c)
   #'(self-map/c type/c (head arg/c))]
  [(_ (~seq #:order (~datum 'bab)) arg/c type/c)
   #'(self-map/c type/c (tail arg/c))]
  [(_ arg/c type/c)
   ;; default to abb order
   #'(binary-constructor/c #:order 'abb arg/c type/c)])

(define-syntax-parser variadic-constructor/c
  [(_ (~seq #:order (~datum 'abb)) primitive/c composite/c)
   #'(variadic-function/c primitive/c composite/c (tail composite/c))]
  [(_ (~seq #:order (~datum 'bab)) primitive/c composite/c)
   #'(variadic-function/c primitive/c composite/c (head composite/c))]
  [(_ primitive/c composite/c)
   ;; default to abb order
   #'(variadic-constructor/c #:order 'abb primitive/c composite/c)])
