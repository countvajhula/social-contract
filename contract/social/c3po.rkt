#lang racket

(require racket/format
         syntax/parse/define
         mischief/shorthand
         version-case)

(require contract/social/c3po/parser
         contract/social/c3po/compiler)

(version-case
 [(version< (version) "7.9.0.22")
  (define-alias define-syntax-parse-rule define-simple-macro)])

(module+ test
  (require rackunit
           rackunit/text-ui))

(provide translate)

(define (~translate src parser)
  (read
   (open-input-string
    (upcompile src
               parser))))

(define-syntax-parse-rule (translate src)
  (~translate (~a 'src) spec/p))

(module+ test
  (define c3po-tests
    (test-suite
     "c3po tests"
     (test-suite
      "contract tests"

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
       (check-equal? (translate (-> any/c any/c any/c)) 'binary-function/c))

      (test-suite
       "binary-operation/c"
       (check-equal? (translate (-> a a b)) '(binary-operation/c a b))
       (check-equal? (translate (-> a a any/c)) '(binary-operation/c a)))

      (test-suite
       "variadic-function/c"
       (check-equal? (translate (-> a ... b c)) '(variadic-function/c a c (tail b)))
       (check-equal? (translate (-> a b ... c)) '(variadic-function/c b c (head a)))
       (check-equal? (translate (-> a q b ... c)) '(variadic-function/c b c (head a q)))
       (check-equal? (translate (-> a ... b)) '(variadic-function/c a b))
       (check-equal? (translate (-> a ... any/c)) '(variadic-function/c a))
       (check-equal? (translate (-> any/c ... any/c)) 'variadic-function/c))

      (test-suite
       "operation/c"
       (check-equal? (translate (-> a a a b)) '(operation/c 3 a b))
       (check-equal? (translate (-> a a a a b)) '(operation/c 4 a b))
       (check-equal? (translate (-> a a a c b)) '(operation/c 3 a b (tail c)))
       (check-equal? (translate (-> c a a a b)) '(operation/c 3 a b (head c)))
       (check-equal? (translate (-> a a a any/c)) '(operation/c 3 a))
       (check-equal? (translate (-> a a a b any/c)) '(operation/c 3 a (tail b)))
       (check-equal? (translate (-> b a a a any/c)) '(operation/c 3 a (head b))))

      (test-suite
       "maybe/c"
       (check-equal? (translate (or/c number? void?)) '(maybe/c number? void?))
       (check-equal? (translate (or/c number? #f)) '(maybe/c number?)))

      (test-suite
       "nonempty/c"
       (check-equal? (translate (and/c list? (not/c empty?))) '(nonempty/c list?))
       (check-equal? (translate (and/c string? (not/c empty?))) '(nonempty/c string?))
       (check-equal? (translate (and/c (listof number?) (not/c empty?))) '(nonempty/c (listof number?))))

      (test-suite
       "self-map/c"
       (check-equal? (translate (-> a a)) '(self-map/c a)))

      (test-suite
       "parametrized self-map/c"
       (check-equal? (translate (-> a b b)) '(self-map/c b (head a)))
       (check-equal? (translate (-> b a b)) '(self-map/c b (tail a)))
       (check-equal? (translate (-> a c b b)) '(self-map/c b (head a c)))
       (check-equal? (translate (-> b a c b)) '(self-map/c b (tail a c))))

      (test-suite
       "functional/c"
       (check-equal? (translate (-> procedure? procedure?)) 'functional/c))

      (test-suite
       "predicate/c"
       (check-equal? (translate (-> a boolean?)) '(predicate/c a))
       (check-equal? (translate (-> a b c boolean?)) '(predicate/c a b c))
       (check-equal? (translate (-> any/c boolean?)) 'predicate/c))

      (test-suite
       "binary-predicate/c"
       (check-equal? (translate (-> a b boolean?)) '(binary-predicate/c a b))
       (check-equal? (translate (-> a a boolean?)) '(binary-predicate/c a))
       (check-equal? (translate (-> any/c any/c boolean?)) 'binary-predicate/c))

      (test-suite
       "variadic-predicate/c"
       (check-equal? (translate (-> a ... b boolean?)) '(variadic-predicate/c a (tail b)))
       (check-equal? (translate (-> a b ... boolean?)) '(variadic-predicate/c b (head a)))
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
       "lift/c"
       (check-equal? (translate (-> number? (listof number?))) '(lift/c number? listof))
       (check-equal? (translate (-> number? (sequenceof number?))) '(lift/c number? sequenceof)))

      (test-suite
       "hash-function/c"
       (check-equal? (translate (-> any/c fixnum?)) 'hash-function/c))

      (test-suite
       "composition/c"
       (check-equal? (translate (-> a a a a)) '(composition/c 3 a))
       (check-equal? (translate (-> a a a a a)) '(composition/c 4 a)))

      (test-suite
       "binary-composition/c"
       (check-equal? (translate (-> a a a)) '(binary-composition/c a)))

      (test-suite
       "variadic-composition/c"
       (check-equal? (translate (-> a ... a)) '(variadic-composition/c a))
       (check-equal? (translate (-> a a ... a)) '(variadic-composition/c a a))
       (check-equal? (translate (-> a ... a a)) '(variadic-composition/c a a)))

      (test-suite
       "variadic-constructor/c"
       (check-equal? (translate (-> a ... b b)) '(variadic-constructor/c a b))
       (check-equal? (translate (-> b a ... b)) '(variadic-constructor/c #:order 'bab a b)))

      (test-suite
       "classifier/c"
       (check-equal? (translate (-> (-> any/c number?) sequence? (sequenceof sequence?))) '(classifier/c number?))
       (check-equal? (translate (-> (-> any/c string?) sequence? (sequenceof sequence?))) '(classifier/c string?))
       (check-equal? (translate (-> (-> any/c number?) sequence? (listof sequence?))) '(classifier/c number?))
       (check-equal? (translate (-> (-> any/c number?) list? (listof list?))) '(classifier/c number?)))

      (test-suite
       "map/c"
       (check-equal? (translate (-> (-> number? string?) (sequenceof number?) (sequenceof string?))) '(map/c number? string?))
       (check-equal? (translate (-> (-> any/c any/c) (sequenceof any/c) (sequenceof any/c))) 'map/c)
       (check-equal? (translate (-> (-> any/c any/c) (listof any/c) (listof any/c))) 'map/c)
       (check-equal? (translate (-> (-> any/c any/c) sequence? sequence?)) 'map/c)
       (check-equal? (translate (-> (-> any/c any/c) list? list?)) 'map/c)
       (check-equal? (translate (-> (-> number? string?) (listof number?) (listof string?))) '(map/c number? string?)))

      (test-suite
       "filter/c"
       (check-equal? (translate (-> (-> number? boolean?) (sequenceof number?) (sequenceof number?))) '(filter/c number?))
       (check-equal? (translate (-> (-> any/c boolean?) (sequenceof any/c) (sequenceof any/c))) 'filter/c)
       (check-equal? (translate (-> (-> any/c boolean?) (listof any/c) (listof any/c))) 'filter/c)
       (check-equal? (translate (-> (-> any/c boolean?) sequence? sequence?)) 'filter/c)
       (check-equal? (translate (-> (-> any/c boolean?) list? list?)) 'filter/c)
       (check-equal? (translate (-> (-> number? boolean?) (listof number?) (listof number?))) '(filter/c number?)))

      (test-suite
       "reducer/c"
       (check-equal? (translate (-> (sequenceof number?) string?)) '(reducer/c number? string?))
       (check-equal? (translate (-> (sequenceof number?) number?)) '(reducer/c number?))
       (check-equal? (translate (-> (sequenceof any/c) any/c)) 'reducer/c)
       (check-equal? (translate (-> (listof any/c) any/c)) 'reducer/c)
       (check-equal? (translate (-> sequence? any/c)) 'reducer/c)
       (check-equal? (translate (-> list? any/c)) 'reducer/c)
       (check-equal? (translate (-> (listof number?) number?)) '(reducer/c number?))
       (check-equal? (translate (-> (listof number?) string?)) '(reducer/c number? string?)))

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
       "infix arrow contracts"
       (check-equal? (translate (a . -> . b))
                     '(function/c a b))
       (check-equal? (translate (a b . -> . c))
                     '(binary-function/c a b c))
       (check-equal? (translate (a ... . -> . b))
                     '(variadic-function/c a b))
       (check-equal? (translate (a b ... . -> . c))
                     '(variadic-function/c b c (head a)))
       (check-equal? (translate (a ... b . -> . c))
                     '(variadic-function/c a c (tail b))))

      (test-suite
       "edge cases"
       (check-equal? (translate (-> (function/c number? string?) (sequenceof string?) (sequenceof number?)))
                     '(binary-function/c (function/c number? string?) (sequenceof string?) (sequenceof number?)))
       (check-equal? (translate (-> (-> number? string?) (sequenceof string?) (sequenceof number?)))
                     '(binary-function/c (function/c number? string?) (sequenceof string?) (sequenceof number?))))

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
                     '(self-map/c list? (head number?)))
       (check-equal? (translate (binary-function/c list? number? list?))
                     '(self-map/c list? (tail number?)))
       (check-equal? (translate (binary-operation/c number? boolean?))
                     '(binary-predicate/c number?))
       (check-equal? (translate (predicate/c number? ...))
                     '(variadic-predicate/c number?))
       (check-equal? (translate (binary-operation/c boolean? boolean?))
                     '(binary-composition/c boolean?))
       (check-equal? (translate (operation/c 2 number? list?))
                     '(binary-operation/c number? list?))
       (check-equal? (translate (operation/c 2 number? list? (tail string?)))
                     '(binary-operation/c number? list? (tail string?)))
       (check-equal? (translate (operation/c 2 number? list? (head string?)))
                     '(binary-operation/c number? list? (head string?)))
       (check-equal? (translate (operation/c 3 number? number?))
                     '(composition/c 3 number?))
       (check-equal? (translate (operation/c 2 number? number?))
                     '(binary-composition/c number?))
       (check-equal? (translate (variadic-function/c number? number?))
                     '(variadic-composition/c number?))
       (check-equal? (translate (variadic-function/c number? boolean?))
                     '(variadic-predicate/c number?))
       (check-equal? (translate (variadic-function/c number? list? (tail list?)))
                     '(variadic-constructor/c number? list?))
       (check-equal? (translate (variadic-function/c number? list? (head list?)))
                     '(variadic-constructor/c #:order 'bab number? list?))
       (check-equal? (translate (maybe/c number? #f))
                     '(maybe/c number?))))

     (test-suite
      "provide-tests"
      (test-suite
       "pass-through"
       (check-equal? (translate (provide abc)) '(provide abc))
       (check-equal? (translate (provide a b (contract-out [c (->i p q)])))
                     '(provide a b (contract-out [c (->i p q)]))))

      (test-suite
       "->"
       (check-equal? (translate (provide (contract-out [c (-> a b)])))
                     '(provide (contract-out [c (function/c a b)])))
       (check-equal? (translate (provide abc (contract-out [c (-> a b)]) def))
                     '(provide abc (contract-out [c (function/c a b)]) def)))

      (test-suite
       "->*"
       (check-equal? (translate (provide (contract-out [c (->* ((-> a b) (-> a a))
                                                               (#:key (-> a b) (-> a a))
                                                               (-> a a))])))
                     '(provide (contract-out [c (->* ((function/c a b) (self-map/c a))
                                                     (#:key (function/c a b) (self-map/c a))
                                                     (self-map/c a))])))
       (check-equal? (translate (provide (contract-out [c (->* ((-> a b) (-> a a))
                                                               (#:key (-> a b) (-> a a))
                                                               #:rest (-> a b)
                                                               (-> a a))])))
                     '(provide (contract-out [c (->* ((function/c a b) (self-map/c a))
                                                     (#:key (function/c a b) (self-map/c a))
                                                     #:rest (function/c a b)
                                                     (self-map/c a))])))))))

  (void (run-tests c3po-tests)))
