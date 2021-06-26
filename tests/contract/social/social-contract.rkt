#lang racket/base

(require rackunit
         rackunit/text-ui
         (except-in racket/contract
                    predicate/c)
         (only-in racket/list
                  first
                  rest
                  empty?)
         contract/social
         (only-in racket/function thunk curryr)
         (only-in adjutor values->list)
         "private/util.rkt")

(define tests
  (test-suite
   "Tests for social contracts"

   (test-suite
    "Unary function"
    (test-case
        "Basic"
      (define/contract (g lst)
        (function/c list? natural-number/c)
        0)
      (check-equal? (g '(h e l l o)) 0)
      (check-exn exn:fail:contract? (thunk (g "hello")))
      (check-exn exn:fail:contract? (thunk (g '(h e l l o) '(h e l l o))))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Return value"
      (define/contract (g lst)
        (function/c list? natural-number/c)
        (list 0))
      (check-exn exn:fail:contract? (thunk (g '(h e l l o)))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g lst)
        function/c
        0)
      (check-equal? (g '(h e l l o)) 0)
      (check-equal? (g "hello") 0)
      (check-exn exn:fail:contract? (thunk (g '(h e l l o) '(h e l l o))))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Defaults with no parameters - backwards compat"
      (define/contract (g lst)
        (function/c)
        0)
      (check-equal? (g '(h e l l o)) 0)
      (check-equal? (g "hello") 0)
      (check-exn exn:fail:contract? (thunk (g '(h e l l o) '(h e l l o))))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "any"
      (define/contract (g lst)
        (function/c list? any)
        (values 0 1))
      (check-equal? (values->list (g '(h e l l o)))
                    (list 0 1))
      (check-exn exn:fail:contract? (thunk (g "hello"))))
    (test-case
        "values"
      (define/contract (g lst)
        (function/c list? (values positive? negative?))
        (values 1 -1))
      (check-equal? (values->list (g '(h e l l o)))
                    (list 1 -1))
      (check-exn exn:fail:contract? (thunk (g "hello"))))
    ;; (test-case
    ;;     "case->"
    ;;   (define/contract (g lst)
    ;;     (case->
    ;;      (function/c string? number?)
    ;;      (function/c list?))
    ;;     (case-lambda
    ;;       [() (list 1 2)]
    ;;       [(arg) 0]))
    ;;   (check-equal? (g "hello") 0)
    ;;   (check-equal? (g) (list 1 2))
    ;;   (check-exn exn:fail:contract? (thunk (g 1))))
    )

   (test-suite
    "self-map"
    (test-case
        "Basic"
      (define/contract (g n)
        (self-map/c natural-number/c)
        0)
      (check-equal? (g 5) 0)
      (check-exn exn:fail:contract? (thunk (g "hello"))))
    (test-case
        "Return value"
      (define/contract (g n)
        (self-map/c natural-number/c)
        "0")
      (check-exn exn:fail:contract? (thunk (g 5))))
    (test-case
        "any/c"
      (define/contract (g n)
        (self-map/c any/c)
        0)
      (check-equal? (g 5) 0)
      (check-equal? (g "hello") 0)))

   (test-suite
    "thunk/c"
    (test-case
        "Basic"
      (define/contract (num-thunk)
        (thunk/c natural-number/c)
        0)
      (check-equal? (num-thunk) 0)
      (define/contract (list-thunk)
        (thunk/c list?)
        0)
      (check-exn exn:fail:contract? (thunk (list-thunk))))
    (test-case
        "any"
      (define/contract (g)
        (thunk/c any)
        (values 0 1))
      (check-equal? (values->list (g))
                    (list 0 1))
      (check-exn exn:fail:contract? (thunk (g "hello"))))
    (test-case
        "values"
      (define/contract (g)
        (thunk/c (values positive? negative?))
        (values 1 -1))
      (check-equal? (values->list (g))
                    (list 1 -1))
      (check-exn exn:fail:contract? (thunk (g "hello")))))

   (test-suite
    "binary-function/c"
    (test-case
        "Basic"
      (define/contract (g a b)
        (binary-function/c positive? negative? negative?)
        (* a b))
      (check-equal? (g 2 -3) -6)
      (check-exn exn:fail:contract? (thunk (g -2 3))))
    (test-case
        "Return value"
      (define/contract (g a b)
        (binary-function/c positive? negative? negative?)
        5)
      (check-exn exn:fail:contract? (thunk (g 2 -3))))
    (test-case
        "All default contracts"
      (define/contract (g a b)
        binary-function/c
        (* a b))
      (check-equal? (g 2 -3) -6))
    (test-case
        "All default contracts - backwards compat"
      (define/contract (g a b)
        (binary-function/c)
        (* a b))
      (check-equal? (g 2 -3) -6)))

   (test-suite
    "variadic-function/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-function/c negative? positive?)
        5)
      (check-equal? (g -1 -2 -5) 5)
      (check-exn exn:fail:contract? (thunk (g -2 3))))
    (test-case
        "Return value"
      (define/contract (g . as)
        (variadic-function/c negative? positive?)
        -5)
      (check-exn exn:fail:contract? (thunk (g -1 -2 -5))))
    (test-case
        "All default contracts"
      (define/contract (g . as)
        variadic-function/c
        5)
      (check-equal? (g -1 -2 -5) 5)
      (check-equal? (g "hi" -2 -5) 5))
    (test-case
        "All default contracts - backwards compat"
      (define/contract (g . as)
        (variadic-function/c)
        5)
      (check-equal? (g -1 -2 -5) 5)
      (check-equal? (g "hi" -2 -5) 5))
    (test-case
        "Default output to input contract"
      (define/contract (g . as)
        (variadic-function/c negative?)
        -5)
      (check-equal? (g -1 -2 -5) -5)))

   (test-suite
    "binary-variadic-function/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-function/c number? list? number?)
        5)
      (check-equal? (g 3 (list 1 2) null) 5)
      (check-equal? (g 3) 5)
      (check-exn exn:fail:contract? (thunk (g (list 1 2) null))))
    (test-case
        "Variadic head"
      (define/contract (g . as)
        (variadic-function/c number? (tail list?) number?)
        5)
      (check-equal? (g 3 4 5 (list 1 2)) 5)
      (check-equal? (g (list 1 2)) 5)
      (check-exn exn:fail:contract? (thunk (g 5)))))

   (test-suite
    "predicate/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (predicate/c number?)
        #t)
      (check-true (g 5))
      (check-exn exn:fail:contract? (thunk (g null))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        predicate/c
        #t)
      (check-true (g 5))
      (check-true (g null))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Defaults with no parameters - backwards compat"
      (define/contract (g . as)
        (predicate/c)
        #t)
      (check-true (g 5))
      (check-true (g null))
      (check-exn exn:fail:contract? (thunk (g)))))

   (test-suite
    "binary-predicate/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (binary-predicate/c number? string?)
        #t)
      (check-true (g 5 "hi"))
      (check-exn exn:fail:contract? (thunk (g 5 5))))
    (test-case
        "Defaults with one parameter"
      (define/contract (g . as)
        (binary-predicate/c number?)
        #t)
      (check-true (g 5 5))
      (check-exn exn:fail:contract? (thunk (g 5)))
      (check-exn exn:fail:contract? (thunk (g 5 "hi"))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        binary-predicate/c
        #t)
      (check-true (g 5 "hi"))
      (check-exn exn:fail:contract? (thunk (g 5))))
    (test-case
        "Defaults with no parameters - backwards compat"
      (define/contract (g . as)
        (binary-predicate/c)
        #t)
      (check-true (g 5 "hi"))
      (check-exn exn:fail:contract? (thunk (g 5)))))

   (test-suite
    "variadic-predicate/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-predicate/c number?)
        #t)
      (check-true (g 5))
      (check-true (g 5 6))
      (check-true (g))
      (check-exn exn:fail:contract? (thunk (g "hi"))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        (variadic-predicate/c)
        #t)
      (check-true (g 5))
      (check-true (g "hi"))
      (check-true (g)))
    (test-case
        "Defaults with no parameters - backwards compat"
      (define/contract (g . as)
        (variadic-predicate/c)
        #t)
      (check-true (g 5))
      (check-true (g "hi"))
      (check-true (g))))

   (test-suite
    "binary-variadic-predicate/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-predicate/c number? string?)
        #t)
      (check-true (g 2 "hi" "bye"))
      (check-true (g 2))
      (check-exn exn:fail:contract? (thunk (g "hi")))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Variadic head"
      (define/contract (g . as)
        (variadic-predicate/c number? (tail list?))
        #t)
      (check-true (g 3 4 5 (list 1 2)))
      (check-true (g (list 1 2)))
      (check-exn exn:fail:contract? (thunk (g 5)))))

   (test-suite
    "encoder/c"
    (test-case
        "Basic"
      (define/contract (g v)
        (encoder/c number?)
        5)
      (check-equal? (g "hi") 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g)))))

   (test-suite
    "decoder/c"
    (test-case
        "Basic"
      (define/contract (g v)
        (decoder/c string?)
        5)
      (check-equal? (g "hi") 5)
      (check-exn exn:fail:contract? (thunk (g)))))

   (test-suite
    "hash-function/c"
    (test-case
        "Basic"
      (define/contract (g v)
        hash-function/c
        5)
      (check-equal? (g "hi") 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Basic - backwards compat"
      (define/contract (g v)
        (hash-function/c)
        5)
      (check-equal? (g "hi") 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g)))))

   (test-suite
    "maybe/c"
    (test-case
        "Basic"
      (check-true ((maybe/c string? zero?) "hi"))
      (check-true ((maybe/c string? zero?) 0))
      (check-false ((maybe/c string? zero?) 5)))
    (test-case
        "Default with no parameters"
      (check-true ((maybe/c number?) 5))
      (check-true ((maybe/c number?) #f))
      (check-false ((maybe/c number?) "hi"))))

   (test-suite
    "binary-composition/c"
    (test-case
        "Basic"
      (define/contract (g a b)
        (binary-composition/c number?)
        5)
      (check-equal? (g 1 2) 5)
      (check-exn exn:fail:contract? (thunk (g 1 "2")))
      (check-exn exn:fail:contract? (thunk (g 1)))
      (check-exn exn:fail:contract? (thunk (g "1" "2")))))

   (test-suite
    "variadic-composition/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-composition/c number?)
        5)
      (check-equal? (g 1 2) 5)
      (check-equal? (g 1) 5)
      (check-equal? (g) 5)
      (check-exn exn:fail:contract? (thunk (g "1")))
      (check-exn exn:fail:contract? (thunk (g 1 "2")))))

   (test-suite
    "binary-variadic-composition/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (binary-variadic-composition/c number?)
        5)
      (check-equal? (g 1 2) 5)
      (check-equal? (g 1 2 3) 5)
      (check-equal? (g 1) 5)
      (check-exn exn:fail:contract? (thunk (g)))))

   (test-suite
    "classifier/c"
    (test-case
        "Basic"
      (define/contract (g cls-f lst)
        (classifier/c number?)
        (when (not (empty? lst))
          (cls-f (first lst)))
        (list (list 1 2) (list 3)))
      (check-equal? (g (curryr remainder 3) (list 1 2 3)) (list (list 1 2) (list 3)))
      (check-exn exn:fail:contract? (thunk (g number->string (list 1 2 3))))
      (check-exn exn:fail:contract? (thunk (g (curryr remainder 3) 5))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g cls-f lst)
        (classifier/c)
        (when (not (empty? lst))
          (cls-f (first lst)))
        (list (list 1 2) (list 3)))
      (check-equal? (g (curryr remainder 3) (list 1 2 3)) (list (list 1 2) (list 3)))
      (check-equal? (g number->string (list 1 2 3)) (list (list 1 2) (list 3)))
      (check-exn exn:fail:contract? (thunk (g (curryr remainder 3) 5)))))

   (test-suite
    "map/c"
    (test-case
        "Basic"
      (define/contract (g mapf lst)
        (map/c string? number?)
        (when (not (empty? lst))
          (mapf (first lst)))
        (list 5))
      (check-equal? (g string->number (list "5")) (list 5))
      (check-equal? (g string->number null) (list 5))
      (check-exn exn:fail:contract? (thunk (g string->number 5)))
      (check-exn exn:fail:contract? (thunk (g add1 (list 5)))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g mapf lst)
        (map/c)
        (when (not (empty? lst))
          (mapf (first lst)))
        (list 5))
      (check-equal? (g string->number (list "5")) (list 5))
      (check-equal? (g add1 (list 5)) (list 5))
      (check-equal? (g add1 null) (list 5))
      (check-exn exn:fail:contract? (thunk (g string->number 5))))
    (test-case
        "Default output contract"
      (define/contract (g mapf lst)
        (map/c number?)
        (when (not (empty? lst))
          (mapf (first lst)))
        (list 5))
      (check-equal? (g add1 (list 5)) (list 5))
      (check-equal? (g add1 null) (list 5))
      (check-exn exn:fail:contract? (thunk (g number->string (list 5))))
      (check-exn exn:fail:contract? (thunk (g add1 5)))))

   (test-suite
    "filter/c"
    (test-case
        "Basic"
      (define/contract (g pred lst)
        (filter/c number?)
        (when (not (empty? lst))
          (pred (first lst)))
        (list 5))
      (check-equal? (g positive? (list 1 2 3)) (list 5))
      (check-exn exn:fail:contract? (thunk (g positive? (list "1" "2" "3")))))
    (test-case
        "Predicate"
      (define/contract (g pred lst)
        (filter/c number?)
        (pred "hi")
        (list 5))
      (check-exn exn:fail:contract? (thunk (g (λ (x) #t) (list 1 2 3)))))
    (test-case
        "Return value"
      (define/contract (g pred lst)
        (filter/c number?)
        (when (not (empty? lst))
          (pred (first lst)))
        (list "5"))
      (check-exn exn:fail:contract? (thunk (g (λ (x) #t) (list 1 2 3)))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g pred lst)
        (filter/c)
        (when (not (empty? lst))
          (pred (first lst)))
        (list 5))
      (check-equal? (g positive? (list 1 2 3)) (list 5))
      (check-equal? (g (λ (x) #t) (list "1" "2" "3")) (list 5))))

   (test-suite
    "reducer/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (reducer/c string? number?)
        5)
      (check-equal? (g (list "a" "b")) 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g (list 1 2))))
      (check-exn exn:fail:contract? (thunk (g 5))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        (reducer/c)
        5)
      (check-equal? (g (list "a" "b")) 5)
      (check-equal? (g (list 1 2)) 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g 5))))
    (test-case
        "Default output contract"
      (define/contract (g . as)
        (reducer/c number?)
        5)
      (check-equal? (g (list 1 2)) 5)
      (check-equal? (g null) 5)
      (check-exn exn:fail:contract? (thunk (g 5)))
      (check-exn exn:fail:contract? (thunk (g (list "a" "b"))))))

   (test-suite
    "functional/c"
    (test-case
        "Basic"
      (define/contract (g h)
        (functional/c procedure?)
        add1)
      (check-equal? (g sqrt) add1)
      (check-exn exn:fail:contract? (thunk (g 5))))
    (test-case
        "Return value"
      (define/contract (g h)
        (functional/c procedure?)
        5)
      (check-exn exn:fail:contract? (thunk (g add1))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g h)
        (functional/c)
        add1)
      (check-equal? (g sqrt) add1)
      (check-exn exn:fail:contract? (thunk (g 5)))))

   (test-suite
    "binary-constructor/c"
    (test-case
        "Basic"
      (define/contract (g a b)
        (binary-constructor/c number? list?)
        (list 5))
      (check-equal? (g 5 (list 3)) (list 5))
      (check-exn exn:fail:contract? (thunk (g 5)))
      (check-exn exn:fail:contract? (thunk (g 5 6))))
    (test-case
        "Return value"
      (define/contract (g a b)
        (binary-constructor/c number? list?)
        5)
      (check-exn exn:fail:contract? (thunk (g 5 (list 4)))))
    (test-case
        "bab"
      (define/contract (g a b)
        (binary-constructor/c number? list? #:order 'bab)
        (list 5))
      (check-equal? (g (list 3) 5) (list 5))
      (check-exn exn:fail:contract? (thunk (g (list 5))))
      (check-exn exn:fail:contract? (thunk (g (list 5) (list 6))))))

   (test-suite
    "variadic-constructor/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (variadic-constructor/c number? list?)
        (list 5))
      (check-equal? (g 1 (list 3)) (list 5))
      (check-equal? (g 1 2 3 (list 3)) (list 5))
      (check-equal? (g (list 3)) (list 5))
      (check-exn exn:fail:contract? (thunk (g (list 4) (list 2)))))
    (test-case
        "Return value"
      (define/contract (g . as)
        (variadic-constructor/c number? list?)
        5)
      (check-exn exn:fail:contract? (thunk (g 1 (list 3)))))
    (test-case
        "bab"
      (define/contract (g . as)
        (variadic-constructor/c number? list? #:order 'bab)
        (list 5))
      (check-equal? (g (list 3) 1) (list 5))
      (check-equal? (g (list 3) 1 2 3) (list 5))
      (check-equal? (g (list 3)) (list 5))
      (check-exn exn:fail:contract? (thunk (g (list 4) (list 2))))))))

(module+ test
  (just-do
   (run-tests tests)))
