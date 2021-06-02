#lang racket/base

(require rackunit
         rackunit/text-ui
         (except-in racket/contract
                    predicate/c)
         (only-in racket/list
                  rest)
         contract/social
         (only-in racket/function thunk)
         (only-in adjutor values->list)
         "private/util.rkt")

(define tests
  (test-suite
   "Tests for social contracts"

   (test-suite
    "Unary function"
    (test-case
        "Basic"
      (define/contract (list-to-num lst)
        (function/c list? natural-number/c)
        0)
      (check-equal? (list-to-num '(h e l l o)) 0)
      (check-exn exn:fail:contract? (thunk (list-to-num "hello"))))
    ;; (test-case
    ;;     "any with single value"
    ;;   (define/contract (list-to-any lst)
    ;;     (function/c list? any)
    ;;     0)
    ;;   (check-equal? (list-to-any '(h e l l o)) 0)
    ;;   (check-exn exn:fail:contract? (thunk (list-to-any "hello"))))
    ;; (test-case
    ;;     "any with multiple values"
    ;;   (define/contract (list-to-any lst)
    ;;     (function/c list? any)
    ;;     (values 0 1))
    ;;   (check-equal? (values->list (list-to-any '(h e l l o)))
    ;;                 (list 0 1))
    ;;   (check-exn exn:fail:contract? (thunk (list-to-any "hello"))))
    )

   (test-suite
    "self-map"
    (test-case
        "Basic"
      (define/contract (num-to-num n)
        (self-map/c natural-number/c)
        0)
      (check-equal? (num-to-num 5) 0)
      (check-exn exn:fail:contract? (thunk (num-to-num "hello"))))
    (test-case
        "any/c"
      (define/contract (any-to-any n)
        (self-map/c any/c)
        0)
      (check-equal? (any-to-any 5) 0)
      (check-equal? (any-to-any "hello") 0)))

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
      (check-exn exn:fail:contract? (thunk (list-thunk)))))

   (test-suite
    "binary-function/c"
    (test-case
        "Basic"
      (define/contract (a-b-c a b)
        (binary-function/c positive? negative? negative?)
        (* a b))
      (check-equal? (a-b-c 2 -3) -6)
      (define/contract (a-b-c2 a b)
        (binary-function/c positive? negative? negative?)
        (* a b))
      (check-exn exn:fail:contract? (thunk (a-b-c2 -2 3))))
    (test-case
        "All default contracts"
      (define/contract (a-b-c a b)
        (binary-function/c)
        (* a b))
      (check-equal? (a-b-c 2 -3) -6))
    (test-case
        "Default to first input contract"
      (define/contract (a-b-c a b)
        (binary-function/c positive?)
        (* a b))
      (check-equal? (a-b-c 2 3) 6)
      (check-exn exn:fail:contract? (thunk (a-b-c -2 3))))
    (test-case
        "Default output to first input contracts"
      (define/contract (a-b-c a b)
        (binary-function/c negative? positive?)
        (* a b))
      (check-equal? (a-b-c -3 2) -6)
      (check-exn exn:fail:contract? (thunk (a-b-c 3 -2)))))

   (test-suite
    "variadic-function/c"
    (test-case
        "Basic"
      (define/contract (as-b . as)
        (variadic-function/c negative? positive?)
        5)
      (check-equal? (as-b -1 -2 -5) 5)
      (check-exn exn:fail:contract? (thunk (as-b -2 3))))
    (test-case
        "All default contracts"
      (define/contract (as-b . as)
        (variadic-function/c)
        5)
      (check-equal? (as-b -1 -2 -5) 5)
      (check-equal? (as-b "hi" -2 -5) 5))
    (test-case
        "Default output to input contract"
      (define/contract (as-b . as)
        (variadic-function/c negative?)
        -5)
      (check-equal? (as-b -1 -2 -5) -5)))

   (test-suite
    "binary-variadic-function/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (binary-variadic-function/c number? list? number?)
        5)
      (check-equal? (g 3 (list 1 2) null) 5)
      (check-equal? (g 3) 5)
      (check-exn exn:fail:contract? (thunk (g (list 1 2) null))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        (binary-variadic-function/c)
        5)
      (check-equal? (g 2 3) 5)
      (check-equal? (g 1) 5)
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Default variadic contract"
      (define/contract (g . as)
        (binary-variadic-function/c number?)
        5)
      (check-equal? (g 4 5 6) 5)
      (check-exn exn:fail:contract? (thunk (g (list 1 2)))))
    (test-case
        "Default output contract"
      (define/contract (g . as)
        (binary-variadic-function/c number? list?)
        5)
      (check-equal? (g 3 (list 1 2) null) 5)
      (check-exn exn:fail:contract? (thunk (g 3 5))))
    (test-case
        "Variadic head"
      (define/contract (g . as)
        (binary-variadic-function/c number? list? #:tail? #t)
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
      (check-true (g))))

   (test-suite
    "binary-variadic-predicate/c"
    (test-case
        "Basic"
      (define/contract (g . as)
        (binary-variadic-predicate/c number? string?)
        #t)
      (check-true (g 2 "hi" "bye"))
      (check-true (g 2))
      (check-exn exn:fail:contract? (thunk (g "hi")))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Defaults with no parameters"
      (define/contract (g . as)
        (binary-variadic-predicate/c)
        #t)
      (check-true (g 2 3 5))
      (check-true (g 2 "hi" 5))
      (check-true (g 2))
      (check-exn exn:fail:contract? (thunk (g))))
    (test-case
        "Default with one parameter"
      (define/contract (g . as)
        (binary-variadic-predicate/c number?)
        #t)
      ;;... and tail
      (check-true (g 5))
      (check-true (g 5 6 7))
      (check-exn exn:fail:contract? (thunk (g 5 "hi")))
      (check-exn exn:fail:contract? (thunk (g)))))

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
      (check-false ((maybe/c number?) "hi"))))))

(module+ test
  (just-do
   (run-tests tests)))
