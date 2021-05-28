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
    (test-case
        "any with single value"
      (define/contract (list-to-any lst)
        (function/c list? any)
        0)
      (check-equal? (list-to-any '(h e l l o)) 0)
      (check-exn exn:fail:contract? (thunk (list-to-any "hello"))))
    (test-case
        "any with multiple values"
      (define/contract (list-to-any lst)
        (function/c list? any)
        (values 0 1))
      (check-equal? (values->list (list-to-any '(h e l l o)))
                    (list 0 1))
      (check-exn exn:fail:contract? (thunk (list-to-any "hello")))))

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
      (check-equal? (as-b -1 -2 -5) -5)))))

(module+ test
  (just-do
   (run-tests tests)))
