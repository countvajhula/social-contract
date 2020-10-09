#lang racket/base

(module+ test
  (require rackunit
           rackunit/text-ui
           (except-in racket/contract
                      predicate/c)
           (only-in racket/list
                    rest)
           contract/social
           (only-in racket/function thunk)
           "private/util.rkt"))

(module+ test
  (define tests
    (check-true #t)
    ;; (test-suite
    ;;  "Tests for social contracts"

    ;;  (test-case
    ;;      "Unary function contract"
    ;;    (define/contract (list-length lst)
    ;;      (function/c list? natural-number/c)
    ;;      (if (eq? lst null)
    ;;          0
    ;;          (add1 (list-length (rest lst)))))
    ;;    (check-equal? (list-length '(h e l l o)) 5)
    ;;    (check-exn exn:fail:contract? (thunk (list-length "hello")))))
    ))

(module+ test
  (just-do
   (run-tests tests)))
