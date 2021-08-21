#lang racket

(provide all-equal?
         leading-run-length)

(define (~rotate lst)
  ;; rotate list by 1
  (if (empty? lst)
      lst
      (append (rest lst) (list (first lst)))))

(define (all-equal? lst)
  (andmap equal? lst (~rotate lst)))

(define (take-while pred lst)
  (match lst
    ['() null]
    [(cons v vs)
     (if (pred v)
         (cons v (take-while pred vs))
         null)]))

(define (leading-run-length lst)
  (if (empty? lst)
      0
      (length (take-while (curry equal? (first lst)) lst))))
