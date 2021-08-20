#lang racket

(provide all-equal?)

(define (~rotate lst)
  ;; rotate list by 1
  (if (empty? lst)
      lst
      (append (rest lst) (list (first lst)))))

(define (all-equal? lst)
  (andmap equal? lst (~rotate lst)))
