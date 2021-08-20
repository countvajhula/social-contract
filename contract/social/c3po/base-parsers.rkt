#lang racket

(require megaparsack
         megaparsack/parser-tools/lex
         data/monad
         data/applicative)

(provide identifier/p
         literal/p
         keyword/p
         number/p
         form/p
         symex/p)

(define (identifier/p [name #f])
  (guard/p (token/p 'IDENTIFIER)
           (λ (result)
             (or (not name)
                 (eq? name result)))))

(define (literal/p [name #f])
  (guard/p (token/p 'LITERAL)
           (λ (result)
             (or (not name)
                 (eq? name result)))))

(define (keyword/p [name #f])
  (guard/p (token/p 'KEYWORD)
           (λ (result)
             (or (not name)
                 (eq? name result)))))

(define (number/p [n #f])
  (guard/p (literal/p)
           (λ (result)
             (and (number? result)
                  (or (not n)
                      (= n result))))))

(define form/p
  (do (token/p 'OPEN-PAREN)
      [a <- (many/p symex/p)]
    (token/p 'CLOSE-PAREN)
    (pure a)))

(define symex/p
  (or/p (try/p (literal/p))
        (try/p (identifier/p))
        (try/p (keyword/p))
        (try/p form/p)))
