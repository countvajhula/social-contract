#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[contract/social
                    (except-in racket = predicate/c)
                    (only-in relation =)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (only-in relation =)
                                           racket/list
                                           (except-in racket/contract
                                                      predicate/c)
                                           contract/social))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined contracts for commonly encountered types.

@defproc[(function/c [source/c contract? any/c]
                     [target/c contract? any/c])
         contract?]{

 A contract to recognize a simple function that maps values of type @racket[source/c] to values of type @racket[target/c] (i.e. a unary function).

@examples[
    #:eval eval-for-docs
    (define/contract (list-length lst)
      (function/c list? natural-number/c)
      (if (= lst null)
        0
        (add1 (list-length (rest lst)))))
    (list-length '(h e l l o))
    (eval:error (list-length "hello"))
  ]
}

@defproc[(self-map/c [type/c contract?])
         contract?]{

 A contract to recognize a @hyperlink["https://proofwiki.org/wiki/Definition:Self-Map"]{self-map}, i.e. a function that maps a value of type @racket[domain/c] to a value of the same type.

@examples[
    #:eval eval-for-docs
    (define/contract (double n)
      (self-map/c natural-number/c)
      (* n 2))
    (double 5)
    (eval:error (double "hello"))
  ]
}

@defproc[(thunk/c [target/c contract? any/c])
         contract?]{

 A contract to recognize a @hyperlink["https://beautifulracket.com/explainer/functions.html#a_alsKX"]{"thunk"}, i.e. a function taking no arguments, that returns a value of type @racket[target/c].

@examples[
    #:eval eval-for-docs
    (define/contract (hello-button)
      (thunk/c string?)
      "hello!")
    (hello-button)
    (eval:error (hello-button "friend"))
    (define/contract (hello-button)
      (thunk/c number?)
      "hello!")
    (eval:error (hello-button))
  ]
}

@defproc[(binary-function/c [a/c contract?]
                            [b/c contract?]
                            [target/c contract?])
         contract?]{}

@defproc[(variadic-function/c [source/c contract?]
                              [target/c contract?])
         contract?]{}

@defproc[(binary-variadic-function/c [a/c contract?]
                                     [b/c contract?]
                                     [target/c contract?])
         contract?]{}

@defproc[(predicate/c [on-type/c contract? any/c])
         contract?]{}

@defproc[(encoder/c [as-type/c contract?])
         contract?]{}

@defproc[(decoder/c [from-type/c contract?])
         contract?]{}

@defproc[(hash-function/c)
         contract?]{}

@defproc[(maybe/c [contract contract?]
                  [default/c contract? #f])
         contract?]{}

@defproc[(binary-composition/c [type/c contract?])
         contract?]{}

@defproc[(variadic-composition/c [type/c contract?])
         contract?]{}

@defproc[(reducer/c [type/c contract?])
         contract?]{}

@defproc[(functional/c [procedure/c contract? procedure?])
         contract?]{}

@defproc[(classifier/c [by-type/c contract? any/c])
         contract?]{}

@deftogether[(
@defproc[(binary-constructor-abb/c [primitive/c contract?]
                                   [composite/c contract?])
         contract?]
@defproc[(binary-constructor-bab/c [primitive/c contract?]
                                   [composite/c contract?])
         contract?]
@defproc[(binary-constructor/c [primitive/c contract?]
                               [composite/c contract?])
         contract?]
  )]{}

@deftogether[(
@defproc[(variadic-constructor-abb/c [primitive/c contract?]
                                     [composite/c contract?])
         contract?]
@defproc[(variadic-constructor-bab/c [primitive/c contract?]
                                     [composite/c contract?])
         contract?]
@defproc[(variadic-constructor/c [primitive/c contract?]
                                 [composite/c contract?])
         contract?]
  )]{}
