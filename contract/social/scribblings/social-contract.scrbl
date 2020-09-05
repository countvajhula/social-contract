#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[contract/social
                    racket/contract
                    (except-in racket =)
                    (only-in relation =)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (only-in relation =)
                                           racket/list
                                           racket/contract
                                           contract/social))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined contracts for commonly encountered types.

@defproc[(function/c [source/c contract?]
                     [target/c contract?])
         contract?]{

 A contract to recognize a simple function that maps values of type @racket[source/c] to values of type @racket[target/c].

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

@defproc[(binary-function/c [a/c contract?]
                            [b/c contract?]
                            [target/c contract?])
         contract?]{}

@defproc[(variadic-function/c [source/c contract?]
                              [target/c contract?])
         contract?]{}

@defproc[(encoder/c [as-type/c contract?])
         contract?]{}

@defproc[(decoder/c [from-type/c contract?])
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

@defproc[(self-map/c [type/c contract?])
         contract?]{}

@defproc[(functional/c [procedure/c contract? procedure?])
         contract?]{}

@defproc[(binary-constructor/c [primitive/c contract??]
                               [composite/c contract??])
         contract?]{}

@defproc[(variadic-constructor/c [primitive/c contract??]
                                 [composite/c contract??])
         contract?]{}

@defproc[(variadic-comparison-predicate/c [type/c contract??])
         contract?]{}

@defproc[(variadic-selection-predicate/c [type/c contract??])
         contract?]{}
