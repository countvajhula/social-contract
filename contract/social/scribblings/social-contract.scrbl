#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[contract/social
                    (except-in racket predicate/c)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require racket/list
                                           (except-in racket/contract
                                                      predicate/c)
                                           contract/social))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined contracts for commonly encountered types.

@defproc*[([(function/c)
            contract?]
           [(function/c [source/c contract?]
                        [target/c contract?])
            contract?])]{

 A contract to recognize a simple function that maps values of type @racket[source/c] to values of type @racket[target/c] (i.e. a unary function). If @racket[target/c] is expecting a value of the same type as @racket[source/c], prefer using @racket[self-map/c] instead. If left unspecified, @racket[source/c] and @racket[target/c] assume @racket[any/c].

@examples[
    #:eval eval-for-docs
    (define/contract (list-length lst)
      (function/c list? natural-number/c)
      (if (empty? lst)
        0
        (add1 (list-length (rest lst)))))
    (list-length '(h e l l o))
    (eval:error (list-length "hello"))
  ]
}

@defproc[(self-map/c [type/c contract?])
         contract?]{

 A contract to recognize a @hyperlink["https://proofwiki.org/wiki/Definition:Self-Map"]{self-map}, i.e. a function that maps a value of type @racket[type/c] to a value of the same type.

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

 A contract to recognize a @hyperlink["https://beautifulracket.com/explainer/functions.html#a_alsKX"]{"thunk"}, i.e. a function taking no arguments, which returns a value of type @racket[target/c].

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

@defproc[(binary-function/c [a/c contract? any/c]
                            [b/c contract? #f]
                            [target/c contract? #f])
         contract?]{
  A contract to recognize a @hyperlink["https://en.wikipedia.org/wiki/Binary_function"]{binary function}, that is, a function taking two arguments. The arguments are expected to be of type @racket[a/c] and @racket[b/c], respectively, and the return value is expected to be of type @racket[target/c].
}

@defproc[(variadic-function/c [source/c contract? any/c]
                              [target/c contract? #f])
         contract?]{
  A contract to recognize a @hyperlink["https://beautifulracket.com/appendix/glossary.html#variadic"]{variadic} function, that is, a function taking an arbitrary number of arguments. The arguments are expected to all be of type @racket[source/c], and the return value is expected to be of type @racket[target/c]. If no contract is specified for the return value, it defaults to the input type.
}

@defproc[(binary-variadic-function/c [a/c contract? any/c]
                                     [b/c contract? #f]
                                     [target/c contract? #f]
                                     [#:tail? tail? boolean? #f])
         contract?]{
  Similar to @racket[variadic-function/c] but allows specification of two input types. If @racket[tail?] is false, then the contract expects the first argument to be of type @racket[a/c] and all subsequent arguments to be of type @racket[b/c]. If @racket[tail?] is true, then the contract expects the leading arguments to all be of type @racket[a/c] and the last argument to be of type @racket[b/c]. The return value is expected to be of type @racket[target/c].

}

@deftogether[(
@defproc[(predicate/c [on-type/c contract? any/c])
         contract?]
@defproc[(binary-predicate/c [a/c contract? any/c]
                             [b/c contract? #f])
         contract?]
@defproc[(variadic-predicate/c [on-type/c contract? any/c])
         contract?]
@defproc[(binary-variadic-predicate/c [a/c contract? any/c]
                                      [b/c contract? #f]
                                      [#:tail? tail? boolean? #f])
         contract?]
)]{
  Similar to @racket[function/c], @racket[binary-function/c], @racket[variadic-function/c] and @racket[binary-variadic-function/c], but these contracts recognize @hyperlink["https://en.wikipedia.org/wiki/Boolean-valued_function"]{predicates}, meaning that the output is expected to be of type @racket[boolean?].
}

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

@defproc[(binary-variadic-composition/c [type/c contract?])
         contract?]{}

@defproc[(reducer/c [type/c contract? any/c]
                    [target/c contract? #f])
         contract?]{}

@defproc[(functional/c [procedure/c contract? procedure?])
         contract?]{}

@defproc[(classifier/c [by-type/c contract? any/c])
         contract?]{}

@defproc[(map/c [source/c contract? any/c]
                [target/c contract? #f])
         contract?]{}

@defproc[(filter/c [of-type/c contract? any/c])
         contract?]{}

@defproc[(binary-constructor/c [primitive/c contract?]
                               [composite/c contract?]
                               [#:order order (one-of/c 'abb 'bab) 'abb])
         contract?]{}

@defproc[(variadic-constructor/c [primitive/c contract?]
                                 [composite/c contract?]
                                 [#:order order (one-of/c 'abb 'bab) 'abb])
         contract?]{}
