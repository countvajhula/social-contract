#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[contract/social
                    (except-in racket predicate/c)
                    (only-in data/collection sequenceof)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require racket/list
                                           racket/function
                                           (except-in racket/contract
                                                      predicate/c)
                                           contract/social))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined contracts for commonly encountered types.

@deftogether[(
@defidform[function/c]
@defform[#:link-target? #f
         (function/c source/c target/c)]
)]{
 A contract to recognize a simple function that maps values of type @racket[source/c] to values of type @racket[target/c] (i.e. a unary function). If @racket[target/c] is expecting a value of the same type as @racket[source/c], prefer using @racket[self-map/c] instead. If left unspecified, @racket[source/c] and @racket[target/c] assume @racket[any/c], and in this case, this form must be used as an @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}, i.e. simply as @racket[function/c] rather than as @racket[(function/c)].

@racket[function/c] in general is equivalent to @racket[(-> source/c target/c)].

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

@defform[(self-map/c type/c)]{
 A contract to recognize a @hyperlink["https://proofwiki.org/wiki/Definition:Self-Map"]{self-map}, i.e. a function that maps a value of type @racket[type/c] to a value of the same type.

 @racket[self-map/c] is equivalent to @racket[(-> type/c type/c)].

@examples[
    #:eval eval-for-docs
    (define/contract (double n)
      (self-map/c natural-number/c)
      (* n 2))
    (double 5)
    (eval:error (double "hello"))
  ]
}

@deftogether[(
@defidform[thunk/c]
@defform[#:link-target? #f
         (thunk/c target/c)]
)]{
 A contract to recognize a @hyperlink["https://beautifulracket.com/explainer/functions.html#a_alsKX"]{"thunk"}, i.e. a function taking no arguments, which returns a value of type @racket[target/c]. If no target contract is specified (i.e. when it is used in identifier form as @racket[thunk/c]), the target defaults to @racket[any/c].

 @racket[thunk/c] in general is equivalent to @racket[(-> target/c)].

@examples[
    #:eval eval-for-docs
    (define/contract (hello-button)
      thunk/c
      "hello!")
    (hello-button)
    (eval:error (hello-button "friend"))
    (define/contract (hello-button)
      (thunk/c number?)
      "hello!")
    (eval:error (hello-button))
  ]
}

@defidform[functional/c]{
 A contract to recognize a function that accepts a function and returns another function. Equivalent to @racket[(self-map/c procedure?)] or @racket[(-> procedure? procedure?)].
}

@deftogether[(
@defidform[binary-function/c]
@defform[#:link-target? #f
         (binary-function/c source/c)]
@defform[#:link-target? #f
         (binary-function/c source/c target/c)]
@defform[#:link-target? #f
         (binary-function/c a/c b/c target/c)]
)]{
  A contract to recognize a @hyperlink["https://en.wikipedia.org/wiki/Binary_function"]{binary function}, that is, a function taking two arguments. The arguments are expected to be of type @racket[a/c] and @racket[b/c], respectively, and the return value is expected to be of type @racket[target/c]. When the @racket[(binary-function/c source/c target/c)] form is used, the first contract is used for both of the inputs, while the second is used for the output. If a contract is left unspecified, @racket[any/c] is assumed. If none of the contracts are to be specified, the contract should be used in identifier form simply as @racket[binary-function/c].

  @racket[binary-function/c] in general is equivalent to @racket[(-> a/c b/c target/c)].

  Where applicable, prefer a more specific contract like @racket[binary-predicate/c], @racket[binary-composition/c], or @racket[binary-constructor/c].

@examples[
    #:eval eval-for-docs
    (define/contract (my-add-as-string a b)
      (binary-function/c number? string?)
      (number->string (+ a b)))
    (my-add-as-string 3 5)
    (eval:error (my-add-as-string 5))
  ]
}

@deftogether[(
@defidform[variadic-function/c]
@defform[#:link-target? #f
         (variadic-function/c source/c)]
@defform[#:link-target? #f
         (variadic-function/c source/c target/c)]
@defform[#:link-target? #f
         (variadic-function/c a/c b/c target/c)]
@defform[#:link-target? #f
         (variadic-function/c a/c (tail b/c) target/c)]
)]{
  A contract to recognize a @hyperlink["https://beautifulracket.com/appendix/glossary.html#variadic"]{variadic} function, that is, a function taking an arbitrary number of arguments. The arguments are expected to all be of type @racket[source/c], and the return value is expected to be of type @racket[target/c]. If no contract is specified for the return value, it defaults to the input type. If more than one input contract is specified, then the contract expects the first argument to be of type @racket[a/c] and all subsequent arguments to be of type @racket[b/c], unless the second is provided in a @racket[(tail ...)] form. In that case, the contract expects the leading arguments to all be of type @racket[a/c] and the last argument to be of type @racket[b/c].

 @racket[variadic-function/c] in general is equivalent to @racket[(-> source/c ... target/c)].

  Where applicable, prefer a more specific contract like @racket[variadic-predicate/c], @racket[variadic-composition/c], or @racket[variadic-constructor/c].

@examples[
    #:eval eval-for-docs
    (define/contract (my-add-as-string . vs)
      (variadic-function/c number? string?)
      (number->string (apply + vs)))
    (my-add-as-string 3 5 7)
  ]
}

@deftogether[(
@defidform[predicate/c]
@defform[#:link-target? #f
         (predicate/c source/c)]
@defidform[binary-predicate/c]
@defform[#:link-target? #f (binary-predicate/c source/c)]
@defform[#:link-target? #f (binary-predicate/c a/c b/c)]
@defidform[variadic-predicate/c]
@defform[#:link-target? #f
         (variadic-predicate/c source/c)]
@defform[#:link-target? #f
         (variadic-predicate/c a/c b/c)]
@defform[#:link-target? #f
         (variadic-predicate/c a/c (tail b/c))]
)]{
  Similar to @racket[function/c], @racket[binary-function/c], and @racket[variadic-function/c], but these contracts recognize @hyperlink["https://en.wikipedia.org/wiki/Boolean-valued_function"]{predicates}, meaning that the output is expected to be of type @racket[boolean?].

  @racket[predicate/c] is equivalent to @racket[(-> source/c boolean?)], @racket[binary-predicate/c] is equivalent to @racket[(-> a/c b/c boolean?)], and @racket[variadic-predicate/c] is equivalent to @racket[(-> source/c ... boolean?)], with @racket[source/c], @racket[a/c] and @racket[b/c] defaulting to @racket[any/c] if left unspecified.
}

@deftogether[(
@defform[(encoder/c as-type/c)]
@defform[(decoder/c from-type/c)]
@defidform[hash-function/c]
)]{
  @racket[encoder/c] recognizes functions that map inputs of any type (i.e. @racket[any/c]) to a specific type, such as @racket[integer?]. @racket[decoder/c] recognizes functions that map inputs of a specific type, such as @racket[integer?], to an arbitrary output type (i.e. @racket[any/c]). @racket[hash-function/c] recognizes encoders that specifically map to the type @racket[fixnum?].

@racket[encoder/c] is equivalent to @racket[(-> any/c as-type/c)], @racket[decoder/c] is equivalent to @racket[(-> from-type/c any/c)], and @racket[hash-function/c] is equivalent to @racket[(-> any/c fixnum?)].
}

@deftogether[(
@defform[(maybe/c type/c)]
@defform[#:link-target? #f (maybe/c type/c default/c)]
)]{
 A contract to recognize values that may or may not exist. Equivalent to @racket[(or/c type/c default/c)], with @racket[default/c] defaulting to @racket[#f] if left unspecified.

@examples[
    #:eval eval-for-docs
    (define/contract v (maybe/c number?) 5)
    (define/contract v (maybe/c number?) #f)
    (eval:error (define/contract v (maybe/c number?) "5"))
  ]
}

@deftogether[(
@defform[(binary-composition/c type/c)]
@defform[(variadic-composition/c type/c)]
@defform[#:link-target? #f (variadic-composition/c type/c type/c)]
)]{
  Similar to @racket[binary-function/c] and @racket[variadic-function/c], but these contracts expect the input types and the output type to match, i.e. they recognize functions that @emph{compose} values of some type.

  @racket[binary-composition/c] is equivalent to @racket[(-> type/c type/c type/c)], and @racket[variadic-composition/c] is equivalent to @racket[(-> type/c ... type/c)].
}

@deftogether[(
@defform[(binary-constructor/c primitive/c composite/c)]
@defform[#:link-target? #f (binary-constructor/c #:order order primitive/c composite/c)]
@defform[(variadic-constructor/c primitive/c composite/c)]
@defform[#:link-target? #f (variadic-constructor/c #:order order primitive/c composite/c)]
)]{
  Similar to @racket[binary-function/c] and @racket[variadic-function/c], but these contracts are specialized to recognize @racket[cons]-style constructors that take primitive data and an instance of a rich data type and yield a fresh instance of the rich type including the primitive data. @racket[order] should be either @racket['abb] or @racket['bab], reflecting the intended order of the primitive and composite inputs.

  @racket[binary-constructor/c] is equivalent to @racket[(-> primitive/c composite/c composite/c)] or @racket[(-> composite/c primitive/c composite/c)], depending on the indicated @racket[order], and @racket[variadic-constructor/c] is equivalent to @racket[(-> primitive/c ... composite/c composite/c)] or @racket[(-> composite/c primitive/c ... composite/c)].

@examples[
    #:eval eval-for-docs
    (define/contract (my-cons elem lst)
      (binary-constructor/c any/c list?)
      (cons elem lst))
    (my-cons "apple" (list "banana" "cherry"))
	(eval:error (my-cons "apple" "banana"))
  ]
}

@deftogether[(
@defidform[classifier/c]
@defform[#:link-target? #f (classifier/c by-type/c)]
)]{
 A contract to recognize a function that classifies the elements of the input sequence into distinct classes based on some key function.

Equivalent to @racket[(binary-function/c (encoder/c by-type/c) sequence? (sequenceof sequence?))] or @racket[(-> (-> any/c by-type/c) sequence? (sequenceof sequence?))].

@examples[
    #:eval eval-for-docs
    (define/contract (alphabetize key lst)
      (classifier/c char?)
      (group-by key lst))
    (alphabetize (curryr string-ref 0) (list "apple" "banana" "apricot" "cherry" "blackberry"))
	(eval:error (alphabetize string-upcase (list "apple" "banana" "apricot" "cherry" "blackberry")))
  ]
}

@deftogether[(
@defidform[map/c]
@defform[#:link-target? #f (map/c source/c)]
@defform[#:link-target? #f (map/c source/c target/c)]
)]{
 A contract to recognize a function that maps a function over a sequence of values. The input sequence is expected to contain values of type @racket[source/c] and the mapping function is expected to be of type @racket[(-> source/c target/c)], so that the result of the contractually bound function is expected to be of type @racket[(sequenceof target/c)]. @racket[source/c] and @racket[target/c] are assumed to be @racket[any/c] if neither is specified, and the same if only one is specified.

 Equivalent to @racket[(binary-function/c (function/c source/c target/c) (sequenceof source/c) (sequenceof target/c))] or @racket[(-> (-> source/c target/c) (sequenceof source/c) (sequenceof target/c))].

@examples[
    #:eval eval-for-docs
    (define/contract (stringify-numbers fn lst)
      (map/c number? string?)
      (map fn lst))
    (stringify-numbers number->string (list 1 2 3 4))
	(eval:error (stringify-numbers number->string (list "1" "2" "3" "4")))
  ]
}

@deftogether[(
@defidform[filter/c]
@defform[#:link-target? #f (filter/c type/c)]
)]{
 A contract to recognize a function that filters a sequence using a predicate. The input sequence is expected to contain values of type @racket[type/c]. The predicate is expected to be on @racket[type/c] values as well, and the output is expected to be of the same type as the input. @racket[type/c] is assumed to be @racket[any/c] if left unspecified.

 Equivalent to @racket[(binary-function/c (predicate/c type/c) (sequenceof type/c) (sequenceof type/c))] or @racket[(-> (-> type/c boolean?) (sequenceof type/c) (sequenceof type/c))].

@examples[
    #:eval eval-for-docs
    (define/contract (filter-numbers pred lst)
      (filter/c number?)
      (filter pred lst))
    (filter-numbers positive? (list -1 2 3 -4 5))
	(eval:error (filter-numbers positive? (list "1" "2" "3" "4")))
  ]
}

@deftogether[(
@defidform[reducer/c]
@defform[#:link-target? #f (reducer/c type/c)]
@defform[#:link-target? #f (reducer/c type/c target/c)]
)]{
  A contract to recognize functions that consume a sequence and produce a value. The sequence is expected to contain values of type @racket[type/c], and the result is expected to be of type @racket[target/c].

 If @racket[target/c] is not specified, it is assumed to be @racket[type/c].

 @racket[reducer/c] is equivalent to @racket[(function/c (sequenceof type/c) target/c)] or @racket[(-> (sequenceof type/c) target/c)].

@examples[
    #:eval eval-for-docs
    (define/contract (my-sum lst)
      (reducer/c number?)
      (apply + lst))
    (my-sum (list 1 2 3 4))
	(eval:error (my-sum (list "1" "2" "3" "4")))
  ]
}
