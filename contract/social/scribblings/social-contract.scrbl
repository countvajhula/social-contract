#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[contract/social
                    contract/social/c3po
                    (except-in racket predicate/c sequence? empty?)
                    racket/undefined
                    (only-in data/collection sequenceof sequence? empty?)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require (except-in racket/list empty?)
                                           (only-in data/collection sequence? empty?)
                                           racket/function
                                           racket/undefined
                                           (except-in racket/contract
                                                      predicate/c)
                                           contract/social
                                           contract/social/c3po))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined, composable syntax to describe @seclink["contract-boundaries" #:doc '(lib "scribblings/guide/guide.scrbl")]{contracts} at a high level.

@section{Introduction}

Racket's @seclink["contracts" #:doc '(lib "scribblings/reference/reference.scrbl")]{contract DSL} is a flexible way to describe arbitrary data types. In practice, such contracts often exhibit hierarchical structure that can be understood in terms of abstractions that correspond to standard notions or concepts that are broadly useful -- for instance, involving the arity of the function, or the relationship between its inputs and outputs. But the contract DSL does not provide a way to explicitly encode such abstractions, necessitating that contract authors spell out the details of each contract in terms of primitives such as @racket[->] and @racket[or/c]. In this sense, it is a @emph{low-level language} for describing the contract, analogous to describing the operation of addition in terms of @hyperlink["https://en.wikipedia.org/wiki/Combinational_logic"]{combinational logic}.

The present module provides a curated list of composable forms which describe contracts in terms of their @emph{phrase structure}, that is, in terms of hierarchical abstractions that correspond to commonly encountered high-level data types, contracts whose definitions are collectively agreed upon. This agreement may take the form of collaborative development and discussion on the @hyperlink["https://github.com/countvajhula/social-contract/issues"]{source repository}, to agree on, for instance, the most useful form and variations of a @racketlink[predicate/c]{predicate} contract, or, at least, the agreement may simply be tacit in the sense that these contracts correspond to ideas that are common and widely known.

As an example, when you take two values of the same type and produce another value of the same type, this is an instance of @emph{composition}. Instead of specifying the contract for such a function using the contract DSL (e.g. @racket[(-> integer? integer? integer?)]) where the idea of composition cannot be encoded, we simply use the appropriate high-level composition contract (e.g. @racket[(binary-composition/c integer?)]). This helps both the writer as well as the reader of the code, since for the former it may reveal something that they hadn't already realized in thinking about the function merely as one which takes two integers and produces another -- that the function they wrote is a composing function -- and for the latter, it saves them the trouble of parsing the contract specification to understand that, indeed, this is a function that composes two values.

As the forms compose, complex contracts exhibiting phrase structure have compact representations. For instance, @racket[(-> (-> integer? integer?) (-> integer? integer?) (-> integer? integer?))] may be expressed as @racket[(binary-composition/c (self-map/c integer?))].

@section{How Do I Migrate My Existing Contracts?}

For contracts that you've already written (e.g. in a module's @racket[provide] specification using @racket[contract-out]), one way to migrate them is to just do so manually, inspecting each one, understanding the high level idea involved if it is a common one, and then selecting the appropriate social contract to use in its place. This is a good exercise and would help you see the high level ideas encoded in your function specifications.

Another way, which is a complement to the manual approach and especially useful if you are doing this for a large number of contracts and projects, is to use the @racket[contract/social/c3po] module which is a "social protocol assistant" parser. You could think of it as a shiny golden droid that, while helpful, doesn't always say the right things and requires you to make the final decisions. See @secref{c3po} for more on how to use it to help you migrate your contracts.

@section{What if I Don't See the Contract I Need?}

If the appropriate contract does not exist and you believe that the data you are attempting to describe is relatively general or common (e.g. you've needed this contract more than once, and suspect that others might, as well), consider @hyperlink["https://github.com/countvajhula/social-contract/issues"]{bringing it up} for possible addition. With enough support or motivation, it will be added.

@section{Contracts}

@deftogether[(
@defidform[function/c]
@defform[#:link-target? #f
         (function/c source/c ... target/c)]
)]{
A contract to recognize any function. This is the most general function contract and should be a last resort, to be employed only if a more specific contract is not available or appropriate. For instance, if the concerned function is a unary function and @racket[target/c] is expecting a value of the same type as @racket[source/c], prefer using @racket[self-map/c] instead. Likewise, each of the various function contracts in this module exploits some non-trivial relationship between the contracts specifying the function, and depending on the case, it may be appropriate to use a @racketlink[predicate/c]{predicate} contract, or an @racketlink[operation/c]{operation}, or @racketlink[composition/c]{composition}, and so on.

If used as an @tech[#:doc '(lib "scribblings/guide/guide.scrbl")]{identifier macro}, @racket[function/c] means a unary function with input and output of type @racket[any/c].

@racket[function/c] in general is equivalent to @racket[->].

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
@defform[(parametrized-self-map/c arg/c ... type/c)]
@defform[#:link-target? #f (parametrized-self-map/c #:order order arg/c ... type/c)]
)]{
 Similar to @racket[self-map/c] but accepts any (but a specific) number of additional arguments. The number and the types of these additional arguments are indicated by supplying the @racket[arg/c]'s. In the case of a single additional argument, this contract is equivalent to @racket[binary-constructor/c], but in such cases this contract should be favored over that one if the function doesn't actually entail a notion of "construction."

  As an example, when there are two additional arguments, @racket[parametrized-self-map/c] is equivalent to @racket[(-> arg1/c arg2/c type/c type/c)] or @racket[(-> type/c arg1/c arg2/c type/c)], depending on the indicated @racket[order].

@examples[
    #:eval eval-for-docs
    (define/contract (prefix n str)
      (parametrized-self-map/c natural-number/c string?)
      (substring str 0 n))
    (prefix 3 "apple")
    (eval:error (prefix 3 (list 1 2 3 4 5)))
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
 A contract to recognize a function that accepts a function and returns another function. Not to be confused with @racket[function/c], "functional" is a term used to refer to @hyperlink["https://en.wikipedia.org/wiki/Higher-order_function"]{higher-order functions}.

 Equivalent to @racket[(self-map/c procedure?)] or @racket[(-> procedure? procedure?)].
}

@deftogether[(
@defidform[binary-function/c]
@defform[#:link-target? #f
         (binary-function/c a/c b/c target/c)]
)]{
  A contract to recognize a @hyperlink["https://en.wikipedia.org/wiki/Binary_function"]{binary function}, that is, a function taking two arguments. The arguments are expected to be of type @racket[a/c] and @racket[b/c], respectively, and the return value is expected to be of type @racket[target/c]. If the contract is used in identifier form simply as @racket[binary-function/c], then the inputs and outputs default to @racket[any/c].

  @racket[binary-function/c] in general is equivalent to @racket[(-> a/c b/c target/c)].

  Where applicable, prefer a more specific contract like @racket[binary-predicate/c], @racket[binary-operation/c], @racket[binary-composition/c], or @racket[binary-constructor/c].

@examples[
    #:eval eval-for-docs
    (define/contract (my-add-as-string a b)
      (binary-function/c number? number? string?)
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
@defform[(operation/c n source/c target/c)]
@defform[#:link-target? #f
         (operation/c n source/c)]
@defform[(binary-operation/c source/c target/c)]
@defform[#:link-target? #f
         (binary-operation/c source/c)]
)]{
  @racket[operation/c] is a contract to recognize an @racket[n]-ary @hyperlink["https://en.wikipedia.org/wiki/Homogeneous_relation"]{homogeneous} @hyperlink["https://en.wikipedia.org/wiki/Operation_(mathematics)"]{operation}, that is, a function taking @racket[n] arguments where each argument is of the same type. The inputs are expected to be of type @racket[source/c], and the return value is expected to be of type @racket[target/c]. When the @racket[(operator/c n source/c)] form is used, the output uses @racket[any/c]. @racket[operation/c] expects a @emph{specific} number of inputs. To recognize an arbitrary number of inputs, use @racket[variadic-function/c] instead.

 @racket[binary-operation/c] recognizes an operation of arity @racket[2]. Note that the term "binary operation" isn't used consistently in mathematical literature, where it sometimes refers specifically to a @emph{closed} binary operation where the output is of the same type as the input. For this more specific case, use @racket[binary-composition/c] instead.

  As an example, @racket[(operation/c 3 source/c target/c)] is equivalent to @racket[(-> source/c source/c source/c target/c)], while @racket[(binary-operation/c source/c target/c)] is equivalent to @racket[(-> source/c source/c target/c)].

@examples[
    #:eval eval-for-docs
    (define/contract (my-add-as-string a b)
      (binary-operation/c number? string?)
      (number->string (+ a b)))
    (my-add-as-string 3 5)
    (eval:error (my-add-as-string 5))
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

@defform[(lift/c pure/c functor/c)]{
 A contract to recognize a function that "lifts" a value of type @racket[pure/c] into a container of type @racket[functor/c]. Typically @racket[functor/c] would be a parametric sequence type such as @racket[listof] or @racket[sequenceof].

 Equivalent to @racket[(-> pure/c (functor/c pure/c))].

@examples[
    #:eval eval-for-docs
    (define/contract (my-range n)
      (lift/c number? listof)
      (range n))
    (my-range 5)
    (eval:error (my-range "5"))
  ]
}

@deftogether[(
@defform[(maybe/c type/c)]
@defform[#:link-target? #f (maybe/c type/c default/c)]
)]{
 A contract to recognize values that may or may not exist. This contract is intended to be used in cases where the value, if missing, is a sentinel value indicating absence, such as @racket[#f], @racket[undefined], or @racket[(void)]. In cases where the value may simply be one or another type, use @racket[or/c] instead.

 Equivalent to @racket[(or/c type/c default/c)], with @racket[default/c] defaulting to @racket[#f] if left unspecified.

@examples[
    #:eval eval-for-docs
    (define/contract v (maybe/c number?) 5)
    (define/contract v (maybe/c number?) #f)
    (define/contract v (maybe/c number? void?) (void))
    (eval:error (define/contract v (maybe/c number?) "5"))
  ]
}

@defform[(nonempty/c type/c)]{
 A contract to recognize nonempty sequences of type @racket[type/c]. Equivalent to @racket[(and/c type/c (not/c empty?))].

@examples[
    #:eval eval-for-docs
    (define/contract v (nonempty/c list?) (list 1 2 3))
    (define/contract v (nonempty/c string?) "hello")
    (define/contract v (nonempty/c sequence?) "hello")
    (eval:error (define/contract v (nonempty/c string?) ""))
    (eval:error (define/contract v (nonempty/c list?) (list)))
  ]
}

@deftogether[(
@defform[(composition/c n type/c)]
@defform[(binary-composition/c type/c)]
@defform[(variadic-composition/c type/c)]
@defform[#:link-target? #f (variadic-composition/c type/c type/c)]
)]{
  Similar to @racket[operation/c], @racket[binary-function/c] and @racket[variadic-function/c], but these contracts expect the input types and the output type to match, i.e. they recognize functions that @emph{compose} values of some type.

  @racket[binary-composition/c] is equivalent to @racket[(-> type/c type/c type/c)]; @racket[variadic-composition/c] is equivalent to @racket[(-> type/c ... type/c)]; and for instance, @racket[(composition/c 3 type/c)] is equivalent to @racket[(-> type/c type/c type/c type/c)].
}

@deftogether[(
@defform[(binary-constructor/c primitive/c composite/c)]
@defform[#:link-target? #f (binary-constructor/c #:order order primitive/c composite/c)]
@defform[(variadic-constructor/c primitive/c composite/c)]
@defform[#:link-target? #f (variadic-constructor/c #:order order primitive/c composite/c)]
)]{
  Similar to @racket[binary-function/c] and @racket[variadic-function/c], but these contracts are specialized to recognize @racket[cons]-style constructors that take primitive data and an instance of a rich data type and yield a fresh instance of the rich type incorporating the primitive data. @racket[order] should be either @racket['abb] or @racket['bab], reflecting the intended order of the primitive and composite inputs. Note that the @racket[order] parameter controls the order in which the @emph{contracted function} expects arguments, not the present forms. That is, regardless of the order specified using @racket[order], the first argument to these contract forms is always @racket[primitive/c], and the second argument is always @racket[composite/c].

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

@section[#:tag "c3po"]{"C3PO": Contract Migration Assistant}

@defmodule[contract/social/c3po]

C3PO is a "reverse compiler" that can help you migrate your contracts to social contracts. It accepts contracts, either individually or as an entire @racket[provide] form, and translates the input so it is rephrased in terms of high-level social contracts. It can even accept social contracts you've already written and translate them into more minimal representations, so it could potentially be incorporated into a general-purpose linter for contracts.

To use it, simply @racket[translate] a contract or @racket[provide] specification (provided verbatim without quoting) in order to "reverse-compile" it into high-level social contracts.

@defform[(translate ctc)]{
 "Reverse compile" the contract @racket[ctc] as a social contract specification.

@racketblock[
    (translate (-> integer? integer? integer?))
    (translate (-> any/c number?))
    (translate (-> string? any/c))
    (translate (-> (-> integer? integer? integer?) (-> integer? integer? integer?)))
    (translate (provide address occupation (contract-out [name (-> any/c string?)])))
]
}

@subsection{Limitations}

You should use C3PO as an assistant and not defer to it blindly, due to the following limitations.

@subsubsection{Correct Vs Appropriate Contracts}

Low-level contract specifications cannot in general be uniquely mapped to high level contract representations, and in some cases a matching high-level contract may coincidentally have the same signature but not actually describe the data in question. For instance, a function that takes a number and a list and returns a list has the signature of a constructing function, yet, this particular function may be using the input number as an index of some kind to extract a sublist rather than incorporating it into the resulting list as a constructor typically would. We may prefer to think of this as a @racketlink[binary-function/c]{binary function} rather than as a @racketlink[binary-constructor/c]{binary constructor}.

@subsubsection{Not All Contract Forms Supported}

At the moment, C3PO supports the @racket[->] and @racket[->*] contract forms, but not @racket[->i]. If @racket[->i] is encountered during parsing, it would just leave this form unchanged in the output (while translating the rest of it).
