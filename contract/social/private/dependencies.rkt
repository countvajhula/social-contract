#lang racket

(require mischief/sort
         mischief/dict
         racket/function)

#|
Enumerate dependencies for each form on other forms (by inspecting the
macro definitions in contract/social), and use that to compute a
topological ordering that roughly corresponds to the phrase structure
of the forms. This informs the level at which to express the parsers
for each form in the c3po reverse compiler.

If new social contracts are added, the first step to updating c3po is
to add the dependencies here to recompute the topological order, and
then write the parsers for the new contract in terms of contracts that
appear before it in the topological ordering.

Note that this is a rough guide and not definitive, since there are
cases where certain contracts may appear before others in the phases
of the reverse compiler, but which don't actually precede them in the
phrase structure as indicated by the dependencies. This is because
sometimes a form may structurally be considered a phrase component of
another form without actually being conceptually a component of it. So
we do not choose to express the latter in terms of the former in
contract/social, but it may nevertheless happen during reverse
compilation (e.g. filter/c and binary-constructor/c), and we would
need to handle that in the parser for the new form.
|#

(define nodes
  (list 'function/c
        'thunk/c
        'self-map/c
        'binary-function/c
        'variadic-function/c
        'predicate/c
        'binary-predicate/c
        'variadic-predicate/c
        'encoder/c
        'decoder/c
        'lift/c
        'hash-function/c
        'maybe/c
        'nonempty/c
        'binary-composition/c
        'variadic-composition/c
        'classifier/c
        'map/c
        'filter/c
        'reducer/c
        'functional/c
        'binary-constructor/c
        'variadic-constructor/c))

(define ctc-graph
  (hash 'function/c null
        'thunk/c null
        'binary-function/c null
        'variadic-function/c null
        'maybe/c null
        'nonempty/c null
        'binary-composition/c '(binary-function/c)
        'binary-constructor/c '(binary-function/c)
        'binary-predicate/c '(binary-function/c)
        'classifier/c '(encoder/c binary-function/c)
        'decoder/c '(function/c)
        'encoder/c '(function/c)
        'lift/c '(function/c)
        'filter/c '(predicate/c binary-function/c)
        'functional/c '(self-map/c)
        'hash-function/c '(encoder/c)
        'map/c '(function/c binary-function/c)
        'predicate/c '(function/c)
        'reducer/c '(function/c)
        'self-map/c '(function/c)
        'variadic-composition/c '(variadic-function/c)
        'variadic-constructor/c '(variadic-function/c)
        'variadic-predicate/c '(variadic-function/c)))

(define neighbors
  (dict->procedure #:failure (const empty)
                   ctc-graph))

(topological-sort nodes neighbors)
