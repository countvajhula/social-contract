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
        'operation/c
        'binary-operation/c
        'predicate/c
        'binary-predicate/c
        'variadic-predicate/c
        'encoder/c
        'decoder/c
        'lift/c
        'hash-function/c
        'maybe/c
        'nonempty/c
        'composition/c
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
        'thunk/c '(function/c)
        'binary-function/c '(function/c)
        'variadic-function/c '(function/c)
        'operation/c '(function/c)
        'binary-operation/c '(binary-function/c operation/c)
        'maybe/c null
        'nonempty/c null
        'composition/c '(operation/c)
        'binary-composition/c '(binary-operation/c)
        'binary-constructor/c '(self-map/c)
        'binary-predicate/c '(binary-function/c)
        'classifier/c '(lift/c encoder/c)
        'decoder/c '(function/c)
        'encoder/c '(function/c)
        'lift/c '(function/c)
        'filter/c '(predicate/c self-map/c)
        'functional/c '(self-map/c)
        'hash-function/c '(encoder/c)
        'map/c '(function/c binary-function/c)
        'predicate/c '(function/c)
        'reducer/c '(function/c)
        'self-map/c '(function/c)
        'variadic-composition/c '(variadic-function/c)
        'variadic-constructor/c '(variadic-function/c)
        'variadic-predicate/c '(variadic-function/c)))

(define (form-name form)
  (cond [(eq? 'define-syntax-parser (first form))
         (second form)]
        [(eq? 'define-syntax-parse-rule (first form))
         (first (second form))]))

;; A helper to save us the trouble of visually parsing
;; a contract form to spot its dependencies.
;; Just pass the quoted form to get its
;; dependencies (assuming `nodes` above is up to date)
;; which can be entered into `ctc-graph`
(define (form-dependencies form)
  (let ([name (form-name form)]
        [references (flatten form)])
    (filter (λ (v) (not (or (void? v)
                            (eq? name v))))
            (for/list ([node nodes])
              (when (index-of references node)
                node)))))

;; Handle a whole list of quoted forms at once
;; returning a hash of form-name : dependencies
(define (dependencies forms)
  (for/hash ([form forms])
    (values (form-name form)
            (form-dependencies form))))

(define neighbors
  (dict->procedure #:failure (const empty)
                   ctc-graph))

(topological-sort nodes neighbors)
