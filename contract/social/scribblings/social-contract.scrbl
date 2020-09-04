#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         pict/private/layout
         @for-label[contract/social
                    (except-in racket
                               map
                               filter
                               sequence?)
                    (only-in data/collection
                             map
                             filter
                             sequence?)
                    (only-in relation
                             fold
                             false.
                             ->list
                             /=
                             ..)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require racket/math
                                           (except-in data/collection
                                                      append
                                                      index-of
                                                      foldl
                                                      foldl/steps)
                                           relation
                                           contract/social
                                           racket/stream))))

@title{Social Contracts}
@author{Siddhartha Kasivajhula}

@defmodule[contract/social]

Collectively-defined contracts for commonly encountered types.
