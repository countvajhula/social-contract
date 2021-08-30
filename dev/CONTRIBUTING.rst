Adding a New Contract
=====================

1. Add the contract in ``contract/social.rkt`` and write tests for it in ``tests/contract/social/social-contract.rkt``

2. Add docs for the contract in ``contract/social/scribblings/social-contract.scrbl``

3. Add the contract to the list of ``nodes`` of the dependencies graph in ``contract/social/private/dependencies.rkt``, and also as a key in ``ctc-graph``. For every contract used in the new contract's definition (in ``contract/social.rkt``), add a corresponding entry in the values in ``ctc-graph`` for the new contract key.

4. Execute ``dependencies.rkt`` to get the fresh topological ordering of contracts -- this will help you modify the C3PO parser to handle the new contract correctly.

5. Add tests in ``contract/social/c3po.rkt`` to verify that the new contract can be parsed by C3PO from a representation in terms of Racket's built-in contract DSL. These tests should fail initially since we haven't written the parser yet.

6. Implement a parser for the new contract in ``contract/social/c3po/contract-parser.rkt``. The parser would typically be expressed in terms of its dependencies, i.e. in terms of contracts that appear before it in the output from step 4. See the other contracts in the file as examples.

7. Add the parser to the ``contract/p`` parser. Where you place it doesn't matter in relation to parsers that have more, fewer, or orthogonal dependencies to the new parser. But for parsers with the same dependencies, place more specific parsers earlier, and less specific ones later. Specific in this sense: if parser A would accept inputs p and q while parser B would accept only p, then parser B is the more specific one. Alternatively, for every input p that is acceptable to both A and B, if you would prefer B to be the interpretation, then place B earlier. This ensures that the new parser doesn't need to handle cases framed in terms of peers, only predecessors.

8. Run the tests for C3PO and verify that the new tests pass.

9. Are there any specific instances of higher-level contracts (i.e. those that appear near or after the new contract in the topological ordering) that ought to be simplified to this new contract? If so, add tests for such simplifications in ``contract/social/c3po.rkt`` (you'll find other tests for simplifications, as examples).

10. Implement parsers for the new contract for the special cases in #8.

11. Re-run tests and ensure they pass.

Dev Workflow
============

1. Build source

.. code-block:: bash

  make build

2. Run tests

.. code-block:: bash

  make test

Docs Workflow
=============

3. Build docs

.. code-block:: bash

  make build-docs

4. View docs

.. code-block:: bash

  make docs

Release Workflow (Steps for Maintainer)
=======================================

5. Build package, docs, and check dependencies

.. code-block:: bash

  make build-all

6. Check dependencies

.. code-block:: bash

  make check-deps

7. When you're ready to cut a new release, bump the version in info.rkt and make a fresh commit

.. code-block:: racket

  (define version "i.j.k") ; numbers corresponding to major.minor.patch

8. Tag the release commit

.. code-block:: bash

  git tag -n<NUM>  # list existing tags and annotations; if specified, NUM configures verbosity
  git tag -a <new version number> -m "<release message>"  # or leave out -m to enter it in Vim

9. Push the changes including the new tag to origin

.. code-block:: bash

  git push --follow-tags  # push new tag to remote
