foreign-ocaml
=============

Haskell Foreign Function Interface (FFI) for OCaml.
Allows integrating OCaml code into Haskell programs: right now the interface is still pretty rough, and there are probably terrible memory holes that we need to deal with, consider it as a proof of concept.

It offers:

* calling OCaml functions from Haskell;
* serialization and deserialization of OCaml datatypes, including `unit`, `bool`, `int`, `double`, `string`, `list`s, `tuple`s and `option`s;
* limited automatic suppport for serializing algebraic data types, works when there are no type variables (helper functions are provided for converting between the two representations, so custom serializations can be built)
* strict and lazy evaluation, support for side effects

Current limitations:

* does not support passing higher order functions to OCaml (Haskell function serialization is not supported)
* tuple serialization limited to tuples of 2 to 5 arguments
* no handling of garbage collection, so your program might explode

Installation
------------

To install the OCaml-FFI module simply run:

``cabal install``

Usage
-----

To learn how to use this library take a look at the `example` folder.
There are three files:
- `test.ml` a OCaml file exporting some functions and an algebraic data type for trees
- `Main.hs` a Haskell file registering and using those functions, including an implementation of serialization/deserialization for trees
- `Makefile` the makefile, with commented instructions on how to build your final executable
