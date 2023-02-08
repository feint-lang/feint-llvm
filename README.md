# Feint Targeting LLVM

This is an attempt to implement Feint as a compiler targeting LLVM with
the eventual goal of implementing Feint in itself.

## Implementation Plan

The first implementation will be in OCaml since it has nice tools for
generating lexers and parsers. Plus, it's fun learning new languages.

The lexer will be implemented using `ocamllex` and the parser will be
implemented using `menhir`.

The parser will produce an AST, which will be visited to produce LLVM
IR.
