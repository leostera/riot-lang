# ceibo

Generic red-green syntax trees for lossless and incremental parsers.

`ceibo` is the syntax-tree substrate behind Riot's parsing tools. It gives you
the same general shape used in systems like Roslyn and rust-analyzer:
immutable, shareable green trees plus a lazily materialized red layer for
position-aware traversal.

## When to use it

Use `ceibo` when you are building parser tooling and you need at least one of
these properties:

- lossless trees that preserve every byte of source input;
- incremental re-use of unchanged subtrees across edits;
- cheap position-aware traversal over immutable syntax;
- a generic tree model that is not tied to one grammar.

If you want a ready-made OCaml parser, start with `syn`. `ceibo` is the layer
below that.

## Install

```sh
riot add ceibo
```

## What you get

- green tree builders and node/token representations;
- red tree traversal with offsets and parent links;
- spans and source-range helpers for syntax tooling;
- a generic model you can adapt to many grammars, not just OCaml.

## Where to start

- `src/README.md` contains the deeper architecture write-up.
- `src/green.mli` and `src/red.mli` explain the two layers directly.
- `syn` is the best real package to study if you want to see `ceibo` used in a
  larger parser.
