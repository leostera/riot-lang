# typ

Prototype type analysis for Riot.

`typ` is the current library-first prototype for Riot's future OCaml type
analysis and typechecking story. It is intentionally incomplete, but the shape
is already useful if you want to experiment with semantic queries, batch
analysis, or structured diagnostics over OCaml source.

## Install

```sh
riot add typ
```

## What the package is for

The package is organized around a few core ideas:

- `Session` owns logical sources and stable source identities;
- `Snapshot` represents one immutable analysis revision;
- `Query` exposes semantic queries over a snapshot;
- `Batch` and `Check` provide one-shot entry points for tools and tests.

Underneath that surface, `typ` currently lowers a subset of OCaml through
`syn`, records semantic structure explicitly, and reports diagnostics in a
structured way.

## Example

```ocaml
open Std

let source = {|
let id x = x
let answer = id 42
|}

let result = Typ.Batch.check_source ~filename:(Path.v "example.ml") source

let () = println (Typ.Report.render_report result)
```

A runnable example is included:

```sh
riot run -p typ check_source
```

## Good entry points

- `Typ.Session.empty`
- `Typ.Session.create_source`
- `Typ.Session.snapshot`
- `Typ.Query.diagnostics`
- `Typ.Query.type_at`
- `Typ.Query.export_of`
- `Typ.Batch.check_source`

## Current status

`typ` is still an experimental package. It is best thought of as an
architecture-in-motion for future Riot analysis tooling, not a finished
general-purpose typechecker.

That said, it is already a good package to read if you want to understand:

- how Riot wants to model semantic state explicitly;
- how source-backed diagnostics are represented;
- how future editor and compiler queries may be structured.
