# krasny

Riot's OCaml formatter.

Most users will interact with `krasny` through `riot fmt`, but the package is
also usable as a library when you want to parse OCaml source, render it into
Riot's canonical layout, or inspect formatter failures programmatically.

## Install

```sh
riot add krasny
```

## When to use `krasny`

Use `krasny` when you want:

- Riot's canonical formatting rules in-process;
- a formatter that works from `syn`'s typed CST instead of replaying raw text;
- explicit failure when the source cannot be lifted cleanly enough to format.

That last point matters: `krasny` does not try to pretty-print broken files.
If parsing or CST construction fails, formatting fails too.

## Example

```ocaml
open Std

let source = "let  add  x   y= x+y" in
let parsed = Syn.parse ~filename:(Path.v "sample.ml") source in

match Krasny.format parsed with
| Ok formatted -> println formatted
| Error err -> eprintln (Krasny.format_error_to_string err)
```

A runnable example is included:

```sh
riot run -p krasny format_source
```

## Library surface

The package is intentionally small:

- `Krasny.format` formats a parsed file to a string.
- `Krasny.write` streams formatted output into a writer.
- `Krasny.syntax_hash` computes a normalized syntax hash.
- `Krasny.Runner` and `Krasny.Report` power file-oriented formatting flows.

## Related packages

- `syn` provides the lossless lexer/parser and typed CST that `krasny` renders.
- `riot fmt` is the normal end-user entry point if you just want to format a
  project.
