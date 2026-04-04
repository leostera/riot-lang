# hello-foreign

An OCaml-to-Rust FFI smoke test for Riot.

`hello-foreign` is a tiny package, but it is a useful reference if you want to
see how Riot wires native code into an OCaml package. It links against the
Rust crate in `native/hello-rust`, exposes a couple of OCaml externals, and
ships a tiny binary that calls them.

## Install

```sh
riot add hello-foreign
```

## What it shows

- how to declare a `foreign-dependencies` section in `riot.toml`;
- how to point that dependency at a Rust crate and build it with Cargo;
- how to expose native functions through OCaml `external` declarations;
- how to call those bindings from normal Riot code.

## Example

```ocaml
open Hello_foreign

let doubled = Bindings.double 21
let plus_ten = Bindings.add_ten 21
```

You can run the included example with:

```sh
riot run -p hello-foreign ffi_demo
```

That example calls into the Rust library and prints the results.

## Package layout

- `src/bindings.ml` defines the OCaml externals.
- `src/hello.ml` is the original smoke-test binary.
- `examples/ffi_demo.ml` is the example that works with the current `riot run`
  example flow.
- `native/hello-rust` is the Rust implementation that gets linked in.

## When to use it

Use this package as a template or reference when you need to:

- call into Rust from OCaml;
- verify a local foreign-toolchain setup;
- understand how Riot expects native artifacts to be built and linked.

If you are looking for a general-purpose FFI framework, this package is not
that. It is intentionally small and opinionated.
