# kernel

Low-level runtime primitives for Riot.

`kernel` is the package below `std`. It collects the small, sharp building
blocks that the rest of the stack depends on: primitive data helpers, path and
environment handling, file descriptors, domain helpers, runtime effects, and
other "close to the metal" utilities.

## Should you use it directly?

Usually, no.

Most application code should depend on `std`, which re-exports the safer and
more opinionated surface Riot expects people to use day to day.

Reach for `kernel` directly when:

- you are bootstrapping a low-level library;
- you need primitives that `std` deliberately wraps or re-exports;
- you are working on Riot runtime internals or portability layers.

## Install

```sh
riot add kernel
```

## What is inside

- primitive helpers such as `Int`, `Int32`, `Int64`, `Float`, `Bool`, and
  `Option`;
- runtime-facing modules such as `Effect`, `Domain`, `Fd`, and `Env`;
- path, formatting, and exception helpers used throughout the rest of Riot;
- the foundational surface higher-level packages build on.

## Where to start

- `src/kernel.mli` is the package entrypoint.
- `examples/hello.ml` is the smallest place to see it in action.
- If you are looking for the "normal" Riot standard library, read `packages/std`
  instead.

## Related packages

- `std` builds the everyday application-facing library on top of `kernel`.
- `actors` and `suri` rely on `kernel` for lower-level runtime behavior.
