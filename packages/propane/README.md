# propane

Property-based testing for Riot.

`propane` is Riot's QuickCheck-style testing package. Instead of hand-writing a
small fixed set of examples, you describe properties that should hold for a
wide range of generated inputs. `propane` then generates test cases, runs the
property many times, and shrinks failures down to a smaller counter-example
when it can.

## Install

```sh
riot add propane
```

## Core pieces

- `Generator` builds random values.
- `Arbitrary` combines generation, shrinking, and printing.
- `Property` defines runnable properties.
- `Shrinker` reduces failing values to smaller counter-examples.
- `Printer` renders generated values for reports.

## Example

```ocaml
open Std
open Propane

let list_rev_prop =
  property
    "list reverse is involutive"
    Arbitrary.(list int)
    (fun lst -> List.reverse (List.reverse lst) = lst)

let tests = [ list_rev_prop ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"propane-demo" ~tests ~args)
    ~args:Env.args
    ()
```

The package already ships several runnable examples under `examples/`. A quick
way to make sure they build is:

```sh
riot build propane
```

To inspect one of the example binaries directly, start with:

```sh
riot run -p propane basic
```

## When it is a good fit

Use `propane` when:

- the space of interesting inputs is too large for hand-written unit tests;
- you care about discovering surprising edge cases automatically;
- you want shrinking and repeatable property checks in the same toolchain as
  `Std.Test`.

## Related packages

- `std` provides the test runner integration through `Std.Test`.
