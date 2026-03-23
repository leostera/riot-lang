# tusk-fix

`tusk-fix` is Riot's linting and safe-fix pipeline for OCaml code.

It is built on top of `syn`:

- Ceibo provides exact lossless syntax and spans
- `Syn.Cst` provides typed structure for clean parses
- `tusk-fix-api` provides the shared rule-authoring surface

## Current model

The pipeline is:

1. parse each file once with `syn`
2. keep parse diagnostics separate from lint diagnostics
3. run enabled rules against the red tree plus optional typed CST
4. apply only safe, package-owned fixes
5. report remaining issues in text or JSON

Rules can be built in or provided by workspace packages. Package-provided rules
are fused into a generated runtime under `_build`, so `tusk-fix` does not spawn
one subprocess per rule.

## Rule ids vs diagnostic codes

These are distinct concepts:

- a **rule id** identifies the checker, for example `riot:snake-case-type-names`
- a **diagnostic code** identifies a concrete finding, for example `f0101` or
  `std:f0001`

Many rules emit exactly one diagnostic code, but the CLI keeps the surfaces
separate on purpose:

- `--list-rules` lists rules
- `--list-diagnostics` lists diagnostic codes
- `--explain CODE` explains a diagnostic code

Explain text lives with the rule definition, not in a central registry package.

## CLI

Typical usage:

```sh
# check a path without modifying files
tusk run tusk -- fix --check packages/syn

# stop early after surfacing a small number of diagnostics
tusk run tusk -- fix --check --limit 10 packages/syn

# apply safe fixes
tusk run tusk -- fix packages/tusk-fix

# inspect the currently loaded rule and diagnostic surfaces
tusk run tusk -- fix --list-rules
tusk run tusk -- fix --list-diagnostics

# explain a diagnostic code
tusk run tusk -- fix --explain F0101
tusk run tusk -- fix --explain std:f0001
```

## Rule authoring

Rules are defined with `tusk-fix-api` and run against:

- the file path
- the optional typed CST for clean parses
- the Ceibo red tree for exact source traversal

At a high level:

```ocaml
let make () =
  Rule.make
    ~id:"snake-case-type-names"
    ~code:"F0101"
    ~name:"Snake-case Type Names"
    ~description:"Type names should use snake_case instead of camelCase"
    ~explain:"Prefer snake_case type names so declarations read consistently."
    ~run:(fun context red_root ->
      ignore context;
      ignore red_root;
      [])
    ()
```

The important constraints are:

- keep rules small and specific
- prefer typed `Syn.Cst` structure when the grammar has already been lifted
- push structural parser assumptions down into `syn` instead of re-encoding
  grammar in every rule
- only emit fixes that are clearly safe

## Configuration

Rules are enabled and disabled through `tusk.toml`. Built-in Riot rules are
enabled by default unless they are explicitly disabled in config.

Package-provided rules keep their package-qualified ids, for example:

- `riot:snake-case-type-names`
- `riot:descriptive-type-variables`
- `std:no-stdlib`

## Validation

The usual checks are:

```sh
timeout 30 tusk build tusk-fix
timeout 180 tusk test tusk-fix:runner_tests
tusk run tusk -- fix --list-rules
tusk run tusk -- fix --list-diagnostics
tusk run tusk -- fix --check --limit 10 packages/tusk-fix
```
