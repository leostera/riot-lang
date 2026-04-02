# riot-fix

`riot-fix` is Riot's linting and safe-fix pipeline for OCaml code.

It is built on top of `syn`:

- Ceibo provides exact lossless syntax and spans
- `Syn.Cst` provides typed structure for clean parses
- `fixme` provides the shared rule-authoring surface

## Current model

The pipeline is:

1. parse each file once with `syn`
2. keep parse diagnostics separate from lint diagnostics
3. run enabled rules against the red tree plus optional typed CST
4. apply only safe, package-owned fixes
5. report remaining issues in text or JSON

Rules can be built in or provided by workspace packages. Package-provided rules
are compiled into a generated `fixme-runner` under `_build`, so `riot-fix` does not spawn
one subprocess per rule.

## Rule ids

Rules are keyed by rule id:

- built-in rules are shown as `riot:<id>`
- package rules keep their package-qualified ids such as `std:no-stdlib`

The CLI exposes that same surface consistently:

- `--list-rules` lists rules
- `--list-diagnostics` lists the currently loaded diagnostics
- `--explain RULE_ID` explains a rule

Explain text lives with the rule definition, not in a central registry package.

## CLI

Typical usage:

```sh
# check a path without modifying files
riot run riot -- fix --check packages/syn

# stop early after surfacing a small number of diagnostics
riot run riot -- fix --check --limit 10 packages/syn

# apply safe fixes
riot run riot -- fix --apply packages/riot-fix

# inspect the currently loaded rule and diagnostic surfaces
riot run riot -- fix --list-rules
riot run riot -- fix --list-diagnostics

# explain a rule
riot run riot -- fix --explain riot:snake-case-type-names
riot run riot -- fix --explain std:no-stdlib
```

## Rule authoring

Rules are defined with `fixme` and run against:

- the file path
- the optional typed CST for clean parses
- the Ceibo red tree for exact source traversal

At a high level:

```ocaml
let make () =
  Rule.make
    ~id:"snake-case-type-names"
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

Rules are enabled and disabled through `riot.toml`. Built-in Riot rules are
enabled by default unless they are explicitly disabled in config.

Package-provided rules keep their package-qualified ids, for example:

- `riot:snake-case-type-names`
- `riot:descriptive-type-variables`
- `std:no-stdlib`

## Validation

The usual checks are:

```sh
timeout 30 riot build riot-fix
timeout 180 riot test riot-fix:runner_tests
riot run riot -- fix --list-rules
riot run riot -- fix --list-diagnostics
riot run riot -- fix --check --limit 10 packages/riot-fix
```
