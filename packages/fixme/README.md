# fixme

Rule-authoring types for `riot-fix`.

`fixme` is the package you use when you want to define lint rules, diagnostics,
explanations, and safe auto-fixes that plug into Riot's fixing pipeline. It is
the stable vocabulary layer below `riot-fix`.

## Install

```sh
riot add fixme
```

## Use it when

- you are writing custom fix or lint rules;
- you need to describe diagnostics and safe edits in a reusable way;
- you want rule tests that speak the same data model as `riot-fix`.

## What you get

- rule definitions and traversal helpers;
- diagnostic and explanation types;
- fix descriptions and source-runner helpers;
- rule test support for verifying behavior.

## Where to start

- `src/rule.mli` and `src/diagnostic.mli` define the core authoring surface.
- `src/rule_test.mli` is the fastest way to understand how to test a rule.
- `riot-fix` is the consumer package that actually runs these rules.
