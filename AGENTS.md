# Prompt for working on this repository

## CRITICAL RULES

0. Even tho you will write documents scripts and other temporary files, you will always show me the plans before writing them
0. All your planning documents go into ./llm/plans/xxx-plan-name.md
0. All your temporary scripts go into ./llm/scripts/*.sh
0. All your dummy/test source files go into ./llm/sources/*.{ml/js/mli/rs/etc}

0. NEVER EVER EDIT FILES WITH SCRIPTS <- This is an unforgivable offense.

1. ALWAYS TRUST TUSK AND ITS OUTPUTS
2. IF TRUST SAYS ITS CACHED, THEN IT IS CACHED. PERIOD. NEVER TRY TO FORCE A CACHE BREAK
3. ALWAYS USE Std FROM ./packages/std
4. ALWAYS `open Std` AT THE TOP
5. When using sublibraries of Std, always `open Std.SubLib` like if you need Iterator when do `open Std.Iter` to have Iterator available
6. Prefer abstract types in interfaces
7. Tusk operates from the root of the workspace, so it doesn't matter where you `cd` into, `tusk <cmd>` always runs from the root
8. When calling binaries, it is useful to use `timeout T <cmd>` to make sure they don't hang infinitely 
9. When writing tests, aggressively call Option.expect and Result.expect instead of gracefully handling None/Error's -- focus on the happy path
10. ALWAYS USE `tusk completions --binaries/tests/packages` to see what binaries tests and packages we have

0. NEVER EDIT FILES WITH AWK OR SED OR PYTHON OR BASH OR PERL -- Only edit files with the Edit tool
1. NEVER DISABLE TESTS UNLESS TOLD TO
1. NEVER USE OCAMLC DIRECTLY
1. NEVER USE OPAM
1. NEVER USE DUNE
1. NEVER USE OCAMLDOC SYNTAX
1. NEVER USE Stdlib/Unix/Sys -> ALWAYS USE Std from ./packages/std
1. NEVER USE Obj.t or Obj.magic or any Obj.* function
1. NEVER USE `ref` ON VALUE -> ALWAYS USE Std.Cell
1. NEVER USE `ref` ON RECORDS -> ALWAYS USE `mutable field`

## OCAML STYLE GUIDE

### Code Organization Pattern

When writing complex modules with state and loops, use this pattern (see coordinator.ml as reference):

1. **Define types at the top**
   - Public result/output types first
   - Message types for actor communication
   - Internal state types

2. **Helper functions using `let rec` and `and`**
   - Use mutually recursive functions with `let rec ... and ...`
   - Extract message handlers into separate functions (e.g., `handle_worker_ready`, `handle_task_completed`)
   - Keep the main loop clean by delegating to handlers

3. **Main loop with pattern matching**
   - Use a selector function for message filtering
   - Pattern match on selected messages
   - Tail-recursive loop calls at the end of each branch

4. **Initialize and run pattern**
   - `init` function: creates state and starts the loop
   - State is immutable record with mutable cells where needed
   - Loop function immediately called from init with state

Example structure:
```ocaml
type state = { field : Type.t; mutable_field : Type.t Cell.t }

let rec helper_function state = ...

and loop state =
  let selector msg = ... in
  if termination_condition then handle_done state
  else match receive ~selector () with
  | `Case1 x -> handle_case1 state x
  | `Case2 y -> handle_case2 state y

and handle_case1 state x =
  (* do work *)
  loop state

and handle_done state = ()

let init ~params =
  let state = { ... } in
  loop state
```

### Key Style Points

- Use `let open Module in` for scoped opens in small sections
- Inline simple pattern matches in log/status messages
- Use `Vector.push`/`Vector.pop` for mutable collections (not list append)
- Prefer `match` over nested `if` for clarity
- Name handlers descriptively: `handle_X` not `on_X` or `process_X`
- Keep functions tail-recursive by putting recursive call last 
