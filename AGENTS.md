# Prompt for working on this repository

## CRITICAL RULES

1. ALWAYS TRUST TUSK AND ITS OUTPUTS
2. IF TRUST SAYS ITS CACHED, THEN IT IS CACHED. PERIOD. NEVER TRY TO FORCE A CACHE BREAK
3. ALWAYS USE Std FROM ./packages/std
4. ALWAYS `open Std` AT THE TOP
5. When using sublibraries of Std, always `open Std.SubLib` like if you need Iterator when do `open Std.Iter` to have Iterator available
6. Prefer abstract types in interfaces
7. Tusk operates from the root of the workspace, so it doesn't matter where you `cd` into, `tusk <cmd>` always runs from the root
8. When calling binaries, it is useful to use `timeout T <cmd>` to make sure they don't hang infinitely 
9. When writing tests, aggressively call Option.expect and Result.expect instead of gracefully handling None/Error's -- focus on the happy path

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
