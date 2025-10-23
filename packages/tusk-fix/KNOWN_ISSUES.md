# Known Issues

## Inaccurate spans on larger files

**Symptom**: Code snippets point to wrong lines/columns in files with lots of whitespace

**Root cause**: The syn parser discards whitespace tokens (see `consume_trivia` in `parser.ml`). The Green tree doesn't contain whitespace, so when the Red tree calculates byte offsets by summing child widths, it misses all whitespace bytes. This causes positions to drift on larger files.

**Example**:
```ocaml
(* File with lots of blank lines and indentation *)
let x = 1


let y = Unix.getenv "HOME"  (* Position will be off by ~number of whitespace bytes *)
```

**Workaround**: The linter works correctly on small files and test cases. For production use, syn needs to be fixed to either:
1. Include whitespace tokens in the Green tree, OR
2. Store absolute byte offsets in Green tokens (not just width), OR  
3. Have Red tree track cumulative offset independently

**Status**: This is a syn parser bug, not a tusk_fix bug. Tracked in syn documentation.

## Only detects `open` statements

**Symptom**: Module path usage like `Unix.getenv` and type constructors like `Queue.t` are not detected

**Root cause**: The rule only checks `OPEN_STMT` nodes. Need to add handlers for `PATH_EXPR`, `FIELD_ACCESS_EXPR`, and `TYPE_CONSTR`.

**Status**: Implementation in progress. See test cases 0002 and 0003 which document the expected behavior once implemented.
