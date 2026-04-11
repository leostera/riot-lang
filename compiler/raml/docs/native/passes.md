# Raml Native Passes

This document records the native passes that exist today in
`compiler/raml-native/src`.

The important design rule is simple:

- there is no generic pass runner
- there is no reified pass abstraction that drives compilation
- each stage builds its pipeline with plain function composition

Today that means code shaped like:

```ocaml
let normalize = Passes.Normalize.program initial in
let simplify = Passes.Simplify.program normalize in
...
```

Trace metadata exists only for fixture snapshots and diagnostics. It is not a
compiler framework.

## `NIR`

`NIR` is the first native-only IR. It is still expression-shaped, but it has
already committed to native runtime concerns such as helper calls, closure
materialization, imports, and top-level entry shaping.

### `Normalize`

File:
- [normalize.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/nir/passes/normalize.ml)

`NIR.Normalize` lifts nested `let` structure out of call positions and
conditional conditions, flattens adjacent `let` chains, and generally pushes
the tree toward a more ANF-like shape. It does not try to change runtime
meaning. It exists so later native passes can reason about one regular binding
shape instead of a pile of incidental nesting.

### `Simplify`

File:
- [simplify.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/nir/passes/simplify.ml)

`NIR.Simplify` trims obviously redundant expression structure before
instruction selection. It folds literal-boolean `if` expressions, removes dead
pure `let` bindings, collapses `[let x = expr in x]`, and drops pure entry-side
`Eval` items. The effect is intentionally conservative: keep the native runtime
shape, but stop carrying syntax that every later stage would just delete again.

## `MIR`

`MIR` is the first pseudo-instruction layer. It still has structured
conditionals, but it is close enough to machine code that analysis and local
optimization start to matter.

### `Canonicalize`

File:
- [canonicalize.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/canonicalize.ml)

`MIR.Canonicalize` cleans up the structured instruction tree before the heavier
MIR analyses run. It recursively rewrites conditionals, drops no-op moves, and
folds constant, empty, or identical branches. That leaves later MIR passes with
less structural noise and a more uniform tree to analyze.

### `Insert_polls`

File:
- [insert_polls.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/insert_polls.ml)

`MIR.Insert_polls` walks the structured MIR tree and inserts an explicit
`raml_poll` call before each non-poll call, including inside conditional
branches. That makes polling an ordinary IR obligation instead of an implicit
emitter trick, so snapshots and later passes can reason about it directly.

### `Cse`

File:
- [cse.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/cse.ml)

`MIR.Cse` is a deliberately conservative first step toward real common
subexpression elimination. It only tracks repeated pure materializations of
literals and symbol addresses, and rewrites later duplicates into copies from
the first destination register. It does not pretend calls or global loads are
pure. The payoff is modest but honest: repeated setup work becomes cheap copies
that later passes can shrink further.

### `Copy_propagate`

File:
- [copy_propagate.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/copy_propagate.ml)

`MIR.Copy_propagate` tracks cheap copied values in a forward environment and
rewrites later uses to point at the cheaper source operand when it is safe to
do so. It handles register aliases, literals, and symbol addresses, and only
keeps branch knowledge when both sides agree. The point is to expose
administrative traffic so the next cleanup pass can delete it.

### `Dead_code`

File:
- [dead_code.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/dead_code.ml)

`MIR.Dead_code` walks each procedure backwards using `Mir.Liveness` and drops
work whose result is no longer live and whose execution is pure. Calls are kept
even when their destination is dead, but the dead destination itself is erased.
This is the pass that strips the leftover temporary traffic before
linearization.

Supporting analysis:
- [liveness.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/liveness.ml)

This is analysis, not a pass. It exists so multiple MIR passes can share the
same local liveness computation without reimplementing it.

## `LIR`

`LIR` is the flat pre-emission IR. At this point control flow is already
linearized into labels and branches.

### `Layout_frames`

File:
- [layout_frames.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/passes/layout_frames.ml)

`LIR.Layout_frames` computes explicit frame metadata for each procedure. It
uses `Frame_analysis` to decide whether the procedure needs a frame and whether
it performs calls, assigns stable stack-slot offsets, and records an aligned
frame size. That keeps stack layout in a snapshotable pass instead of letting
the emitter rediscover it opportunistically.

Supporting analysis:
- [frame_analysis.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/frame_analysis.ml)

This is analysis, not a pass. It separates “what kind of frame does this
procedure need?” from “how do we materialize that frame into `LIR.Frame`?”.

### `Simplify`

File:
- [simplify.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/passes/simplify.ml)

`LIR.Simplify` applies cheap local rewrites to the linear stream: it removes
no-op moves, folds constant conditional branches, drops branches that only jump
to the next label, and collapses `[move; return]` into a direct return. These
are small wins, but they make the stream easier to clean up and emit.

### `Schedule`

File:
- [schedule.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/passes/schedule.ml)

`LIR.Schedule` is the last control-flow cleanup before emission. It collapses
adjacent labels, rewrites targets through label aliases, removes unreachable
code after jumps and returns, drops fallthrough jumps, and deletes labels that
are no longer referenced. The emitter should see a compact linear program, not
one still full of lowering artifacts.

## Next Likely Native Passes

The next passes worth adding are the ones that close the biggest gap with
`asmcomp` without forcing us to fake a register allocator:

- MIR local CSE
- MIR spill planning / first location-allocation step
- LIR or emitter-side leaf-function frame optimization using the new
  `contains_calls` metadata

The wrong next move would be inventing a generic pass framework. The right next
move is still to add concrete passes with explicit composition in the stage that
owns them.
