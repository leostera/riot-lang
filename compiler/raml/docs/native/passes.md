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

Purpose:
- put `NIR` into a more regular, ANF-like shape
- lift nested `let` structure out of call positions and conditional conditions
- flatten adjacent `let` chains

Why it exists:
- later passes should not have to reason about arbitrarily nested binding
  structure when they are doing local rewrites

### `Simplify`

File:
- [simplify.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/nir/passes/simplify.ml)

Purpose:
- remove obviously redundant expression structure
- fold literal-boolean `if` expressions
- remove dead pure `let` bindings
- drop pure entry-side `Eval` items

Why it exists:
- `NIR` should carry only semantically meaningful structure into `MIR`

## `MIR`

`MIR` is the first pseudo-instruction layer. It still has structured
conditionals, but it is close enough to machine code that analysis and local
optimization start to matter.

### `Canonicalize`

File:
- [canonicalize.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/canonicalize.ml)

Purpose:
- remove no-op moves
- fold empty or identical-branch conditionals
- fold literal-boolean conditionals

Why it exists:
- later MIR passes should see a smaller and more uniform tree

### `Insert_polls`

File:
- [insert_polls.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/insert_polls.ml)

Purpose:
- insert explicit `raml_poll` calls before non-poll calls

Why it exists:
- polling is a runtime obligation, and it should be visible in IR snapshots
  before emission

### `Copy_propagate`

File:
- [copy_propagate.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/copy_propagate.ml)

Purpose:
- substitute cheap copied values forward through MIR
- propagate registers, literals, and symbol addresses
- expose newly dead moves before dead-code elimination runs

Why it exists:
- this is the first cheap local dataflow cleanup that meaningfully reduces IR
  noise without needing full regalloc machinery

### `Dead_code`

File:
- [dead_code.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/mir/passes/dead_code.ml)

Purpose:
- remove instructions whose result is unused and whose execution is pure
- erase unused call destinations while preserving the call itself

Why it exists:
- MIR should not carry dead temporary traffic into linearization

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

Purpose:
- compute per-procedure frame metadata
- decide whether a frame is required
- record whether the procedure performs calls
- assign stable stack-slot offsets and aligned frame size

Why it exists:
- emitters should consume frame metadata, not rediscover stack layout from the
  instruction stream

Supporting analysis:
- [frame_analysis.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/frame_analysis.ml)

This is analysis, not a pass. It separates “what kind of frame does this
procedure need?” from “how do we materialize that frame into `LIR.Frame`?”.

### `Simplify`

File:
- [simplify.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/passes/simplify.ml)

Purpose:
- perform tiny local simplifications on the linear stream
- remove no-op moves
- fold constant conditional branches
- remove conditional branches that jump to the immediately following label
- collapse `[move; return]` pairs into direct returns

Why it exists:
- these are cheap wins that make scheduling and emission simpler

### `Schedule`

File:
- [schedule.ml](/Users/leostera/Developer/github.com/leostera/riot/compiler/raml-native/src/lir/passes/schedule.ml)

Purpose:
- normalize the linear control-flow graph before emission
- collapse adjacent labels
- rewrite targets through label aliases
- remove unreachable linear code
- remove fallthrough jumps
- drop unused labels

Why it exists:
- emitters should see a compact linear program, not one still full of lowering
  artifacts

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
