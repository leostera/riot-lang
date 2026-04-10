# Raml Mach And Emission Notes

This document records the `asmcomp` pipeline after Cmm generation.

This is the machine-dependent half of the native backend, but much of it is
still target-generic.

## 1. The Actual Pass Order

`asmcomp/asmgen.ml` is the clearest source for the backend pipeline.

For each Cmm function declaration, `compile_fundecl` runs:

1. Cmm invariants
2. instruction selection
3. polling instrumentation
4. allocation combining
5. common subexpression elimination
6. liveness
7. dead-code elimination
8. spilling suggestions
9. liveness again
10. live-range splitting
11. liveness again
12. register allocation and reload insertion
13. linearization
14. instruction scheduling
15. emission

That is the order to preserve mentally when reading the rest of `asmcomp`.

## 2. Selection Produces Mach

`selection.mli` exposes:

- `fundecl : ... -> Cmm.fundecl -> Mach.fundecl`

The generic machinery is in `selectgen.mli`.

`selector_generic` owns:

- immediate checks
- addressing-mode selection
- operation selection
- condition selection
- register creation
- extcall argument lowering
- block-store emission
- Cmm-expression emission into Mach pseudo-instructions

Target-specific selectors specialize that generic selector rather than building
the whole pass from scratch.

## 3. What Mach Represents

`mach.mli` defines:

- integer and float comparisons
- machine operations
- structured instruction trees
- `Mach.fundecl`

The operation set includes:

- moves, spills, reloads, and constants
- direct and indirect calls
- tail calls
- external calls
- stack offsets
- loads and stores with addressing modes
- allocations
- integer and float ops
- opaque identity
- target-specific operations
- polling
- domain-local-state access
- return-address fetch

The instruction graph still has structured control nodes:

- `Iifthenelse`
- `Iswitch`
- `Icatch`
- `Itrywith`

So Mach is not yet a flat instruction list.

## 4. Polling Is A Real Analysis Pass

`polling.ml` is worth reading closely.

It does not blindly insert polls everywhere.

It runs two analyses:

- loop safety for recursive handlers
- "polls before potentially recursive tail call" analysis

The pass then:

- inserts `Ipoll` on unguarded loop back edges
- answers whether a prologue poll is required
- enforces `[@poll error]` by reporting any polling points found in the
  function body

This is an important architectural clue.

Polling is treated as a correctness property over control flow, not just as a
late runtime call insertion.

## 5. Pseudo-Registers Carry Real Backend State

`reg.mli` defines the pseudo-register structure used across the backend.

A register carries:

- a unique stamp
- its machine type component
- an assigned location
- spill preference
- interference and preference lists
- degree and spill cost

Locations can be:

- physical register
- local stack slot
- incoming stack slot
- outgoing stack slot
- domain-state slot

The `Domainstate` location is especially revealing.

The backend has explicit support for argument passing through a domain-state
area rather than ordinary stack slots, so calling convention is not only
"registers plus stack".

## 6. Register Allocation Is Iterative

`asmgen.regalloc` shows the real structure:

- do liveness
- allocate registers either with linear scan or graph coloring
- run reload insertion
- if reload says redo allocation, reinitialize and repeat

There is a hard safety guard:

- after 50 rounds the backend fails with "function too complex"

Two allocation modes exist:

### Graph coloring

- build interference graph with `Interf.build_graph`
- allocate with `Coloring.allocate_registers`

### Linear scan

- build intervals with `Interval.build_intervals`
- allocate with `Linscan.allocate_registers`

So register allocation is already an interchangeable backend policy.

## 7. The Support Passes Matter

Several smaller passes are structurally important:

### `comballoc`

- combines heap allocations in one basic block

### `CSE` / `CSEgen`

- value-numbering common subexpression elimination over extended basic blocks

### `deadcode`

- removes pure instructions whose results are unused

### `spill`

- inserts moves to suggest spill and reload points before allocation

### `split`

- renames registers at reload points to split live ranges

Taken together, these are classical backend passes, but they are arranged
around Mach rather than around a separate SSA IR.

## 8. Reload Is Target-Aware

`reloadgen.mli` provides a generic reloader, but targets can override:

- how operations reload
- which tests can operate on stack locations
- how fresh reload temporaries are created

The point is that post-allocation legality is target-specific.
Not every operation can tolerate every stack/register combination.

## 9. Linear IR Is A Real Boundary

`linear.mli` defines a flatter pseudo-instruction representation.

It adds linear-control concepts such as:

- labels
- branches
- conditional branches
- trap-entry/exit instructions
- reload-return-address
- explicit prologue marker

`linearize.ml` lowers Mach into this list form.

One especially useful comment in `linearize.ml` explains that a branch after a
poll can be absorbed into the poll's `return_label`, which helps loop-back-edge
polls.

So linearization is not a mechanical pretty-printing step.
It still performs backend-relevant control shaping.

## 10. Stack-Frame Analysis Happens Before Emit

`stackframegen.ml` computes per-function frame facts:

- whether the function contains non-tail OCaml calls
- whether a frame is required
- how much extra stack is used for traps and outgoing arguments

The generic analysis treats as frame-requiring calls:

- ordinary non-tail calls
- external calls
- allocations
- polls
- some bound checks in debug mode
- exception-raising constructs for better backtraces
- `trywith`

That means stack-frame policy is not buried only inside the emitter.
It is an explicit analysis step.

## 11. Scheduling Happens After Linearization

`scheduling.mli` exposes:

- `fundecl : Linear.fundecl -> Linear.fundecl`

Scheduling is therefore post-allocation and post-linearization.

That tells us the backend is optimizing final instruction order, not trying to
schedule a higher-level DAG before register allocation.

## 12. Emit Happens On Linear IR

`emit.mli` is intentionally small:

- `fundecl`
- `data`
- `begin_assembly`
- `end_assembly`

By the time emission begins, nearly all structural decisions are already made.

`emitaux.mli` then provides the shared emission substrate:

- textual assembly output helpers
- debug-info emission
- frame-descriptor recording
- call-frame-information helpers
- binary-backend plumbing
- per-function emission environment creation

This separation is useful for `raml` too:

- one layer owns backend decisions
- another layer owns assembler/object serialization details

## 13. The Backend Can Restart At Emit

`Asmgen.compile_implementation_linear` can rebuild code generation from saved
Linear IR.

That is a real seam.

It means the backend is already organized so that:

- earlier expensive stages can be skipped
- emission can be treated as a later standalone phase

If `raml` wants fast backend iteration, keeping a comparable seam is probably
worth it.

## 14. Design Pressure On `raml`

The current Mach-to-emit stack suggests a few rules.

### Keep polling explicit

Polling is a control-flow and safety property.
It should stay a named pass, not an emitter afterthought.

### Keep regalloc and reload separate

The current design treats allocation, reload legality, and retry as distinct
concerns.
That separation is useful.

### Keep a linear IR boundary

The backend benefits from a final flat representation before emission.
That is a good seam for debugging, caching, and alternate emitters.

### Keep stack-frame analysis as data

Frame requirements, extra stack, and call properties are explicit today.
That is much easier to reason about than baking them into ad hoc emitter state.
