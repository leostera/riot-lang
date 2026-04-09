# zort Runtime Loop

Start working immediately, following these directives:

1. Pick a task from ./BACKLOG.md
2. Work on it
3. Validate it with
```
zig fmt
zig build
zig test src
zig test e2e-ml
```
4. Make a small slice commit
5. Add new tasks if you can come up with any
6. Repeat

## Objective

We're primarily building `zort` to be a fully capable multicore effects OCaml
runtime. For more details read up ./spec documents.

## Architecture Constraints

- Treat `zort` as multicore-first from day 1. Do not land single-domain,
  single-threaded, or non-migratable control-flow shortcuts unless they are
  explicitly temporary and documented in `./BACKLOG.md`.
- Treat `zort` as multi-platform from day 1. Do not let POSIX, macOS, Linux,
  or one chosen compiler-compat target leak into the semantic kernel.
- Keep the split explicit:
  - semantic kernel owns values, heap, roots, collector, effects, fibers,
    scheduler/domain ownership;
  - host substrate owns threads, signal ingress, alternate signal stacks,
    plugin loading, clocks, and blocking syscall hooks;
  - compatibility layer owns `caml_*` shims and raw OCaml-shaped ABI details.
- `zort` provides capability; userland provides policy. Do not bake work
  stealing, domain pinning policy, actor scheduling policy, or permission
  policy into the runtime core when the mechanism can be exposed cleanly.
- Platform support must follow the capability intersection described in
  `./spec/platform-capabilities.md`:
  `effective_access = TargetCaps ∩ BuildCaps ∩ RuntimePermissions`.
- Runtime flags and permissions may only subtract access. They must never
  widen what the target or build compiled in.
- If a target cannot support a subsystem, compile it out. Unsupported
  subsystems should not survive as dead runtime branches in that target build.

## Loop Constraints

- Prefer fixture slices that preserve the multicore and multi-platform target
  shape even when the current executable proof is narrower.
- If a compiler-compat slice introduces target-specific glue, keep it in the
  compatibility layer or host substrate and note the locked target explicitly.
- Do not accept a fixture-driven shortcut if it would force the semantic kernel
  to depend on raw OCaml tags, raw native pointers, or one platform ABI.
- When adding new work to `./BACKLOG.md`, describe it in terms of:
  - the fixture that drives it,
  - the runtime capability it unlocks,
  - and the architecture boundary it must preserve.
