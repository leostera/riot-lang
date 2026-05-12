# Primitive Boundary, Named Values, and Native Dynlink

## Source anchors

- `vendor/ocaml/runtime/caml/prims.h`
- `vendor/ocaml/runtime/prims.c`
- `vendor/ocaml/runtime/callback.c`
- `vendor/ocaml/runtime/dynlink_nat.c`
- `vendor/ocaml/runtime/startup_nat.c`

## Primitive dispatch model

- The native runtime boundary is not a typed registry.
- Builtin primitives are compiled into generated tables in `prims.c`:
  - `caml_builtin_cprim[]`
  - `caml_names_of_builtin_cprim[]`
- Runtime dispatch uses the extensible `caml_prim_table`.
- Arity-specific macros in `prims.h` cast primitive slots by convention:
  - `Primitive1`
  - `Primitive2`
  - `Primitive3`
  - `Primitive4`
  - `Primitive5`
  - `PrimitiveN`
- In debug builds, a parallel primitive-name table exists for tracing/disassembly.
- Observable consequence: the primitive ABI is fundamentally “index in a runtime table plus expected calling convention”, not a richer typed contract.

## Named values

- The runtime exposes a string-keyed registry through:
  - `caml_register_named_value`
  - `caml_named_value`
  - `caml_iterate_named_values`
- Registration uses a small fixed hash table protected by a mutex.
- A new entry becomes a generational global root.
- Updating an existing entry uses `caml_modify_generational_global_root`.
- Lookup returns a pointer to the stored `value`, or `NULL` if absent.
- Runtime services use named values for late-bound hooks and exceptions.
- Observable consequence: named values are runtime-global mutable bindings with GC-managed liveness, not mere lookup helpers.

## Native dynlink open/load behavior

- Native dynlink opens shared libraries in a blocking section.
- A library must export `caml_plugin_header` or opening fails with `Failure("not an OCaml plugin")`.
- Successful open returns:
  - an abstract handle block
  - a parsed plugin header value
- Symbol names are mangled as `caml<unit><sep><name>`, where the separator is:
  - `$` on Windows, Cygwin, and macOS
  - `.` elsewhere

## Native dynlink registration behavior

- Registration is not just “make symbols callable”.
- For each requested compilation unit, the runtime requires:
  - a `frametable` symbol
  - a `gc_roots` symbol
- Missing metadata is reported as `Invalid_argument`.
- Registration adds:
  - frame tables for stack scanning and backtraces
  - dynamic global roots
  - non-empty code fragments for code identity / metadata lookup
- Registered code fragments use `DIGEST_LATER`.
- Observable consequence: native dynlink is coupled to GC, stack scanning, and code metadata. It is not just `dlopen + dlsym`.

## Entrypoints and symbol lookup

- `caml_natdynlink_run` resolves the unit `entry` symbol and invokes it if present.
- If no entrypoint exists, it returns `unit`.
- `caml_natdynlink_loadsym` resolves a named global symbol through `caml_globalsym`.
- Missing symbols raise `Failure(<symbol>)`.
- A native dynlink hook can observe `(handle, unit)` before the entrypoint runs.

## Lifetime asymmetry

- `caml_natdynlink_close` closes the library handle.
- Registration of frame tables, globals, and code fragments is additive from this API surface.
- This file does not present a symmetric “unregister everything on close” story.
- For zort, that asymmetry matters more than loader convenience.

## zort takeaways

- If zort wants a replacement/shim runtime, the primitive boundary should be designed explicitly instead of inherited as “integer slot plus cast”.
- Named values are a real late-binding mechanism in the native runtime and should either be supported intentionally or dropped explicitly.
- Native dynlink support requires more than symbol loading:
  - stack metadata
  - GC roots
  - code-fragment registration
- A maintainable zort boundary would be better served by typed handles and explicit registration records than by the OCaml runtime's loosely typed primitive table.

## zort compatibility boundary notes

- zort now treats `src/caml_compat/api.zig` as a shim-only boundary.
- The shim uses an explicit compat codec in `src/caml_compat/codec.zig`:
  - compat ints use a tagged immediate encoding,
  - compat atoms use a distinct tagged immediate encoding,
  - heap values are exported as opaque handles, not raw pointers.
- Handle slots are rooted explicitly through the runtime so a compat-exported block stays alive until the shim releases the handle.
- This is an intentional divergence from the OCaml runtime:
  - zort does not expose raw OCaml heap pointers as boundary values,
  - zort does not rely on OCaml's exact one-bit immediate encoding internally or at the shim boundary.
- The first typed primitive boundary lives in `src/primitive_registry.zig`:
  - string name,
  - explicit arity,
  - semantic `Value` arguments/results,
  - explicit lookup/arity errors.
- External primitive dispatch is now explicitly callback-mediated:
  - `PrimitiveRegistry.callWithBoundary(...)` enters a callback boundary around primitive execution,
  - exported shim entrypoints in `src/caml_compat/api.zig` use that path,
  - ambient parent-fiber handlers are therefore hidden from primitive-triggered `perform` unless the primitive installs its own local handler chain.
- Internal runtime-owned dispatch can still use the naked `call(...)` path when callback mediation is not desired.
- The shim is build-gated:
  - `zig build compat` builds the shim artifact,
  - `zig build test -Dcompat-shim=false` exercises the core runtime without the shim.
- The current exported primitive-call surface is pointer-safe:
  - `(name_ptr, name_len, args...)` at the boundary,
  - semantic decode/dispatch only inside the shim.
- zort now also has a small runtime-services substrate in `src/runtime_services.zig`:
  - named values are stored as explicit service-owned roots,
  - lookup is string-keyed,
  - service state is runtime-instance-local instead of OCaml-runtime-global,
  - and named-value / signal-handler mutation is serialized by a runtime-local mutex.
- This is an intentional divergence from OCaml for now:
  - no global mutexed named-value table,
  - no dynlink frame-table / gc-roots / code-fragment registration yet,
  - no claim that named values survive across runtime instance boundaries.
