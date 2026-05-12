# Runtime Sync Primitives

## Source anchors

- `vendor/ocaml/runtime/sync.c`
- `vendor/ocaml/runtime/sync_posix.h`
- `vendor/ocaml/runtime/sync_win32.h`
- `vendor/ocaml/runtime/caml/custom.h`

## Representation

- Runtime mutexes and condition variables are exposed as custom blocks.
- Finalization destroys the underlying OS primitive.
- Comparison and hashing use primitive identity:
  - compare by underlying handle/pointer value
  - hash by underlying handle/pointer value
- Serialization is not provided.
- Observable consequence: these values are runtime-owned resource handles, not pure OCaml data.

## Error mapping

- Runtime sync helpers convert OS error codes into OCaml exceptions.
- `ENOMEM` becomes `Out_of_memory`.
- Other failures become `Sys_error("<operation>: <os error>")`.

## Mutex behavior

- `Mutex.create` allocates an OS mutex and wraps it in a custom block.
- `Mutex.lock` uses a fast path first:
  - try to acquire without entering a blocking section
  - if that fails, enter a blocking section and block in the OS primitive
- `Mutex.unlock` does not leave/re-enter the runtime lock; it performs the OS unlock directly.
- `Mutex.try_lock` returns `false` only for “already locked”, and raises for other errors.

## Ownership semantics

- On POSIX, the runtime uses `PTHREAD_MUTEX_ERRORCHECK`.
- On Windows, the runtime emulates owner tracking around an SRW lock.
- Observable behavior on both backends is intended to include:
  - recursive lock attempts fail
  - unlocking by a non-owner fails
- Those failures surface through the runtime error-mapping path rather than silent undefined behavior.

## Condition-variable behavior

- `Condition.create` allocates an OS condition variable and wraps it in a custom block.
- `Condition.wait`:
  - enters a blocking section
  - waits while releasing the mutex through the OS primitive
  - returns with the mutex re-acquired on success
  - emits a runtime event marker for domain condition wait
- `Condition.signal` and `Condition.broadcast` are direct wake operations.
- Waiting without owning the mutex is an error on the inspected backends.

## GC and lifetime interaction

- The runtime sync primitives are GC-managed only at the wrapper level.
- The underlying OS primitives live outside the OCaml heap and are reclaimed by finalizers.
- This means logical liveness and OS-resource lifetime are coupled to custom-block reachability.

## zort takeaways

- If zort wants runtime-owned sync primitives, they should be modeled as typed resource handles with explicit ownership/error rules.
- The OCaml runtime's observable contract is “resource identity plus error mapping”, not a rich algebra of synchronization states.
- If zort would rather keep sync out of the core runtime, that should be an explicit boundary decision because the current runtime does expose these as part of its native surface.
