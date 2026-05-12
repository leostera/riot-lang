# Runtime-Hosted Primitives and Boundary Surface

## Source anchors

- `vendor/ocaml/runtime/io.c`
- `vendor/ocaml/runtime/sys.c`
- `vendor/ocaml/runtime/lexing.c`
- `vendor/ocaml/runtime/parsing.c`

## What lives here

- The OCaml runtime tree is not only a GC/value engine.
- It also hosts primitive implementations for:
  - buffered channels
  - basic syscalls and process operations
  - generated lexer/parser engines
- The native primitive table and dynlink/loader boundary are documented separately in [`primitive-boundary-and-native-dynlink.md`](./primitive-boundary-and-native-dynlink.md).

## Channels

- Channels are buffered C structures with explicit locks.
- Channel operations lock first, and the runtime tracks the last locked channel so exception raising can unlock it during unwinding.
- GC-managed channels are linked in a global list.
- Pending actions may temporarily unlock managed channels before running signal handlers/finalizers, then relock afterward.
- Closed/open channel states are encoded structurally in the channel object fields.

## Basic syscalls

- System-call wrappers translate `errno` into OCaml exceptions.
- `EAGAIN` / `EWOULDBLOCK` map to the blocked-I/O exception instead of generic `Sys_error`.
- File-path arguments must be C-safe strings; embedded NUL bytes are rejected.

## Lexer and parser engines

- `lexing.c` implements the table-driven automata used by `ocamllex`.
- Refill is signaled by returning negative states to ML, not by calling the refill function directly in C.
- Empty-token cases raise `Failure("lexing: empty token")`.
- `parsing.c` implements the pushdown automaton used by `ocamlyacc` / `Parsing`.
- The parser cooperates with ML through a command protocol: read token, grow stacks, compute semantic action, call error function, and so on.

## zort boundary guidance

- zort does not need to absorb every runtime-hosted primitive into the core runtime.
- The clean split is:
  - core runtime semantics in the replacement runtime
  - hosted primitive libraries or shims for channels/sys/parsing support
- This matters because these files are observable, but they are not all equally central to replacing the GC/value engine.
