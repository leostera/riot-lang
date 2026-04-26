(**
   Thin regular-expression bindings over the kernel's PCRE2 FFI.

   This module intentionally stays narrow:
   - compile a pattern once
   - test whether it matches a haystack
   - find the first match span

   Higher-level policy such as glob parsing or ignore semantics belongs in
   `std` or above.
*)
open Prelude

type t
type compile_error = {
  message: string;
  offset: int option;
}
type match_ = { start: int; stop: int }
val compile: string -> (t, compile_error) result

val is_match: t -> string -> bool

val find: t -> string -> match_ option
