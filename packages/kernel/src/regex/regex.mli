(** Thin regular-expression bindings over the kernel's PCRE2 FFI.

    This module intentionally stays narrow:
    - compile a pattern once
    - test whether it matches a haystack
    - find the first match span

    Higher-level policy such as glob parsing or ignore semantics belongs in
    `std` or above. *)

(** A compiled regular expression. *)
type t

(** A compile-time regex error. *)
type compile_error = {
  message: string;
  offset: int option;
}

(** The first match span returned by {!find}.

    Offsets are zero-based byte offsets into the haystack, with `stop`
    exclusive. *)
type match_ = {
  start: int;
  stop: int;
}

(** Compile a regular-expression pattern. *)
val compile: string -> (t, compile_error) Result.t

(** Test whether the regex matches anywhere in the haystack. *)
val is_match: t -> string -> bool

(** Find the first match span in the haystack, if any. *)
val find: t -> string -> match_ option
