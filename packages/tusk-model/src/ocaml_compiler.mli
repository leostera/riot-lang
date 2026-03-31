open Std

(** Compilation mode *)
type mode =
  | Compile
  (** Compile to object file (-c flag) *)
  | Library
  (** Create library archive (-a flag) *)
  | Executable
  (** Link executable (default, no special flag) *)
  | CustomExe
(** Link with C stubs (-custom flag) *)
(** OCaml compiler warnings *)
type warning =
  | PartialMatch
  (** Warning 8: partial pattern match *)
  | UnusedVariable
  (** Warning 26: unused variable *)
  | UnusedOpen
  (** Warning 33: unused open *)
  | UnusedConstructor
  (** Warning 34: unused constructor *)
  | UnusedMatch
  (** Warning 11: unused match case *)
  | NoCmiFile
  (** Warning 49: missing cmi when looking up module alias *)
  | All
(** All warnings *)
val warning_to_number : warning -> int

val warning_to_string : warning -> string

val warning_of_string : string -> warning option

(** OCaml compiler flags - commonly used options *)
type flag =
  | NoAliasDeps
  (** -no-alias-deps: Do not record dependencies for module aliases *)
  | Open of string
  (** -open Module: Auto-open module before typing *)
  | NoStdlib
  (** -nostdlib: Do not automatically link with stdlib *)
  | NoPervasives
  (** -nopervasives: Do not open Pervasives/Stdlib *)
  | Impl of Path.t
  (** -impl <file>: Compile <file> as implementation *)
  | Warning of warning list
(** -w: Configure warning flags *)
(** Compilation kind *)
type compilation_kind =
  | Bytecode
  (** ocamlc - fast compilation, slower runtime *)
  | Native
(** ocamlopt - slower compilation, optimized runtime *)
val compilation_kind_to_string : compilation_kind -> string

val compilation_kind_of_string : string -> compilation_kind option
