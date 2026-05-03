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
  | LabelsOmitted
  (** Warning 6: labels were omitted in function application *)
  | PartialMatch
  (** Warning 8: partial pattern match *)
  | BadModuleName
  (** Warning 24: source file name is not a valid module name *)
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
val warning_to_number: warning -> int

val warning_to_string: warning -> string

val warning_of_string: string -> warning option

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
  | Inline of int
  (** -inline <n>: Set inlining threshold *)
  | NoAssert
  (** -noassert: remove assertions *)
  | Compact
  (** -compact: optimize for code size *)
  | Unsafe
  (** -unsafe: disable some safety checks *)
  | Impl of Path.t
  (** -impl <file>: Compile <file> as implementation *)
  | Warning of warning list
  (** -w: Configure warning flags *)
  | WarnError of warning list
  (** -warn-error: Configure which warnings are treated as errors *)
  | Raw of string
  (** Raw ocamlc/ocamlopt argument token *)
  | LinkAll

(** -linkall: Link all modules even if not directly referenced *)
val flags_to_string: flag list -> string list

val flags_of_string: string list -> flag list

(** Compilation kind *)
type compilation_kind =
  | Bytecode
  (** `ocamlc`: fast compilation with slower runtime. *)
  | Native

(** `ocamlopt`: slower compilation with optimized runtime. *)
val compilation_kind_to_string: compilation_kind -> string

val compilation_kind_of_string: string -> compilation_kind option
