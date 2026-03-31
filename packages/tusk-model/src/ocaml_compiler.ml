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
let warning_to_number =
  function
  | PartialMatch -> 8
  | UnusedVariable -> 26
  | UnusedOpen -> 33
  | UnusedConstructor -> 34
  | UnusedMatch -> 11
  | NoCmiFile -> 49
  | All -> (-1)

(* Special: use 'a' *)

let warning_to_string =
  function
  | PartialMatch -> "partial-match"
  | UnusedVariable -> "unused-variable"
  | UnusedOpen -> "unused-open"
  | UnusedConstructor -> "unused-constructor"
  | UnusedMatch -> "unused-match"
  | NoCmiFile -> "no-cmi-file"
  | All -> "all"

let warning_of_string =
  function
  | "partial-match" -> Some PartialMatch
  | "unused-variable" -> Some UnusedVariable
  | "unused-open" -> Some UnusedOpen
  | "unused-constructor" -> Some UnusedConstructor
  | "unused-match" -> Some UnusedMatch
  | "no-cmi-file" -> Some NoCmiFile
  | "all" -> Some All
  | _ -> None

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
let compilation_kind_to_string =
  function
  | Bytecode -> "bytecode"
  | Native -> "native"

let compilation_kind_of_string =
  function
  | "bytecode" -> Some Bytecode
  | "native" -> Some Native
  | _ -> None
