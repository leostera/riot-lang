(**
   OCaml compiler command generation and execution

   This module provides a high-level interface for invoking the OCaml compiler
   with proper configuration and error handling.
*)
open Std
open Riot_model

type t
type invocation

module Diagnostic: sig
  type severity =
    | Warning
    | Error
    | Note
    | Unknown
  type location = {
    path: string;
    line: int option;
    start_char: int option;
    end_char: int option;
    column: int option;
  }
  type t

  val parse: string -> t list

  val render: t -> string

  val render_all: t list -> string

  val map_path: (string -> string option) -> t -> t

  val location: t -> location option

  val severity: t -> severity

  val is_warning: t -> bool
end

val make: Path.t -> t

val path: t -> Path.t

(** Compiler warnings that can be suppressed *)
type compiler_warning = Ocaml_compiler.warning =
  | LabelsOmitted
  | PartialMatch
  | BadModuleName
  | UnusedVariable
  | UnusedOpen
  | UnusedConstructor
  | UnusedMatch
  | NoCmiFile
  | All

(** All warnings *)

(** Compiler flags *)
type compiler_flag = Ocaml_compiler.flag =
  | NoAliasDeps
  | Open of string
  | NoStdlib
  | NoPervasives
  | Inline of int
  | NoAssert
  | Compact
  | Unsafe
  | Impl of Std.Path.t
  | Warning of compiler_warning list
  | WarnError of compiler_warning list
  | Raw of string
  | LinkAll

(** -linkall: Link all modules even if not directly referenced (prevents dead-code elimination) *)
val flags_to_string: compiler_flag list -> string list

val flags_of_string: string list -> compiler_flag list

(** Compilation result *)
type success = {
  message: string;
  diagnostics: Diagnostic.t list;
}
type failure = {
  message: string;
  diagnostics: Diagnostic.t list;
}
type result =
  | Success of success
  (** Successful compilation with output *)
  | Failed of failure

(** Compilation failed with error message *)
(** {1 Compilation} *)

(** {1 Specialized Compilation Functions} *)

val compile_interface:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  invocation

(**
   Compile an interface file (.mli -> .cmi). The current directory is
   automatically included.
*)
val compile_impl:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  invocation

(**
   Compile an implementation file (.ml -> .cmo). The current directory is
   automatically included.
*)
val generate_interface:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  invocation

(**
   Generate interface file (.ml -> .mli) using ocamlc -i. Infers the module
   interface from an implementation file and writes it to output.
*)
val compile_c:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  ?cc:Path.t ->
  ?ccflags:string list ->
  output:Path.t ->
  Path.t ->
  invocation

(**
   Compile a C file. The optional ccflags parameter specifies additional
   C compiler flags like -I for include directories.
*)
val create_library:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  Path.t list ->
  invocation

(** Create a library (.cma) from object files *)
val compile_library:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t list ->
  invocation

(** Compile source files and create a library in one compiler invocation. *)
val create_executable:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cc:Path.t ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  invocation

(**
   Create an executable from object files and libraries. The current directory
   is automatically included. The optional cclibs parameter specifies foreign
   C/Rust libraries to link with -cclib flags. The optional ccopt_flags parameter
   specifies C compiler/linker flags passed with -ccopt (like -framework).
   The optional cclib_flags parameter specifies C linker-only flags passed with
   -cclib (like -L/path, -lssl).
*)
val create_shared_library:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cc:Path.t ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  invocation

(**
   Create a shared library (.cmxs) from object files and libraries using -shared.
   Parameters are the same as create_executable but produces a plugin loadable with Dynlink.
*)
val create_custom_executable:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cc:Path.t ->
  Path.t list ->
  invocation

(**
   Create a custom executable with C stubs. The current directory is
   automatically included.
*)
val to_string: invocation -> string

(**
   Render the prepared compiler invocation as a shell-style string for logging
   and telemetry.
*)
val run: invocation -> result

(** Execute a prepared compiler invocation. *)
(** {1 Result Helpers} *)

val is_success: result -> bool

(** Check if compilation succeeded *)
val get_output: result -> string

(** Get output message from result *)
val get_ocamlc_warnings: result -> string list

(** Get warning payloads emitted during successful compilation *)
