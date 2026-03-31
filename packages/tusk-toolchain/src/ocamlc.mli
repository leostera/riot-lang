(** OCaml compiler command generation and execution

    This module provides a high-level interface for invoking the OCaml compiler
    with proper configuration and error handling. *)
open Std

type t
type invocation
val make: Path.t -> t

val path: t -> Path.t

(** Compiler warnings that can be suppressed *)
type compiler_warning =
  | NoCmiFile
  (** Warning 49: Absent cmi file when looking up module alias *)
  | All
(** All warnings *)
(** Compiler flags *)
type compiler_flag =
  | NoAliasDeps
  (** -no-alias-deps: Do not record dependencies for module aliases *)
  | Open of string
  (** -open <module>: Opens the module before typing *)
  | NoStdlib
  (** -nostdlib: Do not automatically link with the standard library *)
  | NoPervasives
  (** -nopervasives: Do not open the Pervasives module (or Stdlib) *)
  | Impl of Std.Path.t
  (** -impl <file>: Compile <file> as an implementation file *)
  | Warning of compiler_warning list
  (** -w: Configure warning flags *)
  | LinkAll
(** -linkall: Link all modules even if not directly referenced (prevents dead-code elimination) *)
val flags_to_string: compiler_flag list -> string list

(** Compilation result *)
type result =
  | Success of string
  (** Successful compilation with output *)
  | Failed of string
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

(** Compile an interface file (.mli -> .cmi). The current directory is
    automatically included. *)
val compile_impl:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  invocation

(** Compile an implementation file (.ml -> .cmo). The current directory is
    automatically included. *)
val generate_interface:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  invocation

(** Generate interface file (.ml -> .mli) using ocamlc -i. Infers the module
    interface from an implementation file and writes it to output. *)
val compile_c:
  t -> cwd:Std.Path.t -> includes:Path.t list -> ?ccflags:string list -> output:Path.t -> Path.t -> invocation

(** Compile a C file. The optional ccflags parameter specifies additional
    C compiler flags like -I for include directories. *)
val create_library: t -> cwd:Std.Path.t -> includes:Path.t list -> output:Path.t -> Path.t list -> invocation

(** Create a library (.cma) from object files *)
val create_executable:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  invocation

(** Create an executable from object files and libraries. The current directory
    is automatically included. The optional cclibs parameter specifies foreign
    C/Rust libraries to link with -cclib flags. The optional ccopt_flags parameter
    specifies C compiler/linker flags passed with -ccopt (like -framework).
    The optional cclib_flags parameter specifies C linker-only flags passed with
    -cclib (like -L/path, -lssl). *)
val create_shared_library:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  invocation

(** Create a shared library (.cmxs) from object files and libraries using -shared.
    Parameters are the same as create_executable but produces a plugin loadable with Dynlink. *)
val create_custom_executable:
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  Path.t list ->
  invocation

(** Create a custom executable with C stubs. The current directory is
    automatically included. *)
val to_string: invocation -> string

(** Render the prepared compiler invocation as a shell-style string for logging
    and telemetry. *)
val run: invocation -> result

(** Execute a prepared compiler invocation. *)
(** {1 Result Helpers} *)

val is_success: result -> bool

(** Check if compilation succeeded *)
val get_output: result -> string

(** Get output message from result *)
