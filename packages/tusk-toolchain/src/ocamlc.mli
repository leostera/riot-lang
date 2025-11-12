(** OCaml compiler command generation and execution

    This module provides a high-level interface for invoking the OCaml compiler
    with proper configuration and error handling. *)

open Std

type t

val make : Path.t -> t
val path : t -> Path.t

(** Compiler warnings that can be suppressed *)
type compiler_warning =
  | NoCmiFile  (** Warning 49: Absent cmi file when looking up module alias *)
  | All  (** All warnings *)

(** Compiler flags *)
type compiler_flag =
  | NoAliasDeps
      (** -no-alias-deps: Do not record dependencies for module aliases *)
  | Open of string  (** -open <module>: Opens the module before typing *)
  | NoStdlib
      (** -nostdlib: Do not automatically link with the standard library *)
  | NoPervasives
      (** -nopervasives: Do not open the Pervasives module (or Stdlib) *)
  | Impl of Std.Path.t
      (** -impl <file>: Compile <file> as an implementation file *)
  | Warning of compiler_warning list  (** -w: Configure warning flags *)
  | LinkAll
      (** -linkall: Link all modules even if not directly referenced (prevents dead-code elimination) *)

val flags_to_string : compiler_flag list -> string list

(** Compilation mode *)
type mode =
  | Compile  (** Compile to object file (-c flag) *)
  | Library  (** Create library archive (-a flag) *)
  | Executable  (** Link executable (default, no special flag) *)
  | CustomExe  (** Link with C stubs (-custom flag) *)
  | SharedLibrary  (** Create shared library (-shared flag for .cmxs plugins) *)

(** Compilation result *)
type result =
  | Success of string  (** Successful compilation with output *)
  | Failed of string  (** Compilation failed with error message *)

(** {1 Command Building} *)

val make_include_flags : string list -> string
(** Generate include flags from directory list. Converts a list of directories
    to "-I dir1 -I dir2 ..." format. *)

(** {1 Compilation} *)

val run :
  t ->
  cwd:Path.t ->
  ?includes:Path.t list ->
  ?libs:Path.t list ->
  ?cclibs:Path.t list ->
  ?ccflags:string list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  ?output:Path.t option ->
  ?mode:mode ->
  ?flags:compiler_flag list ->
  ?verbose:bool ->
  string list ->
  result
(** Build and run an ocamlc command.

    [run ~toolchain ?includes ?libs ?cclibs ?ccflags ?ccopt_flags ?cclib_flags ?output ?mode ?verbose sources] executes the
    OCaml compiler with the given configuration.

    @param toolchain The OCaml toolchain to use
    @param includes List of include directories (default: [])
    @param libs List of library files to link (default: [])
    @param cclibs List of foreign C/Rust libraries to link with -cclib (default: [])
    @param ccflags Additional C compiler/linker flags (legacy, default: [])
    @param ccopt_flags C compiler/linker flags passed with -ccopt (default: [])
    @param cclib_flags C linker-only flags passed with -cclib (default: [])
    @param output Output file path (default: None)
    @param mode Compilation mode (default: Compile)
    @param verbose Enable verbose output (default: false)
    @param sources Source files to compile
    @return Compilation result *)

(** {1 Specialized Compilation Functions} *)

val compile_interface :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  result
(** Compile an interface file (.mli -> .cmi). The current directory is
    automatically included. *)

val compile_impl :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  result
(** Compile an implementation file (.ml -> .cmo). The current directory is
    automatically included. *)

val generate_interface :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  flags:compiler_flag list ->
  output:Path.t ->
  Path.t ->
  result
(** Generate interface file (.ml -> .mli) using ocamlc -i. Infers the module
    interface from an implementation file and writes it to output. *)

val compile_c :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  ?ccflags:string list ->
  output:Path.t ->
  Path.t ->
  result
(** Compile a C file. The optional ccflags parameter specifies additional
    C compiler flags like -I for include directories. *)

val create_library :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  Path.t list ->
  result
(** Create a library (.cma) from object files *)

val create_executable :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  result
(** Create an executable from object files and libraries. The current directory
    is automatically included. The optional cclibs parameter specifies foreign
    C/Rust libraries to link with -cclib flags. The optional ccopt_flags parameter
    specifies C compiler/linker flags passed with -ccopt (like -framework).
    The optional cclib_flags parameter specifies C linker-only flags passed with
    -cclib (like -L/path, -lssl). *)

val create_shared_library :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  ?cclibs:Path.t list ->
  ?ccopt_flags:string list ->
  ?cclib_flags:string list ->
  Path.t list ->
  result
(** Create a shared library (.cmxs) from object files and libraries using -shared.
    Parameters are the same as create_executable but produces a plugin loadable with Dynlink. *)

val create_custom_executable :
  t ->
  cwd:Std.Path.t ->
  includes:Path.t list ->
  output:Path.t ->
  libs:Path.t list ->
  Path.t list ->
  result
(** Create a custom executable with C stubs. The current directory is
    automatically included. *)

(** {1 Result Helpers} *)

val is_success : result -> bool
(** Check if compilation succeeded *)

val get_output : result -> string
(** Get output message from result *)
