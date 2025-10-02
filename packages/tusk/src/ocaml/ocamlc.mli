(** OCaml compiler command generation and execution

    This module provides a high-level interface for invoking the OCaml compiler
    with proper configuration and error handling. *)

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

val flags_to_string : compiler_flag list -> string list

(** Compilation mode *)
type mode =
  | Compile  (** Compile to object file (-c flag) *)
  | Library  (** Create library archive (-a flag) *)
  | Executable  (** Link executable (default, no special flag) *)
  | CustomExe  (** Link with C stubs (-custom flag) *)

(** Compilation result *)
type result =
  | Success of string  (** Successful compilation with output *)
  | Failed of string  (** Compilation failed with error message *)

(** {1 Command Building} *)

val make_include_flags : string list -> string
(** Generate include flags from directory list. Converts a list of directories
    to "-I dir1 -I dir2 ..." format. *)

val base_command : Model.Toolchains.toolchain -> string
(** Generate the base ocamlc command path from toolchain *)

(** {1 Compilation} *)

val run :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  ?includes:string list ->
  ?libs:string list ->
  ?output:string option ->
  ?mode:mode ->
  ?verbose:bool ->
  string list ->
  result
(** Build and run an ocamlc command.

    [run ~toolchain ?includes ?libs ?output ?mode ?verbose sources] executes the
    OCaml compiler with the given configuration.

    @param toolchain The OCaml toolchain to use
    @param includes List of include directories (default: [])
    @param libs List of library files to link (default: [])
    @param output Output file path (default: None)
    @param mode Compilation mode (default: Compile)
    @param verbose Enable verbose output (default: false)
    @param sources Source files to compile
    @return Compilation result *)

(** {1 Specialized Compilation Functions} *)

val compile_interface :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  flags:compiler_flag list ->
  output:string ->
  string ->
  result
(** Compile an interface file (.mli -> .cmi). The current directory is
    automatically included. *)

val compile_impl :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  flags:compiler_flag list ->
  output:string ->
  string ->
  result
(** Compile an implementation file (.ml -> .cmo). The current directory is
    automatically included. *)

val generate_interface :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  flags:compiler_flag list ->
  output:string ->
  string ->
  result
(** Generate interface file (.ml -> .mli) using ocamlc -i. Infers the module
    interface from an implementation file and writes it to output. *)

val compile_c :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  output:string ->
  string ->
  result
(** Compile a C file *)

val create_library :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  output:string ->
  string list ->
  result
(** Create a library (.cma) from object files *)

val create_executable :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  output:string ->
  libs:string list ->
  string list ->
  result
(** Create an executable from object files and libraries. The current directory
    is automatically included. *)

val create_custom_executable :
  toolchain:Model.Toolchains.toolchain ->
  cwd:Std.Path.t ->
  includes:string list ->
  output:string ->
  libs:string list ->
  string list ->
  result
(** Create a custom executable with C stubs. The current directory is
    automatically included. *)

(** {1 Result Helpers} *)

val is_success : result -> bool
(** Check if compilation succeeded *)

val get_output : result -> string
(** Get output message from result *)
