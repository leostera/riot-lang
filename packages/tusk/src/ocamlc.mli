(** OCaml compiler command generation and execution
    
    This module provides a high-level interface for invoking the OCaml
    compiler with proper configuration and error handling. *)

(** Compilation mode *)
type mode =
  | Compile    (** Compile to object file (-c flag) *)
  | Library    (** Create library archive (-a flag) *)
  | Executable (** Link executable (default, no special flag) *)
  | CustomExe  (** Link with C stubs (-custom flag) *)

(** Compilation result *)
type result = 
  | Success of string  (** Successful compilation with output *)
  | Failed of string   (** Compilation failed with error message *)

(** {1 Command Building} *)

(** Generate include flags from directory list.
    Converts a list of directories to "-I dir1 -I dir2 ..." format. *)
val make_include_flags : string list -> string

(** Generate the base ocamlc command path from toolchain *)
val base_command : Toolchains.toolchain -> string

(** {1 Compilation} *)

(** Build and run an ocamlc command.
    
    [run ~toolchain ?includes ?libs ?output ?mode ?verbose sources]
    executes the OCaml compiler with the given configuration.
    
    @param toolchain The OCaml toolchain to use
    @param includes List of include directories (default: [])
    @param libs List of library files to link (default: [])
    @param output Output file path (default: None)
    @param mode Compilation mode (default: Compile)
    @param verbose Enable verbose output (default: false)
    @param sources Source files to compile
    @return Compilation result
*)
val run : 
  toolchain:Toolchains.toolchain ->
  ?includes:string list ->
  ?libs:string list ->
  ?output:string option ->
  ?mode:mode ->
  ?verbose:bool ->
  string list -> result

(** {1 Specialized Compilation Functions} *)

(** Compile an interface file (.mli -> .cmi).
    The current directory is automatically included. *)
val compile_interface : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  string -> result

(** Compile an implementation file (.ml -> .cmo).
    The current directory is automatically included. *)
val compile_impl : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  string -> result

(** Compile a C file *)
val compile_c : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  string -> result

(** Create a library (.cma) from object files *)
val create_library : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  string list -> result

(** Create an executable from object files and libraries.
    The current directory is automatically included. *)
val create_executable : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  libs:string list ->
  string list -> result

(** Create a custom executable with C stubs.
    The current directory is automatically included. *)
val create_custom_executable : 
  toolchain:Toolchains.toolchain ->
  includes:string list ->
  output:string ->
  libs:string list ->
  string list -> result

(** {1 Result Helpers} *)

(** Check if compilation succeeded *)
val is_success : result -> bool

(** Get output message from result *)
val get_output : result -> string