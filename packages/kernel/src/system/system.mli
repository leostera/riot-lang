(** System-level operations for Kernel *)
open Global0
(** Host triplet type representing the target platform *)
module Host: sig
  type t = {
    (** CPU architecture: x86_64, aarch64, arm, etc. *)
    architecture: string;
    (** Vendor: apple, pc, unknown, etc. *)
    vendor: string;
    (** Operating system: linux, darwin, windows, etc. *)
    os: string;
    (** ABI: gnu, musl, msvc, mingw, etc. *)
    abi: string option;
  }
  val current: t
  (** Convert host triplet to standard string format: arch-vendor-os[-abi] *)
  val to_string: t -> string
  (** Parse a host triplet from string format: arch-vendor-os[-abi] *)
  val from_string: string -> (t, string) result
  (** Compare two host triplets for equality *)
  val equal: t -> t -> bool
end
(** The current host triplet *)
val host_triplet: Host.t

val available_parallelism: int
(** The type of the operating system (Unix, Win32, Cygwin) *)
val os_type: string
(** True if os_type is Unix *)
val unix: bool
(** True if os_type is Win32 *)
val win32: bool
(** True if os_type is Cygwin *)
val cygwin: bool
(** Size of one word in bits: 32 or 64 *)
val word_size: int
(** Size of integers in bits *)
val int_size: int
(** Whether the system is big-endian *)
val big_endian: bool
(** Maximum length of a string *)
val max_string_length: int
(** Maximum length of an array *)
val max_array_length: int
(** Maximum length of a float array *)
val max_floatarray_length: int
(** Return the name of the runtime variant *)
val runtime_variant: unit -> string
(** Return the values of the runtime parameters *)
val runtime_parameters: unit -> string
(** Set signal handler and return the previous handler *)
val signal: int -> (int -> unit) -> int -> unit
(** Set the behavior of the given signal *)
val set_signal: int -> signal_behavior -> unit

val sigabrt: int

val sigalrm: int

val sigfpe: int

val sighup: int

val sigill: int

val sigint: int

val sigkill: int

val sigpipe: int

val sigquit: int

val sigsegv: int

val sigterm: int

val sigusr1: int

val sigusr2: int

val sigchld: int

val sigcont: int

val sigstop: int

val sigtstp: int

val sigttin: int

val sigttou: int

val sigvtalrm: int

val sigprof: int

val sigbus: int

val sigpoll: int

val sigsys: int

val sigtrap: int

val sigurg: int

val sigxcpu: int
(** Standard signal numbers *)
val sigxfsz: int

exception Break
(** Exception raised on interactive interrupt if catch_break is on *)

(** catch_break governs whether interactive interrupt (ctrl-C) raises Break *)
val catch_break: bool -> unit
(** The version of OCaml *)
val ocaml_version: string
(** Enable or disable runtime warnings *)
val enable_runtime_warnings: bool -> unit
(** Return whether runtime warnings are enabled *)
val runtime_warnings_enabled: unit -> bool
(** Identity function that prevents optimizations *)
val opaque_identity: 'a -> 'a
(** Name of the current executable *)
val executable_name: string
(** Command line arguments *)
val argv: unit -> string array
(** Execute a program, replacing the current process *)
val execv: string -> string array -> unit

module OsProcess = OsProcess
