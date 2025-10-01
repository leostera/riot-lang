(** System-level operations for Kernel *)

(** Host triplet type representing the target platform *)
module Host : sig
  type t = {
    architecture : string;  (** CPU architecture: x86_64, aarch64, arm, etc. *)
    vendor : string;  (** Vendor: apple, pc, unknown, etc. *)
    os : string;  (** Operating system: linux, darwin, windows, etc. *)
    abi : string option;  (** ABI: gnu, musl, msvc, mingw, etc. *)
  }

  val current : t

  val to_string : t -> string
  (** Convert host triplet to standard string format: arch-vendor-os[-abi] *)
end

val host_triplet : Host.t
(** The current host triplet *)

val available_parallelism : int

val os_type : string
(** The type of the operating system (Unix, Win32, Cygwin) *)

val unix : bool
(** True if os_type is Unix *)

val win32 : bool
(** True if os_type is Win32 *)

val cygwin : bool
(** True if os_type is Cygwin *)

val word_size : int
(** Size of one word in bits: 32 or 64 *)

val int_size : int
(** Size of integers in bits *)

val big_endian : bool
(** Whether the system is big-endian *)

val max_string_length : int
(** Maximum length of a string *)

val max_array_length : int
(** Maximum length of an array *)

val max_floatarray_length : int
(** Maximum length of a float array *)

val runtime_variant : unit -> string
(** Return the name of the runtime variant *)

val runtime_parameters : unit -> string
(** Return the values of the runtime parameters *)

val signal : int -> (int -> unit) -> int -> unit
(** Set signal handler and return the previous handler *)

val set_signal : int -> Sys.signal_behavior -> unit
(** Set the behavior of the given signal *)

val sigabrt : int
val sigalrm : int
val sigfpe : int
val sighup : int
val sigill : int
val sigint : int
val sigkill : int
val sigpipe : int
val sigquit : int
val sigsegv : int
val sigterm : int
val sigusr1 : int
val sigusr2 : int
val sigchld : int
val sigcont : int
val sigstop : int
val sigtstp : int
val sigttin : int
val sigttou : int
val sigvtalrm : int
val sigprof : int
val sigbus : int
val sigpoll : int
val sigsys : int
val sigtrap : int
val sigurg : int
val sigxcpu : int

val sigxfsz : int
(** Standard signal numbers *)

exception Break
(** Exception raised on interactive interrupt if catch_break is on *)

val catch_break : bool -> unit
(** catch_break governs whether interactive interrupt (ctrl-C) raises Break *)

val ocaml_version : string
(** The version of OCaml *)

val enable_runtime_warnings : bool -> unit
(** Enable or disable runtime warnings *)

val runtime_warnings_enabled : unit -> bool
(** Return whether runtime warnings are enabled *)

val opaque_identity : 'a -> 'a
(** Identity function that prevents optimizations *)

val executable_name : string
(** Name of the current executable *)

val argv : unit -> string array
(** Command line arguments *)

module OsProcess = OsProcess
