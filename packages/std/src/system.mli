(** # System - System information and operations

    System-level operations and information queries with cross-platform
    abstractions for common system tasks.

    ## Examples

    Getting system information:

    ```ocaml open Std

    (* Get environment information *) let os = System.os_type in let arch =
    System.arch in

    Log.info "Running on %s (%s)" os arch ```

    Working with exit codes:

    ```ocaml (* Exit with error code *) if not valid_config then System.exit 1

    (* Successful exit *) System.exit 0 ```

    ## See Also

    - [Env] for environment variables
    - [Command] for running external processes *)

(** Platform identity and executable metadata for the current host. *)
module Host: sig
  type t = Kernel.System.Host.t = {
    architecture: string;
    vendor: string;
    os: string;
    abi: string option;
  }
  val current: t

  (** Use `to_string host` to render `host` as `arch-vendor-os[-abi]`. *)
  val to_string: t -> string

  (** Use `from_string value` to parse `arch-vendor-os[-abi]`. *)
  val from_string: string -> (t, string) Result.t

  val equal: t -> t -> bool
end

module OS: sig
  type t = Kernel.System.OS.t =
    | Unix
    | Win32
    | Cygwin
  val current: t

  (** Use `to_string value` for the stable legacy rendering used across Riot. *)
  val to_string: t -> string

  val is_unix: bool

  val is_win32: bool

  val is_cygwin: bool
end

val host_triplet: Host.t

val os_type: string

val unix: bool

val win32: bool

val cygwin: bool

(** Exit the current process immediately with the given status code. *)
val exit: int -> 'a
