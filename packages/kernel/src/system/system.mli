(** Platform identity and executable metadata for the current host. *)
module Host: sig
  type error =
    | InvalidTripletFormat of { value: string }
  type t = {
    architecture: string;
    vendor: string;
    os: string;
    abi: string option;
  }

  val current: t

  (** Use `to_string host` to render `host` as `arch-vendor-os[-abi]`. *)
  val to_string: t -> string

  (** Use `from_string value` to parse `arch-vendor-os[-abi]`. *)
  val error_message: error -> string

  val from_string: string -> (t, error) Result.t

  val equal: t -> t -> bool
end

module OS: sig
  type t =
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
