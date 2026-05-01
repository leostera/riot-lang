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

  val to_string: t -> string

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
