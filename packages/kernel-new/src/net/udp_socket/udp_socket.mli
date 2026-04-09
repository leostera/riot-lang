type t
type error =
  | Invalid_slice of { pos: int; len: int; buffer_len: int }
  | Not_connected
  | Message_too_long
  | Destination_address_required
  | Address_in_use
  | System of System_error.t
val error_to_string: error -> string

val bind: ?reuse_addr:bool -> ?reuse_port:bool -> Socket_addr.t -> (t, error) Result.t

val connect: t -> Socket_addr.t -> (unit, error) Result.t

val close: t -> (unit, error) Result.t

val local_addr: t -> (Socket_addr.t, error) Result.t

val recv: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val recv_from: t -> ?pos:int -> ?len:int -> bytes -> ((int * Socket_addr.t), error) Result.t

val send: t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val send_to: t -> Socket_addr.t -> ?pos:int -> ?len:int -> bytes -> (int, error) Result.t

val to_source: t -> Async.Source.t
