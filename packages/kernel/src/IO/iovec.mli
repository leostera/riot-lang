type iov = {
  ba : bytes;
  off : int;
  len : int;
}
type t = iov array
val with_capacity : int -> t

val create : ?count:int -> size:int -> unit -> t

val sub : ?pos:int -> len:int -> t -> t

val length : t -> int

val iter : t -> (iov -> unit) -> unit

val of_bytes : bytes -> t

val from_string : string -> t

val from_buffer : Buffer.t -> t

val into_string : t -> string
