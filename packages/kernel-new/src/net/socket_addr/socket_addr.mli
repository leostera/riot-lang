type t
type error = Error.t
val make: ip:Ip_addr.t -> port:int -> (t, error) Result.t

val of_parts: ip:Ip_addr.t -> port:int -> (t, error) Result.t

val of_parts_unchecked: ip:Ip_addr.t -> port:int -> t

val loopback_v4: port:int -> t

val loopback_v6: port:int -> t

val ip: t -> Ip_addr.t

val port: t -> int

val to_parts: t -> Ip_addr.t * int

val to_string: t -> string
