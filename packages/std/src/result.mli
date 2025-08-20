(** Result type for error handling *)

type ('a, 'e) t = ('a, 'e) Stdlib.result = Ok of 'a | Error of 'e

val ok : 'a -> ('a, 'e) t
val err : 'e -> ('a, 'e) t
val error : 'e -> ('a, 'e) t (* Alias for err *)
val is_ok : ('a, 'e) t -> bool
val is_error : ('a, 'e) t -> bool
val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
val get_ok : ('a, 'e) t -> 'a option
val get_error : ('a, 'e) t -> 'e option
val get_ok_exn : ('a, 'e) t -> 'a
val get_error_exn : ('a, 'e) t -> 'e
val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) t -> 'c
val iter : ('a -> unit) -> ('a, 'e) t -> unit
val iter_error : ('e -> unit) -> ('a, 'e) t -> unit
val to_option : ('a, 'e) t -> 'a option
val of_option : error:'e -> 'a option -> ('a, 'e) t
val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
val all : ('a, 'e) t list -> ('a list, 'e) t
val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t

val unwrap : ('a, 'e) t -> 'a
(** Get the Ok value or panic with a message *)

val unwrap_err : ('a, 'e) t -> 'e
(** Get the Error value or panic with a message *)
