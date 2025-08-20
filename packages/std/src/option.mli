(** Option type utilities *)

type 'a t = 'a option = None | Some of 'a

val some : 'a -> 'a t
val none : 'a t
val is_some : 'a t -> bool
val is_none : 'a t -> bool
val map : ('a -> 'b) -> 'a t -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val value : 'a t -> default:'a -> 'a
val value_exn : 'a t -> 'a
val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b
val fold : none:'b -> some:('a -> 'b) -> 'a t -> 'b
val iter : ('a -> unit) -> 'a t -> unit
val filter : ('a -> bool) -> 'a t -> 'a t
val join : 'a t t -> 'a t
val all : 'a t list -> 'a list t
val both : 'a t -> 'b t -> ('a * 'b) t
val to_result : error:'e -> 'a t -> ('a, 'e) Result.t
val to_list : 'a t -> 'a list

val unwrap : 'a t -> 'a
(** Get the Some value or panic with a message *)

val unwrap_none : 'a t -> unit
(** Panic if the option is Some *)
