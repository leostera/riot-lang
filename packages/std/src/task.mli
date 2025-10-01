type 'a t

val async : (unit -> 'a) -> 'a t
val await : 'a t -> ('a, exn) result
