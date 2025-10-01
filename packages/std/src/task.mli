type 'a t

val async : (unit -> 'a) -> 'a t
val await : 'a t -> ('a, exn) result

val await_all : 'a t list -> ('a, exn) result list
(** Efficiently await multiple tasks, collecting results as they arrive. More
    efficient than [List.map await] for large task lists. *)
