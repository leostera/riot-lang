type t
module OpenFlags : sig
  type t =
    | ReadOnly
    | WriteOnly
    | ReadWrite
    | Create
    | Truncate
    | Append
    | Exclusive
  val to_unix : t list -> Unix.open_flag list
end

val to_int : t -> int

val to_unix : t -> Unix.file_descr

val of_unix : Unix.file_descr -> t

val make_blocking : Unix.file_descr -> t

val set_blocking : t -> unit

val close : t -> unit

val equal : t -> t -> bool

val open_file : string -> OpenFlags.t list -> int -> t

val is_tty : t -> bool

type pipe = {
  read_fd : t;
  write_fd : t;
}
val pipe : unit -> pipe
