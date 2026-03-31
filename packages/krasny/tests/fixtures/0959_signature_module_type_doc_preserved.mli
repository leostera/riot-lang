(** First-class module type for writable destinations. *)
module type Write = sig
  type t
  val write : t -> buf:string -> int
end
