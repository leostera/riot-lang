(* Private type in module signature. *)
module User_id : sig
  type t = private int
  val make : int -> t
  val to_int : t -> int
end = struct
  type t = int
  let make n =
    if n < 0 then invalid_arg "negative user id" else n
  let to_int x = x
end

let () =
  let id = User_id.make 7 in
  Printf.printf "%d\n" (User_id.to_int id)
