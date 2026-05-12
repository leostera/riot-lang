(* Signature ascription and representation hiding. *)
module type COUNTER = sig
  type t
  val zero : t
  val succ : t -> t
  val to_int : t -> int
end

module Counter : COUNTER = struct
  type t = int
  let zero = 0
  let succ x = x + 1
  let to_int x = x
end

let () =
  let n = Counter.(succ (succ zero)) in
  Printf.printf "%d\n" (Counter.to_int n)
