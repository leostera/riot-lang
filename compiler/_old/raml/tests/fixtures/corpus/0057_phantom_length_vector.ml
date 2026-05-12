(* Phantom type indices using GADT-style constructors. *)
type zero
type 'n succ

type _ vec =
  | VNil : zero vec
  | VCons : int * 'n vec -> 'n succ vec

let rec sum : type n. n vec -> int = function
  | VNil -> 0
  | VCons (x, xs) -> x + sum xs

let () =
  let v = VCons (1, VCons (2, VCons (3, VNil))) in
  Printf.printf "%d\n" (sum v)
