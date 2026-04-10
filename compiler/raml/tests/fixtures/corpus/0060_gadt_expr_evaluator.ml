(* Typed expression interpreter. *)
type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Add : int expr * int expr -> int expr
  | Eq : int expr * int expr -> bool expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

let rec eval : type a. a expr -> a = function
  | Int n -> n
  | Bool b -> b
  | Add (x, y) -> eval x + eval y
  | Eq (x, y) -> eval x = eval y
  | If (c, t, e) -> if eval c then eval t else eval e

let () =
  let program =
    If (Eq (Int 4, Add (Int 2, Int 2)), Add (Int 20, Int 22), Int 0)
  in
  Printf.printf "%d\n" (eval program)
