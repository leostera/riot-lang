(* oracle corpus fixture
   category: 12_gadts
   title: expr_pair_4
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, expr, evaluator
*)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

let rec eval : type a. a expr -> a = function
  | Int x -> x
  | Bool x -> x
  | Pair (left, right) -> (eval left, eval right)

let answer = eval (Pair (Int 3, Bool false))
