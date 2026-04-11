(* oracle corpus fixture
   category: 12_gadts
   title: expr_eval_basic
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type _ expr =
  | Int : int -> int expr
  | Bool : bool -> bool expr
  | Pair : 'a expr * 'b expr -> ('a * 'b) expr

let rec eval : type a. a expr -> a = function
  | Int x -> x
  | Bool x -> x
  | Pair (left, right) -> (eval left, eval right)

let answer = (eval (Int 0), eval (Bool true), eval (Pair (Int 1, Bool false)))
