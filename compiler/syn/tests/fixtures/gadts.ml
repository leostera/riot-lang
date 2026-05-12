(* Basic GADT *)

type _ t =
  | Int: int t
  | Bool: bool t

(* GADT with arguments *)

type _ expr =
  | Val: 'a -> 'a expr
  | Pair: 'a expr * 'b expr -> ('a * 'b) expr
  | App: ('a -> 'b) expr * 'a expr -> 'b expr

(* GADT with multiple type parameters *)

type (_, _) eq =
  | Refl: ('a, 'a) eq

(* Existential wrapper *)

type dyn =
  | Dyn: 'a * ('a -> string) -> dyn

(* Mixed regular and GADT constructors *)

type _ value =
  | String: string -> string value
  | Int: int -> int value
  | None: _ value
