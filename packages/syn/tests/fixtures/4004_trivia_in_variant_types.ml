(* Test that trivia is preserved in variant types *)

(* Simple variant *)

type t1 =
  A
  | B
  | C

(* Variant with comments between constructors *)

type t2 =
  | A
  (* first *)
  | B
  (* second *)
  | C

(* third *)

(* Variant with arguments and trivia *)

type t3 =
  None
  | Some
    (* value *)
    of
    (* type *)
    int

(* Variant with multiple arguments *)

type t4 =
  | Pair
    (* constructor *)
    of
    (* first *)
    int * string

(* Polymorphic variant with trivia *)

type t5 =
[
  `A
  (* first *)
  | `B
    (* second *)
    of
    (* payload *)
    int
]

(* GADT with trivia *)

type _ t6 =
  | Int: int t6
  | String: string t6
