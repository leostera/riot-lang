(* Test that trivia is preserved in type constructors *)

(* Simple type constructor *)

type t1 = list

(* Type constructor with comment before type arg *)

type t2 = int list

(* Type constructor with multiple args *)

type t3 = (int, string) Hashtbl.t

(* Type constructor with comments between module path *)

type t4 = Foo.Bar.t

(* module *)

(* separator *)

(* submodule *)

(* Parametric type with comment after param *)

(* parameter *)

type 'a t5 = 'a list

(* Multiple type params with trivia *)

type ('a, 'b) t6 = ('a, 'b) result
