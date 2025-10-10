(* Test that trivia is preserved in arrow types *)

(* Simple arrow with spaces *)
type t1 = int -> string

(* Arrow with comments before arrow *)
type t2 = int (* comment *) -> string

(* Arrow with comments after arrow *)
type t3 = int -> (* result type *) string

(* Nested arrows with comments *)
type t4 = int -> (* middle *) string -> (* end *) bool

(* Labeled parameter with trivia *)
type t5 = ~label:int -> string

(* Labeled parameter with comment after colon *)
type t6 = ~label: (* type *) int -> string

(* Optional parameter with trivia *)
type t7 = ?label:int -> string

(* Multiple parameters with trivia everywhere *)
type t8 = 
  ~first: (* first type *) int -> 
  ?second: (* optional *) string -> 
  (* final result *) bool
