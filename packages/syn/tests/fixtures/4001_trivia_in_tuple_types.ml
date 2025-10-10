(* Test that trivia is preserved in tuple types *)

(* Simple tuple with spaces *)
type t1 = int * string

(* Tuple with comment before star *)
type t2 = int (* first *) * string

(* Tuple with comment after star *)
type t3 = int * (* second *) string

(* Long tuple with comments between elements *)
type t4 =
  int (* first *) * string (* second *) * bool (* third *) * float (* fourth *)

(* Nested tuple with trivia *)
type t5 = (int * string) * (* pair *) (bool * float) (* another pair *)
