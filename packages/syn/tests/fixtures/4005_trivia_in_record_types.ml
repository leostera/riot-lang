(* Test that trivia is preserved in record types *)

(* Simple record *)

type t1 = {
  x: int;
  y: string;
}

(* Record with comments on fields *)

type t2 = {
  (* The x coordinate *)
  x: int;
  (* The y coordinate *)
  y: string;
}

(* Record with inline comments *)

type t3 = {
  x: int;  (* semicolon *)
  y: string;
}

(* Mutable record with trivia *)

type t4 = {
  mutable count: int;
  name: string;
}

(* Record with type parameters and trivia *)

type 'a t5 = {
  value: 'a;
  metadata: string;
}
