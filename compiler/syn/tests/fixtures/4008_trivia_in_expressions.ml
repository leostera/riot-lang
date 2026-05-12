(* Test that trivia is preserved in expressions *)

(* Let bindings with trivia *)

let x =
  (* equals *)
  1

(* value *)

(* Multiple bindings *)

let a = 1

and b = 2

(* Function with trivia *)

let f x =
  (* body *)
  x
  + 1

(* Pattern matching with trivia *)

let g x =
  match x with
  | 0 ->
      (* arrow *)
      1
  | n -> n + 1

(* If-then-else with trivia *)

let h x =
  if x > 0 then
    1
    (* then branch *)
  else
    (* else *)
    (-1)

(* else branch *)

(* For loop with trivia *)

let _ =
  for i = 0 to 10 do
    print_int i
  done

(* done *)

(* While loop with trivia *)

let _ =
  while true do
    print_endline "loop"
  done

(* done *)

(* Try-with with trivia *)

let _ =
  try raise Not_found with
  | Not_found ->
      (* arrow *)
      ()

(* handler *)
