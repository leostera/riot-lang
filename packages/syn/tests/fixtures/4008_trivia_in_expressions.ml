(* Test that trivia is preserved in expressions *)

(* Let bindings with trivia *)
let x (* name *) = (* equals *) 1 (* value *)

(* Multiple bindings *)
let a (* first *) = 1
and b (* second *) = 2

(* Function with trivia *)
let f (* name *) x (* param *) = (* body *) x + 1

(* Pattern matching with trivia *)
let g x =
  match (* match *) x (* scrutinee *) with (* with *)
  | (* bar *) 0 (* pattern *) -> (* arrow *) 1 (* result *)
  | n (* otherwise *) -> n + 1

(* If-then-else with trivia *)
let h x =
  if (* if *) x > 0 (* condition *)
  then (* then *) 1 (* then branch *)
  else (* else *) -1 (* else branch *)

(* For loop with trivia *)
let _ =
  for (* for *) i (* var *) = (* equals *) 0 (* start *)
    to (* direction *) 10 (* end *)
  do (* do *)
    print_int i (* body *)
  done (* done *)

(* While loop with trivia *)
let _ =
  while (* while *) true (* condition *) do (* do *)
    print_endline "loop" (* body *)
  done (* done *)

(* Try-with with trivia *)
let _ =
  try (* try *)
    raise Not_found (* body *)
  with (* with *)
  | (* bar *) Not_found (* pattern *) -> (* arrow *) () (* handler *)
