(* Existential packing with GADTs. *)
type showable =
  | Show : 'a * ('a -> string) -> showable

let items =
  [
    Show (42, string_of_int);
    Show (true, string_of_bool);
    Show ("ok", Fun.id);
  ]

let () =
  List.iter
    (fun (Show (x, show)) -> print_endline (show x))
    items
