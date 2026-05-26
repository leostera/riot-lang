(* oracle corpus fixture
   category: 02_functions
   title: map_custom_list_bools
   complexity: 3
   min_ocaml: 4.08
   tags: functions, recursion, map, custom_list
*)

type 'a my_list = Nil | Cons of 'a * 'a my_list

let rec map f xs =
  match xs with
  | Nil -> Nil
  | Cons (x, rest) -> Cons (f x, map f rest)

let answer = map (fun x -> (x, ())) (Cons (true, Cons (false, Nil)))
