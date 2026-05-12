(* oracle corpus fixture
   category: 02_functions
   title: fold_custom_list_chars
   complexity: 3
   min_ocaml: 4.08
   tags: functions, recursion, fold, custom_list
*)

type 'a my_list = Nil | Cons of 'a * 'a my_list

let rec fold_left f acc xs =
  match xs with
  | Nil -> acc
  | Cons (x, rest) -> fold_left f (f acc x) rest

let step acc x = Cons (x, acc)

let answer = fold_left step Nil (Cons ('a', Cons ('b', Nil)))
