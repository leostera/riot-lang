(* oracle corpus fixture
   category: 05_variants
   title: custom_result_resultish
   complexity: 2
   min_ocaml: 4.08
   tags: variants, custom_result
*)

type ('a, 'b) resultish = Good of 'a | Bad of 'b

let map_good f value =
  match value with
  | Good x -> Good (f x)
  | Bad y -> Bad y

let answer = map_good (fun x -> x) (Good 0)
