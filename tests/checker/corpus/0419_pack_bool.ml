(* oracle corpus fixture
   category: 12_gadts
   title: pack_bool
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, existential
*)

type packed = Pack : 'a * ('a -> 'a) -> packed

let run (Pack (x, f)) = Pack (f x, f)

let answer = run (Pack (true, fun x -> x))
