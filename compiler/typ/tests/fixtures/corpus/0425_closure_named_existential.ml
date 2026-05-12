(* oracle corpus fixture
   category: 12_gadts
   title: closure_named_existential
   complexity: 8
   min_ocaml: 4.08
   tags: gadts, existential, locally_abstract_types
*)

type _ closure = Closure : ('a -> 'b) * 'a -> 'b closure

let eval = fun (Closure (type a) (f, x : (a -> _) * _)) -> f (x : a)

let answer = eval (Closure ((fun x -> x), 0))
