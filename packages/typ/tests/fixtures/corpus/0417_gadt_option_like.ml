(* oracle corpus fixture
   category: 12_gadts
   title: gadt_option_like
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type _ maybe =
  | Nothing : unit maybe
  | Just : 'a -> 'a maybe

let view : type a. a maybe -> a maybe = function
  | Nothing -> Nothing
  | Just x -> Just x

let answer = (view Nothing, view (Just 0))
