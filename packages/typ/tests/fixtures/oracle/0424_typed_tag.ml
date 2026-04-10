(* oracle corpus fixture
   category: 12_gadts
   title: typed_tag
   complexity: 7
   min_ocaml: 4.08
   tags: gadts, locally_abstract_types
*)

type _ tag =
  | TInt : int tag
  | TBool : bool tag

let default : type a. a tag -> a = function
  | TInt -> 0
  | TBool -> false

let answer = (default TInt, default TBool)
