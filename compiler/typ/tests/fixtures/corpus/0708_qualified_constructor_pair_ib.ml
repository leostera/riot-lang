(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_pair_ib
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

module M = struct
  type t = Wrap of int * bool
  let value = Wrap ((0, true))
end

let answer =
  match M.value with
  | M.Wrap (left, right) -> M.Wrap (left, right)
