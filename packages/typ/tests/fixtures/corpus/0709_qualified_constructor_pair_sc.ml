(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_pair_sc
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

module M = struct
  type t = Wrap of string * char
  let value = Wrap (("x", 'y'))
end

let answer =
  match M.value with
  | M.Wrap (left, right) -> M.Wrap (left, right)
