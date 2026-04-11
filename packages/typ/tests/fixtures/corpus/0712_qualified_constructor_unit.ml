(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_unit
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

module M = struct
  type t = Wrap of unit
  let value = Wrap (())
end

let answer =
  match M.value with
  | M.Wrap x -> M.Wrap x
