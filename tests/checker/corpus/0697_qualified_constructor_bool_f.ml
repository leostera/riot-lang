(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_bool_f
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

module M = struct
  type t = Wrap of bool
  let value = Wrap (false)
end

let answer =
  match M.value with
  | M.Wrap x -> M.Wrap x
