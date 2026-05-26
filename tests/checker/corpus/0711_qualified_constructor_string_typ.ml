(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_string_typ
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

module M = struct
  type t = Wrap of string
  let value = Wrap ("typ")
end

let answer =
  match M.value with
  | M.Wrap x -> M.Wrap x
