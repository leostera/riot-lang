(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = Wrap of int option
  let value = Wrap (Some 0)
end

let answer =
  match M.value with
  | M.Wrap x -> M.Wrap x
