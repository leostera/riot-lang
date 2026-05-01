(* oracle corpus fixture
   category: 14_schema_expansion
   title: qualified_constructor_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, variant, module_paths
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = Wrap of char option
  let value = Wrap (Some 'x')
end

let answer =
  match M.value with
  | M.Wrap x -> M.Wrap x
