(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_option_char
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = char option
  let value : t = (Some 'x')
end

let answer = M.(value)
