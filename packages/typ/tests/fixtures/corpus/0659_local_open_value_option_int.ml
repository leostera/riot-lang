(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_option_int
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

type 'a option =
  | Some of 'a
  | None

module M = struct
  type t = int option
  let value : t = (Some 0)
end

let answer = M.(value)
