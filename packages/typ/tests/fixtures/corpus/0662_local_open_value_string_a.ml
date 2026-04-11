(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_string_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

module M = struct
  type t = string
  let value : t = ("a")
end

let answer = M.(value)
