(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_char_a
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

module M = struct
  type t = char
  let value : t = ('a')
end

let answer = M.(value)
