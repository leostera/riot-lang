(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_unit
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

module M = struct
  type t = unit
  let value : t = (())
end

let answer = M.(value)
