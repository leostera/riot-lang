(* oracle corpus fixture
   category: 14_schema_expansion
   title: local_open_value_int0
   complexity: 4
   min_ocaml: 4.08
   tags: schema, module, local_open
*)

module M = struct
  type t = int
  let value : t = (0)
end

let answer = M.(value)
