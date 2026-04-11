(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_type_of_copy_pair_sc
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, module_type_of
*)

module Original = struct
  let value = (("x", 'y'))
  let id x = x
end

module Copy : module type of Original = struct
  let value = (("x", 'y'))
  let id x = x
end

let answer = Copy.id Copy.value
