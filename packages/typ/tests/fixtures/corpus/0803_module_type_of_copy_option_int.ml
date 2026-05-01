(* oracle corpus fixture
   category: 14_schema_expansion
   title: module_type_of_copy_option_int
   complexity: 5
   min_ocaml: 4.08
   tags: schema, module, module_type_of
*)

type 'a option =
  | Some of 'a
  | None

module Original = struct
  let value = (Some 0)
  let id x = x
end

module Copy : module type of Original = struct
  let value = (Some 0)
  let id x = x
end

let answer = Copy.id Copy.value
