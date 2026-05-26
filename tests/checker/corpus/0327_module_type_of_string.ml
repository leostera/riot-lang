(* oracle corpus fixture
   category: 08_modules
   title: module_type_of_string
   complexity: 5
   min_ocaml: 4.08
   tags: modules, module_type_of
*)

module Original = struct
  let value = "x"
  let id x = x
end

module Copy : module type of Original = struct
  let value = "x"
  let id x = x
end

let answer = Copy.id Copy.value
