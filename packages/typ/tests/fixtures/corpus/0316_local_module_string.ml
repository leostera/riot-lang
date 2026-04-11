(* oracle corpus fixture
   category: 08_modules
   title: local_module_string
   complexity: 4
   min_ocaml: 4.08
   tags: modules, local_module, expression
*)

let answer =
  let module M = struct
    let value = "local"
    let id x = x
  end in
  M.id M.value
