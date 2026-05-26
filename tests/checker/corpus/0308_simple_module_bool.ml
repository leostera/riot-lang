(* oracle corpus fixture
   category: 08_modules
   title: simple_module_bool
   complexity: 3
   min_ocaml: 4.08
   tags: modules, structures, values
*)

module M = struct
  type t = bool
  let value : t = true
  let id (x : t) = x
end

let answer = M.id M.value
