(* oracle corpus fixture
   category: 08_modules
   title: simple_module_int
   complexity: 3
   min_ocaml: 4.08
   tags: modules, structures, values
*)

module M = struct
  type t = int
  let value : t = 0
  let id (x : t) = x
end

let answer = M.id M.value
