(* oracle corpus fixture
   category: 08_modules
   title: simple_module_char
   complexity: 3
   min_ocaml: 4.08
   tags: modules, structures, values
*)

module M = struct
  type t = char
  let value : t = 'x'
  let id (x : t) = x
end

let answer = M.id M.value
