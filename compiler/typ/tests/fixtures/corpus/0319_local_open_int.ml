(* oracle corpus fixture
   category: 08_modules
   title: local_open_int
   complexity: 4
   min_ocaml: 4.08
   tags: modules, open, local_open
*)

module M = struct
  type t = int
  let value : t = 0
end

let answer = M.(value)
