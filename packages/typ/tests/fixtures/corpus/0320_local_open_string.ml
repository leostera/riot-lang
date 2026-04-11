(* oracle corpus fixture
   category: 08_modules
   title: local_open_string
   complexity: 4
   min_ocaml: 4.08
   tags: modules, open, local_open
*)

module M = struct
  type t = string
  let value : t = "s"
end

let answer = M.(value)
