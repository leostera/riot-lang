(* oracle corpus fixture
   category: 09_functors
   title: choose_packed_module_string_or_char
   complexity: 7
   min_ocaml: 4.08
   tags: modules, first_class_modules, selection
*)

module type BOX = sig
  type t
  val value : t
end

module Left = struct
  type t = string
  let value : t = "a"
end

module Right = struct
  type t = string
  let value : t = "b"
end

let choose flag =
  if flag then
    (module Left : BOX with type t = string)
  else
    (module Right : BOX with type t = string)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of (choose true)
