(* oracle corpus fixture
   category: 09_functors
   title: choose_packed_module_int_or_bool
   complexity: 7
   min_ocaml: 4.08
   tags: modules, first_class_modules, selection
*)

module type BOX = sig
  type t
  val value : t
end

module Left = struct
  type t = int
  let value : t = 0
end

module Right = struct
  type t = int
  let value : t = 1
end

let choose flag =
  if flag then
    (module Left : BOX with type t = int)
  else
    (module Right : BOX with type t = int)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of (choose true)
