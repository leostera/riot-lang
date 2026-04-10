(* oracle corpus fixture
   category: 09_functors
   title: choose_packed_module_pair_or_pair
   complexity: 7
   min_ocaml: 4.08
   tags: modules, first_class_modules, selection
*)

module type BOX = sig
  type t
  val value : t
end

module Left = struct
  type t = int * bool
  let value : t = (0, true)
end

module Right = struct
  type t = int * bool
  let value : t = (1, false)
end

let choose flag =
  if flag then
    (module Left : BOX with type t = int * bool)
  else
    (module Right : BOX with type t = int * bool)

let seed_of (type a) (module X : BOX with type t = a) : a =
  X.value

let answer = seed_of (choose true)
