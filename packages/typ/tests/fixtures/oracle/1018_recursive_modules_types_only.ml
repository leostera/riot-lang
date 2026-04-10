(* oracle corpus fixture
   category: 15_recursive_modules
   title: recursive_modules_types_only
   complexity: 8
   min_ocaml: 4.08
   tags: modules, recursive_modules
*)

module rec A : sig
  type t = Leaf | Node of B.t
end = struct
  type t = Leaf | Node of B.t
end
and B : sig
  type t = A.t list
end = struct
  type t = A.t list
end

let answer : A.t = A.Leaf
