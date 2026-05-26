(* oracle corpus fixture
   category: 15_recursive_modules
   title: recursive_modules_values
   complexity: 8
   min_ocaml: 4.08
   tags: modules, recursive_modules
*)

module rec A : sig
  type t = Wrap of B.t
  val make : B.t -> t
end = struct
  type t = Wrap of B.t
  let make value = Wrap value
end
and B : sig
  type t = Empty | More of A.t
  val empty : t
end = struct
  type t = Empty | More of A.t
  let empty = Empty
end

let answer = A.make B.empty
