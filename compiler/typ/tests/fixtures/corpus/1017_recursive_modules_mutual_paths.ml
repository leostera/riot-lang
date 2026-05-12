(* oracle corpus fixture
   category: 15_recursive_modules
   title: recursive_modules_mutual_paths
   complexity: 8
   min_ocaml: 4.08
   tags: modules, recursive_modules
*)

module rec Left : sig
  type t = This of Right.t | Done
  val done_ : t
end = struct
  type t = This of Right.t | Done
  let done_ = Done
end
and Right : sig
  type t = That of Left.t | Stop
  val stop : t
end = struct
  type t = That of Left.t | Stop
  let stop = Stop
end

let answer = (Left.done_, Right.stop)
