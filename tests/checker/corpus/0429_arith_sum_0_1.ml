(* oracle corpus fixture
   category: 13_primitives
   title: arith_sum_0_1
   complexity: 4
   min_ocaml: 4.08
   tags: primitives, externals, int
*)

module Prim = struct
  external add_int : int -> int -> int = "%addint"
  external sub_int : int -> int -> int = "%subint"
  external eq : 'a -> 'a -> bool = "%equal"
  external lt : 'a -> 'a -> bool = "%lessthan"
  external raise_ : exn -> 'a = "%raise"
end


        let sum left right = Prim.add_int left right

        let answer = sum 0 1
