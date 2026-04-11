(* oracle corpus fixture
   category: 13_primitives
   title: array_get_set_arith_sum_2_3
   complexity: 5
   min_ocaml: 4.08
   tags: primitives, arrays, mutation
*)

module Prim = struct
  external add_int : int -> int -> int = "%addint"
  external sub_int : int -> int -> int = "%subint"
  external eq : 'a -> 'a -> bool = "%equal"
  external lt : 'a -> 'a -> bool = "%lessthan"
  external raise_ : exn -> 'a = "%raise"
end


        let touch array =
          let before = array.(0) in
          array.(0) <- Prim.add_int before 2;
          (before, array.(0))

        let answer = touch [| 3; 2 |]
