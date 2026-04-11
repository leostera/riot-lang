(* oracle corpus fixture
   category: 13_primitives
   title: while_loop_loop_count_2
   complexity: 5
   min_ocaml: 4.08
   tags: primitives, while, mutable_record
*)

module Prim = struct
  external add_int : int -> int -> int = "%addint"
  external sub_int : int -> int -> int = "%subint"
  external eq : 'a -> 'a -> bool = "%equal"
  external lt : 'a -> 'a -> bool = "%lessthan"
  external raise_ : exn -> 'a = "%raise"
end


        type counter = { mutable value : int }

        let run start =
          let state = { value = start } in
          while Prim.lt 0 state.value do
            state.value <- Prim.sub_int state.value 1
          done;
          state.value

        let answer = run 2
