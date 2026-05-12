(* oracle corpus fixture
   category: 13_primitives
   title: for_loop_loop_count_0
   complexity: 5
   min_ocaml: 4.08
   tags: primitives, for, mutable_record
*)

module Prim = struct
  external add_int : int -> int -> int = "%addint"
  external sub_int : int -> int -> int = "%subint"
  external eq : 'a -> 'a -> bool = "%equal"
  external lt : 'a -> 'a -> bool = "%lessthan"
  external raise_ : exn -> 'a = "%raise"
end


        type counter = { mutable value : int }

        let run () =
          let state = { value = 0 } in
          for i = 0 to 0 do
            state.value <- Prim.add_int state.value i
          done;
          state.value

        let answer = run ()
