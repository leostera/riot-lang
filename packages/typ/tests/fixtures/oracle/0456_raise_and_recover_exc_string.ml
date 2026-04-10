(* oracle corpus fixture
   category: 13_primitives
   title: raise_and_recover_exc_string
   complexity: 6
   min_ocaml: 4.08
   tags: exceptions, primitives, try_with
*)

module Prim = struct
  external add_int : int -> int -> int = "%addint"
  external sub_int : int -> int -> int = "%subint"
  external eq : 'a -> 'a -> bool = "%equal"
  external lt : 'a -> 'a -> bool = "%lessthan"
  external raise_ : exn -> 'a = "%raise"
end


        exception E of string

        let run flag =
          try
            if flag then
              Prim.raise_ (E "boom")
            else
              ""
          with
          | E value -> value

        let answer = run true
