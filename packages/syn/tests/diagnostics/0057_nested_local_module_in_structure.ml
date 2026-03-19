(* Nested local modules used as structure items are invalid *)
let compute x y =
  let module Outer = struct
    let module Inner = struct
      let add a b = a + b
    end in
    let multiply a b = a * b
  end in
  Outer.Inner.add x y
