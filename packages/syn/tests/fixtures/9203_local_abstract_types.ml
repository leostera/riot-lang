(* Test: Multiple type variables in locally abstract types *)

let cast (type a b) (x: a) : b = Obj.magic x

let f (type a b c) x y z = (x, y, z)
