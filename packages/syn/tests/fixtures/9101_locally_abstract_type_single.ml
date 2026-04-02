(* Single locally abstract type *)

let id (type a) (x: a) : a = x

(* With type annotation *)

let convert (type a) (x: a) : string = "value"

(* Without type annotation *)

let apply (type a) (f: a -> a) (x: a) = f x
