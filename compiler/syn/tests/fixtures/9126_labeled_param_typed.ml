(* Labeled parameters with type annotations *)

(* Simple type *)

let foo ~(x:int) = x + 1

(* Function type with arrow *)

let map ~(f:int -> int) lst = List.map f lst

(* Complex type *)

let process ~(handler:'a -> 'b option) ~(data:'a list) = List.filter_map handler data

(* Multiple type variables *)

let combine ~(fn:'a -> 'b -> 'c) ~(x:'a) ~(y:'b) : 'c = fn x y
