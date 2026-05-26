(* Labeled parameters with type annotations *)

let f ~(x:int) = x

(* Multiple labeled params with types *)

let g ~(a:string) ~(b:float) = (a, b)

(* Mixed labeled params *)

let h ~x ~(y:int) ~(z:bool) = (x, y, z)

(* Optional params with types *)

let i ?(x:int = 0) = x

(* In function types *)

type t = x:(int -> int) -> unit
