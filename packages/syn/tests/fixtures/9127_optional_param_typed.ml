(* Optional parameters with type annotations *)

(* Simple type with default *)

let foo ?(x:int = 5) = x + 1

(* Function type with default *)

let apply ?(f:int -> int = fun x -> x * 2) n = f n

(* Multiple optional params *)

let greet ?(prefix:string = "Hello") ?(suffix:string = "!") name = prefix ^ " " ^ name ^ suffix
