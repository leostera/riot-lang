(* Test: locally abstract types in let bindings *)

let id : type a. a -> a = fun x -> x

let make : type item state. (item, state) iter -> state -> item t = fun i s -> ()

let convert : type a b. (a -> b) -> a -> b = fun f x -> f x
