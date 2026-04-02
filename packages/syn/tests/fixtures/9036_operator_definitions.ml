let ( ! ) = fun x -> x

let ( := ) = fun x y -> ()

let ( <> ) = fun x y -> true

let ( && ) = fun x y -> x && y

let ( || ) = fun x y -> x || y

let ( |> ) x f = f x
