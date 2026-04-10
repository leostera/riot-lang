external ( = ) : 'a -> 'a -> bool = "%equal"

external ( mod ) : int -> int -> int = "%modint"

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
