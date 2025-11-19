let rec map f lst = match lst with [] -> [] | x :: xs -> f x :: map f xs
