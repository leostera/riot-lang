(* Two type parameters *)

let map (type a b) (f: a -> b) (x: a) : b = f x

(* Three type parameters *)

let fold (type a b c) (f: a -> b -> c) (x: a) (y: b) : c = f x y

(* Four type parameters *)

let complex (type a b c d) (w: a) (x: b) (y: c) (z: d) = (w, x, y, z)
