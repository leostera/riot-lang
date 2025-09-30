type t

let unsafe_to_value (x : t) = Obj.magic x
let unsafe_to_int (t : t) : int = unsafe_to_value t
let hash t = Int.hash (unsafe_to_int t)

let equal ?eq a b =
  match eq with
  | Some f -> f (unsafe_to_value a) (unsafe_to_value b)
  | None -> Int.equal (unsafe_to_int a) (unsafe_to_int b)

let pp fmt t = Format.fprintf fmt "Token(%d)" (unsafe_to_int t)
let make (x : 'whatever) : t = Obj.magic x
