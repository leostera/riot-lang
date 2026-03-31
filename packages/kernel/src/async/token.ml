type t

let unsafe_to_value = fun (x:t) -> Obj.magic x

let unsafe_to_int : t -> int = fun t -> unsafe_to_value t

let hash = fun t -> Int.hash (unsafe_to_int t)

let equal = fun ?eq a b ->
  match eq with
  | Some f -> f (unsafe_to_value a) (unsafe_to_value b)
  | None -> Int.equal (unsafe_to_int a) (unsafe_to_int b)

let make : 'whatever -> t = fun x -> Obj.magic x
