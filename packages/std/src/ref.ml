open Global
open Sync

type 'a t =
  Ref of int64 [@@unboxed]

let __current__ = Atomic.make 0L

let rec make = fun () ->
    let last = Atomic.get __current__ in
    let current = last |> Int64.succ in
    if Atomic.compare_and_set __current__ last current then
      Ref last
    else
      make ()

let equal = fun (Ref a) (Ref b) ->
    Int64.equal a b

let type_equal : type a b. a t -> b t -> (a, b) Type.eq option = fun a b ->
    match (a, b) with
    | Ref a', Ref b' when Int64.equal a' b' -> Some (Kernel.dangerous_unsafe_cast Type.Equal)
    | _ -> None

let cast : type a b. (a, b) Type.eq -> a -> b = fun (Type.Equal) a -> a

let cast : type a b. a t -> b t -> a -> b option = fun a b value ->
    match type_equal a b with
    | Some witness -> Some (cast witness value)
    | None -> None

let is_newer = fun (Ref a) (Ref b) -> Int64.compare a b = 1

let hash = fun (Ref a) -> Int64.hash a
