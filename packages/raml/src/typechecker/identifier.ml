open Std

type t =
  | Local of { name : string; stamp : int }
  | Scoped of { name : string; stamp : int; scope : int }
  | Global of string
  | Predef of { name : string; stamp : int }

type context = { stamp_counter : int }

let create_context () = { stamp_counter = 0 }

let create_scoped ~ctx ~scope name =
  let stamp = ctx.stamp_counter in
  let ctx = { stamp_counter = ctx.stamp_counter + 1 } in
  (Scoped { name; stamp; scope }, ctx)

let create_local ~ctx name =
  let stamp = ctx.stamp_counter in
  let ctx = { stamp_counter = ctx.stamp_counter + 1 } in
  (Local { name; stamp }, ctx)

let create_predef ~ctx name =
  let stamp = ctx.stamp_counter in
  let ctx = { stamp_counter = ctx.stamp_counter + 1 } in
  (Predef { name; stamp }, ctx)

let create_persistent name = Global name

let name = function
  | Local { name; _ } | Scoped { name; _ } | Global name | Predef { name; _ } ->
      name

let rename ~ctx = function
  | Local { name; _ } | Scoped { name; _ } ->
      let stamp = ctx.stamp_counter in
      let ctx = { stamp_counter = ctx.stamp_counter + 1 } in
      (Local { name; stamp }, ctx)
  | id -> (id, ctx)

let unique_name = function
  | Local { name; stamp } | Scoped { name; stamp; _ } ->
      name ^ "_" ^ string_of_int stamp
  | Global name -> name ^ "_0"
  | Predef { name; _ } -> name

let persistent = function Global _ -> true | _ -> false

let equal i1 i2 =
  match (i1, i2) with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ } -> s1 = s2
  | Scoped { stamp = s1; _ }, Scoped { stamp = s2; _ } -> s1 = s2
  | Global n1, Global n2 -> n1 = n2
  | Predef { stamp = s1; _ }, Predef { stamp = s2; _ } -> s1 = s2
  | _ -> false

let same = equal

let compare i1 i2 =
  match (i1, i2) with
  | Local { stamp = s1; _ }, Local { stamp = s2; _ } -> Int.compare s1 s2
  | Scoped { stamp = s1; _ }, Scoped { stamp = s2; _ } -> Int.compare s1 s2
  | Global n1, Global n2 -> String.compare n1 n2
  | Predef { stamp = s1; _ }, Predef { stamp = s2; _ } -> Int.compare s1 s2
  | Local _, _ -> -1
  | _, Local _ -> 1
  | Scoped _, _ -> -1
  | _, Scoped _ -> 1
  | Global _, Predef _ -> -1
  | Predef _, Global _ -> 1

let scope = function
  | Scoped { scope; _ } -> scope
  | Local _ -> 0
  | Global _ -> 0
  | Predef _ -> 0

let to_string id = unique_name id
