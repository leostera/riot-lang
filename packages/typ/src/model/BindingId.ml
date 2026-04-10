open Std

type t =
  | Local of {
      stamp: int;
      name: string;
    }
  | Persistent of SurfacePath.t
  | Predef of {
      stamp: int;
      name: string;
    }

let local = fun ~stamp ~name -> Local { stamp; name }

let predef = fun ~stamp ~name -> Predef { stamp; name }

let persistent = fun path -> Persistent path

let name = function
  | Local { name; _ }
  | Predef { name; _ } ->
      name
  | Persistent path -> (
      match SurfacePath.last_name path with
      | Some name -> name
      | None -> SurfacePath.to_string path
    )

let stamp = function
  | Local { stamp; _ }
  | Predef { stamp; _ } ->
      Some stamp
  | Persistent _ ->
      None

let compare = fun left right ->
  match (left, right) with
  | (Local left, Local right) ->
      Int.compare left.stamp right.stamp
  | (Local _, _) ->
      -1
  | (_, Local _) ->
      1
  | (Persistent left, Persistent right) ->
      SurfacePath.compare left right
  | (Persistent _, _) ->
      -1
  | (_, Persistent _) ->
      1
  | (Predef left, Predef right) ->
      Int.compare left.stamp right.stamp

let equal = fun left right -> Int.equal (compare left right) 0

let to_string = function
  | Local { stamp; name } ->
      format Format.[ str name; char '#'; int stamp ]
  | Persistent path ->
      SurfacePath.to_string path
  | Predef { stamp; name } ->
      format Format.[ str "predef("; str name; char '#'; int stamp; char ')' ]
