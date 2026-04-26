open Std

type t = { owner: string; local_id: int }

let compare = fun left right ->
  match String.compare left.owner right.owner with
  | 0 -> Int.compare left.local_id right.local_id
  | other -> other

let equal = fun left right ->
  String.equal left.owner right.owner && Int.equal left.local_id right.local_id

let make = fun ~owner ~local_id -> { owner; local_id }

let owner = fun value -> value.owner

let local_id = fun value -> value.local_id

let of_path = fun path -> make ~owner:("$path:" ^ SurfacePath.to_string path) ~local_id:0

let of_int = fun value -> make ~owner:"$legacy" ~local_id:value

let to_int = fun value -> value.local_id

let to_json = fun value ->
  Data.Json.Object [
    ("owner", Data.Json.String value.owner);
    ("local_id", Data.Json.Int value.local_id);
  ]

let of_json = function
  | Data.Json.Object fields -> (
      match (List.assoc_opt "owner" fields, List.assoc_opt "local_id" fields) with
      | (Some (Data.Json.String owner), Some (Data.Json.Int local_id)) -> Ok (make ~owner ~local_id)
      | (Some _, Some _) -> Error "expected type constructor id owner:string and local_id:int"
      | _ -> Error "missing type constructor id owner/local_id fields"
    )
  | Data.Json.Int value ->
      Ok (of_int value)
  | other ->
      Error (
        "expected type constructor id object but got " ^ (
          match other with
          | Data.Json.Null -> "null"
          | Data.Json.Bool _ -> "bool"
          | Data.Json.Int _ -> "int"
          | Data.Json.Float _ -> "float"
          | Data.Json.String _ -> "string"
          | Data.Json.Array _ -> "array"
          | Data.Json.Object _ -> "object"
          | Data.Json.Embed _ -> "embed"
        )
      )

let to_string = fun type_constructor_id ->
  type_constructor_id.owner ^ "#" ^ Int.to_string type_constructor_id.local_id
