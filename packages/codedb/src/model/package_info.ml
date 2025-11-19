open Std

type t = {
  name : Package_name.t;
  path : Path.t;
}

let make ~name ~path =
  { name; path }

let to_json t =
  Data.Json.Object [
    ("name", Package_name.to_json t.name);
    ("path", Data.Json.String (Path.to_string t.path));
  ]

let from_json json =
  match json with
  | Data.Json.Object fields ->
      (match 
        ( List.assoc_opt "name" fields,
          List.assoc_opt "path" fields )
       with
       | Some name_json, Some (Data.Json.String path) ->
           (match Package_name.from_json name_json with
            | Error e -> Error e
            | Ok name -> Ok { name; path = Path.v path })
       | _ -> Error "Missing required fields in package_info JSON")
  | _ -> Error "Expected object for package_info"
