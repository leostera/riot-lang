open Std
open Std.Data
  open Std.Collections

type t = { hash : Std.Crypto.hash; files : Std.Path.t list }

let to_json artifact =
  Json.Object
    [
      ("hash", Json.String (Crypto.Digest.hex artifact.hash));
      ( "files",
        Json.Array
          (List.map (fun p -> Json.String (Path.to_string p)) artifact.files) );
    ]
