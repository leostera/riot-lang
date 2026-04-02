open Std
open Std.Data
open Std.Collections

type t = {
  hash: Std.Crypto.hash;
  files: Std.Path.t list;
  ocamlc_warnings: string list;
}

let to_json = fun artifact ->
  Json.Object [
    ("hash", Json.String (Crypto.Digest.hex artifact.hash));
    ("files", Json.Array (List.map (fun p -> Json.String (Path.to_string p)) artifact.files));
    ("ocamlc_warnings", Json.Array (List.map (fun msg -> Json.String msg) artifact.ocamlc_warnings));
  ]
