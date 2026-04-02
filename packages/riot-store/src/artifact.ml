open Std
open Std.Data
open Std.Collections

type t = {
  hash: Std.Crypto.hash;
  files: Std.Path.t list;
  ocamlc_warnings: string list;
  exports: Manifest.export_entry list;
}

let to_json = fun artifact ->
  let export_to_json (entry: Manifest.export_entry) = Json.Object [
    ("name", Json.String entry.name);
    ("path", Json.String (Path.to_string entry.path));
    ("action_hash", Json.String entry.action_hash);
  ] in
  Json.Object [
    ("hash", Json.String (Crypto.Digest.hex artifact.hash));
    ("files", Json.Array (List.map (fun p -> Json.String (Path.to_string p)) artifact.files));
    ("ocamlc_warnings", Json.Array (List.map (fun msg -> Json.String msg) artifact.ocamlc_warnings));
    ("exports", Json.Array (List.map export_to_json artifact.exports));
  ]
