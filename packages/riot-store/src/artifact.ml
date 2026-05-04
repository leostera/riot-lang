open Std
open Std.Data
open Std.Collections

type t = {
  input_hash: Std.Crypto.hash;
  output_hash: Std.Crypto.hash;
  size_bytes: int64;
  files: Manifest.file_entry list;
  ocamlc_warnings: string list;
  exports: Manifest.export_entry list;
}

let to_json = fun artifact ->
  let export_to_json (entry: Manifest.export_entry) =
    Json.Object [
      ("name", Json.String entry.name);
      ("path", Json.String (Path.to_string entry.path));
      ("action_hash", Json.String entry.action_hash);
    ]
  in
  Json.Object [
    ("input_hash", Json.String (Crypto.Digest.hex artifact.input_hash));
    ("output_hash", Json.String (Crypto.Digest.hex artifact.output_hash));
    ("size_bytes", Json.String (Int64.to_string artifact.size_bytes));
    (
      "files",
      Json.Array (List.map
        artifact.files
        ~fn:(fun entry ->
          Json.Object [
            ("path", Json.String (Path.to_string entry.Manifest.path));
            ("hash", Json.String entry.Manifest.hash);
            ("size", Json.Int entry.Manifest.size);
          ]))
    );
    (
      "ocamlc_warnings",
      Json.Array (List.map artifact.ocamlc_warnings ~fn:(fun msg -> Json.String msg))
    );
    ("exports", Json.Array (List.map artifact.exports ~fn:export_to_json));
  ]
