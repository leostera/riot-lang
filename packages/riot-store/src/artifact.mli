open Std

type t = {
  input_hash: Crypto.hash;
  output_hash: Crypto.hash;
  files: Manifest.file_entry list;
  ocamlc_warnings: string list;
  exports: Manifest.export_entry list;
}

val to_json: t -> Data.Json.t
