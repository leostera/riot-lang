open Std

type t = {
  input_hash: Crypto.hash;
  output_hash: Crypto.hash;
  size_bytes: int64;
  files: Manifest.file_entry list;
  ocamlc_warnings: string list;
  exports: Manifest.export_entry list;
}

val serializer: t Serde.Ser.t

val deserializer: t Serde.De.t
