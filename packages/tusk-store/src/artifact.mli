open Std

type t = {
  hash: Crypto.hash;
  files: Path.t list;
  ocamlc_warnings: string list;
}
val to_json: t -> Data.Json.t
