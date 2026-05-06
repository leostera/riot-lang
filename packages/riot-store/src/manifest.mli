(** Manifest for tracking stored build artifacts *)
open Std

type version =
  | V2
type file_entry = {
  path: Path.t;
  hash: string;
  size: int;
}
type export_entry = {
  name: string;
  path: Path.t;
  action_hash: string;
}
type t = {
  version: version;
  package: string;
  input_hash: string;
  output_hash: string;
  timestamp: Std.Time.SystemTime.t;
  size_bytes: int64;
  files: file_entry list;
  ocamlc_warnings: string list;
  exports: export_entry list;
}
type metadata = {
  input_hash: string;
  output_hash: string;
  size_bytes: int64;
  ocamlc_warnings: string list;
  exports: export_entry list;
}

val create:
  ?base_dir:Path.t ->
  ?ocamlc_warnings:string list ->
  ?exports:export_entry list ->
  unit ->
  package:string ->
  input_hash:string ->
  files:(Path.t * int) list ->
  t

(**
   Create a manifest for stored files. Takes a list of (file_path, size) pairs
   and calculates hashes.
*)
val save: t -> path:Path.t -> (unit, string) result

(** Save manifest to a JSON file *)
val load: path:Path.t -> (t, string) result

(** Load manifest from a JSON file *)
val load_metadata: path:Path.t -> (metadata, string) result

(** Load manifest metadata without decoding the stored file list. *)
val metadata_to_string: metadata -> (string, string) result

val metadata_of_string: string -> (metadata, string) result

val to_json: t -> Std.Data.Json.t

(** Convert manifest to JSON *)
val from_json: Std.Data.Json.t -> (t, string) result

(** Parse manifest from JSON *)
