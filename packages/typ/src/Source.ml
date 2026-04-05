open Std

type kind =
  | File
  | Fragment
  | Generated

type origin =
  | Path of Path.t
  | Label of string

type t = {
  source_id: SourceId.t;
  kind: kind;
  origin: origin;
  text: string;
  revision: int;
}

let make = fun ~source_id ~kind ~origin ~revision ~text ->
  {
    source_id;
    kind;
    origin;
    text;
    revision;
  }

let update_text = fun source ~revision ~text -> { source with revision; text }

let sanitize_module_name = fun name ->
  String.map (fun ch -> if ch = '-' then '_' else ch) name

let module_name = fun source ->
  let raw_name =
    match source.origin with
    | Path path -> Path.remove_extension path |> Path.basename
    | Label label ->
        label
        |> Path.v
        |> Path.remove_extension
        |> Path.basename
  in
  sanitize_module_name raw_name |> String.capitalize_ascii

let kind_tag = function
  | File -> "file"
  | Fragment -> "fragment"
  | Generated -> "generated"

let input_hash = fun source ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  let () = H.write state (kind_tag source.kind) in
  let () = H.write state "\x1f" in
  let () = H.write state (module_name source) in
  let () = H.write state "\x1f" in
  let () = H.write state source.text in
  H.finish state

let display_name = fun source ->
  match source.origin with
  | Path path -> Path.to_string path
  | Label label -> label
