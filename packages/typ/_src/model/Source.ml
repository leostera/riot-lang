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
  module_name: string;
  implicit_opens: SurfacePath.t list;
  origin: origin;
  source_hash: Crypto.hash;
  revision: int;
  parse_result: Syn.Parser.parse_result;
  cst: Syn.Cst.source_file;
}

let module_name_of_origin = function
  | Path path ->
      Path.remove_extension path
      |> Path.basename
  | Label label ->
      label
      |> Path.v
      |> Path.remove_extension
      |> Path.basename

let sanitize_module_name = fun name ->
  String.map
    (fun ch ->
      if ch = '-' then
        '_'
      else
        ch)
    name

let hash = fun ~implicit_opens ~cst ->
  let module H = Crypto.Sha256 in
  let state = H.create () in
  H.write
    state
    (
      Syn.Cst.semantic_hash cst
      |> Crypto.Digest.hex
    );
  H.write state "\x1f";
  implicit_opens
  |> List.iter
    (fun module_path ->
      H.write state (SurfacePath.to_string module_path);
      H.write state "\x1f");
  H.finish state

let infer_module_name = fun origin ->
  sanitize_module_name (module_name_of_origin origin)
  |> String.capitalize_ascii

let make_prepared = fun
  ~source_id ~kind ~module_name ~implicit_opens ~origin ~revision ~source_hash ~parse_result ~cst ->
  {
    source_id;
    kind;
    module_name;
    implicit_opens;
    origin;
    source_hash;
    revision;
    parse_result;
    cst;
  }

let module_name = fun source -> source.module_name

let input_hash = fun source -> source.source_hash

let display_name = fun source ->
  match source.origin with
  | Path path -> Path.to_string path
  | Label label -> label
