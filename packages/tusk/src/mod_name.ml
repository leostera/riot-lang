(** A module name, including namespace support *)

open Std

type namespace = string list
type t = { filename : Path.t; namespace : namespace; name : string }

let namespace_separator = "__"

(* Namespace functions *)
let namespace_of_string s =
  if s = "" then []
  else String.split_on_char '/' s |> List.map String.capitalize_ascii

let namespace_of_path p = Path.to_string p |> namespace_of_string
let namespace_of_list l = List.map String.capitalize_ascii l
let namespace_append ns component = ns @ [ String.capitalize_ascii component ]
let namespace_to_list ns = ns

(* ModName functions *)
let make ~filename ~namespace ~name = { filename; namespace; name }

let of_filename ?(namespace = []) filename =
  let path_str = Path.to_string filename in
  let name =
    Filename.basename path_str |> Filename.remove_extension
    |> String.capitalize_ascii
  in
  { filename; namespace; name }

let of_string ?(namespace = []) s =
  let name = String.capitalize_ascii s in
  let filename =
    Path.of_string s
    |> Result.expect ~msg:(Printf.sprintf "Expected '%s' to be a valid Path" s)
  in
  { filename; namespace; name }

let filename t = t.filename
let module_name t = t.name
let namespace t = t.namespace

let qualified_name t =
  String.concat namespace_separator (t.namespace @ [ t.name ])

(* Output file names based on qualified names *)
let cmo t = qualified_name t ^ ".cmo"
let cmi t = qualified_name t ^ ".cmi"
let cmx t = qualified_name t ^ ".cmx"
let o t = qualified_name t ^ ".o"
let canonical_mli t = qualified_name t ^ ".mli"
let canonical_ml t = qualified_name t ^ ".ml"
