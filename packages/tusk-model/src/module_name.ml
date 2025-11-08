(** A module name with namespace support *)

open Std

type t = { filename : Path.t; namespace : Namespace.t; name : string }

let make ~filename ~namespace ~name = { filename; namespace; name }
let sanitize_name name = String.map (fun c -> if c = '-' then '_' else c) name

let of_filename ?(namespace = Namespace.empty) filename =
  let name =
    Path.remove_extension filename
    |> Path.basename |> sanitize_name |> String.capitalize_ascii
  in
  { filename; namespace; name }

let of_string ?(namespace = Namespace.empty) s =
  let name = sanitize_name s |> String.capitalize_ascii in
  let filename =
    Path.of_string s
    |> Result.expect ~msg:("Expected '" ^ s ^ "' to be a valid Path")
  in
  { filename; namespace; name }

let of_path path =
  let name =
    Path.remove_extension path |> Path.basename |> sanitize_name
    |> String.capitalize_ascii
  in
  { filename = path; namespace = Namespace.empty; name }

let filename t = t.filename
let to_string t = t.name
let namespace t = t.namespace

let qualified_name t =
  match Namespace.to_list t.namespace with
  | [] -> t.name
  | ns -> Namespace.to_string (Namespace.append t.namespace t.name)

(* Output file names based on qualified names *)
let cma t = qualified_name t ^ ".cma" |> Path.v
let cmxa t = qualified_name t ^ ".cmxa" |> Path.v
let cmo t = qualified_name t ^ ".cmo" |> Path.v
let cmi t = qualified_name t ^ ".cmi" |> Path.v
let cmx t = qualified_name t ^ ".cmx" |> Path.v
let cmt t = qualified_name t ^ ".cmt" |> Path.v
let cmti t = qualified_name t ^ ".cmti" |> Path.v
let o t = qualified_name t ^ ".o" |> Path.v
let a t = qualified_name t ^ ".a" |> Path.v
let canonical_mli t = qualified_name t ^ ".mli" |> Path.v
let canonical_ml t = qualified_name t ^ ".ml" |> Path.v
