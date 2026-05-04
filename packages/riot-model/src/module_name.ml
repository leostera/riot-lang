(** A module name with namespace support *)
open Std

type t = {
  filename: Path.t;
  namespace: Namespace.t;
  name: string;
}

let make = fun ~filename ~namespace ~name -> { filename; namespace; name }

let sanitize_name = fun name ->
  String.map
    ~fn:(fun c ->
      if c = '-' then
        '_'
      else
        c)
    name

let from_filename = fun ?(namespace = Namespace.empty) filename ->
  let name =
    Path.remove_extension filename
    |> Path.basename
    |> sanitize_name
    |> String.capitalize_ascii
  in
  { filename; namespace; name }

let from_string = fun ?(namespace = Namespace.empty) s ->
  let name =
    sanitize_name s
    |> String.capitalize_ascii
  in
  let filename = Path.v s in
  { filename; namespace; name }

let from_path = fun path ->
  let name =
    Path.remove_extension path
    |> Path.basename
    |> sanitize_name
    |> String.capitalize_ascii
  in
  { filename = path; namespace = Namespace.empty; name }

let filename = fun t -> t.filename

let to_string = fun t -> t.name

let namespace = fun t -> t.namespace

let simple_name = fun t -> t.name

let qualified_name = fun t ->
  match Namespace.to_list t.namespace with
  | [] -> t.name
  | ns -> Namespace.to_string (Namespace.append t.namespace t.name)

(* Output file names based on qualified names *)

let cma = fun t ->
  qualified_name t ^ ".cma"
  |> Path.v

let cmxa = fun t ->
  qualified_name t ^ ".cmxa"
  |> Path.v

let cmxs = fun t ->
  qualified_name t ^ ".cmxs"
  |> Path.v

let cmo = fun t ->
  qualified_name t ^ ".cmo"
  |> Path.v

let cmi = fun t ->
  qualified_name t ^ ".cmi"
  |> Path.v

let cmx = fun t ->
  qualified_name t ^ ".cmx"
  |> Path.v

let cmt = fun t ->
  qualified_name t ^ ".cmt"
  |> Path.v

let cmti = fun t ->
  qualified_name t ^ ".cmti"
  |> Path.v

let o = fun t ->
  qualified_name t ^ ".o"
  |> Path.v

let a = fun t ->
  qualified_name t ^ ".a"
  |> Path.v

let canonical_mli = fun t ->
  qualified_name t ^ ".mli"
  |> Path.v

let canonical_ml = fun t ->
  qualified_name t ^ ".ml"
  |> Path.v

let binary = fun t ->
  (* Get the base name without extension from the original filename *)
  Path.remove_extension t.filename
  |> Path.basename
  |> sanitize_name
