open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

type kind =
  | ML of Module.t
  | MLI of Module.t
  | C
  | H
  | Other of string
  | Root
  | Native of { files : Path.t list; }
  | Library of { name : string; includes : Path.t list; }
  | Binary of { name : string; source : Path.t; libraries : Path.t list; includes : Path.t list; }

type file =
  | Concrete of Path.t
  | Generated of { path : Path.t; contents : string; }

type t = {
  file : file;
  mutable open_modules : t G.node list;
  kind : kind;
}

let file_to_string = fun file ->
  match file with
  | Concrete path -> Path.to_string path
  | Generated { path; _ } -> Path.to_string path ^ " (generated)"

let make_ml = fun mod_ file -> {file; open_modules = []; kind = ML mod_}

let make_mli = fun mod_ file -> {file; open_modules = []; kind = MLI mod_}

let make_c = fun path -> {file = Concrete path; open_modules = []; kind = C}

let make_h = fun path -> {file = Concrete path; open_modules = []; kind = H}

let make_root = fun () -> {file = Concrete (Path.v ""); open_modules = []; kind = Root}

let make_library = fun ~name ~includes ->
  {file = Concrete (Path.v ""); open_modules = []; kind = Library {name; includes}; }

let make_native = fun ~files ->
  {file = Concrete (Path.v "native"); open_modules = []; kind = Native {files}; }

let make_binary = fun ~name ~source ~libraries ~includes ->
  {file = Concrete source; open_modules = []; kind = Binary {name; source; libraries; includes}; }

let set_open_modules = fun t modules -> t.open_modules <- modules

let kind_to_string =
  function
  | ML mod_ -> "ML(" ^ (Module.module_name mod_ |> Module_name.to_string) ^ ")"
  | MLI mod_ -> "MLI(" ^ (Module.module_name mod_ |> Module_name.to_string) ^ ")"
  | C -> "C"
  | H -> "H"
  | Other s -> "Other(" ^ s ^ ")"
  | Root -> "Root"
  | Native { files } -> "Native(" ^ Int.to_string (List.length files) ^ " files)"
  | Library { name; _ } -> "Library(" ^ name ^ ")"
  | Binary { name; _ } -> "Binary(" ^ name ^ ")"
