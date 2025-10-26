open Std
open Tusk_model
module G = Std.Graph.SimpleGraph

type kind =
  | ML of Module.t
  | MLI of Module.t
  | C
  | H
  | Other of string
  | Root
  | Native of { files : Path.t list }
  | Library of { name : string; includes : Path.t list }
  | Binary of {
      name : string;
      source : Path.t;
      libraries : Path.t list;
      includes : Path.t list;
    }

type file =
  | Concrete of Path.t
  | Generated of { path : Path.t; contents : string }

type t = { file : file; mutable open_modules : t G.node list; kind : kind }

let file_to_string file =
  match file with
  | Concrete path -> Path.to_string path
  | Generated { path; _ } -> Path.to_string path ^ " (generated)"

let make_ml mod_ file = { file; open_modules = []; kind = ML mod_ }
let make_mli mod_ file = { file; open_modules = []; kind = MLI mod_ }
let make_c path = { file = Concrete path; open_modules = []; kind = C }
let make_h path = { file = Concrete path; open_modules = []; kind = H }

let make_root () =
  { file = Concrete (Path.v ""); open_modules = []; kind = Root }

let make_library ~name ~includes =
  {
    file = Concrete (Path.v "");
    open_modules = [];
    kind = Library { name; includes };
  }

let make_native ~files =
  {
    file = Concrete (Path.v "native");
    open_modules = [];
    kind = Native { files };
  }

let make_binary ~name ~source ~libraries ~includes =
  {
    file = Concrete source;
    open_modules = [];
    kind = Binary { name; source; libraries; includes };
  }

let set_open_modules t modules = t.open_modules <- modules

let kind_to_string = function
  | ML mod_ -> format "ML(%s)" (Module.module_name mod_ |> Module_name.to_string)
  | MLI mod_ ->
      format "MLI(%s)" (Module.module_name mod_ |> Module_name.to_string)
  | C -> "C"
  | H -> "H"
  | Other s -> format "Other(%s)" s
  | Root -> "Root"
  | Native { files } -> format "Native(%d files)" (List.length files)
  | Library { name; _ } -> format "Library(%s)" name
  | Binary { name; _ } -> format "Binary(%s)" name
