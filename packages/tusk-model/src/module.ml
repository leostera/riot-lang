(** OCaml module representation for the module graph *)

open Std

type t = {
  module_name : Module_name.t;
  namespace : Namespace.t;
  filename : Path.t;
  kind : [ `implementation | `interface ];
}

let make ~namespace ~filename =
  let mod_name = Module_name.of_filename ~namespace filename in
  let kind =
    match Path.extension filename with
    | Some ".mli" -> `interface
    | Some ".ml" -> `implementation
    | _ -> `implementation
  in
  { module_name = mod_name; namespace; filename; kind }

let module_name t = t.module_name
let namespaced_name t = Module_name.qualified_name t.module_name
let qualified_name t = namespaced_name t
let filename t = t.filename
let kind t = t.kind
let cmi t = Module_name.cmi t.module_name
let cmo t = Module_name.cmo t.module_name
let cmx t = Module_name.cmx t.module_name
let o t = Module_name.o t.module_name
let cmt t = Module_name.cmt t.module_name
let cmti t = Module_name.cmti t.module_name

let eq a b =
  Module_name.qualified_name a.module_name
  = Module_name.qualified_name b.module_name
