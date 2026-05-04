(** OCaml module representation for the module graph *)
open Std

type t = {
  module_name: Module_name.t;
  namespace: Namespace.t;
  filename: Path.t;
  kind: [`implementation | `interface];
}

let make = fun ~namespace ~filename ->
  let mod_name = Module_name.from_filename ~namespace filename in
  let kind =
    match Path.extension filename with
    | Some ".mli" -> `interface
    | Some ".ml" -> `implementation
    | _ -> `implementation
  in
  {
    module_name = mod_name;
    namespace;
    filename;
    kind;
  }

let module_name = fun t -> t.module_name

let namespaced_name = fun t -> Module_name.qualified_name t.module_name

let qualified_name = fun t -> namespaced_name t

let filename = fun t -> t.filename

let kind = fun t -> t.kind

let cmi = fun t -> Module_name.cmi t.module_name

let cmo = fun t -> Module_name.cmo t.module_name

let cmx = fun t -> Module_name.cmx t.module_name

let o = fun t -> Module_name.o t.module_name

let cmt = fun t -> Module_name.cmt t.module_name

let cmti = fun t -> Module_name.cmti t.module_name

let eq = fun a b ->
  Module_name.qualified_name a.module_name = Module_name.qualified_name b.module_name
