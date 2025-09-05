(** Module tree structure for organizing sources semantically *)

open Std

type gen_kind =
  | Static of { contents : string; path : Path.t }
  | Dynamic of { path : Path.t }

type module_info =
  | Generated of {
      simple_name : string;
      kind : gen_kind;
    }
  | Concrete of {
      simple_name : string;
      namespaced_name : string;
      impl : Build_node.source option;
      intf : Build_node.source option;
    }

type package_kind =
  | Library (* builds to .cma/.cmxa *)
  | Binary of { src : Path.t; name : string }
(* builds to executable *)

type t =
  | Package of {
      name : string;
      kind : package_kind;
      entry_point : module_info option;
          (* main.ml for Binary, lib.ml for Library *)
      children : t list;
      aliases : module_info list; (* generated alias modules for this level *)
    }
  | Library of {
      (* represents a folder/subfolder *)
      name : Mod_name.t;
      folder_interface : module_info option; (* e.g., cli/cli.ml *)
      children : t list;
      aliases : module_info list; (* generated alias modules for this level *)
    }
  | Module of module_info (* leaf node - just a .ml/.mli file *)

(** Iterate over all nodes in a module tree *)
let rec iter f tree =
  match tree with
  | Package { children; aliases; _ } ->
      List.iter f (List.map (fun a -> Module a) aliases);
      List.iter (iter f) children
  | Library { children; aliases; folder_interface; _ } ->
      List.iter f (List.map (fun a -> Module a) aliases);
      (match folder_interface with Some info -> f (Module info) | None -> ());
      List.iter (iter f) children
  | Module _ as m -> f m

(** Print a module tree for debugging *)
let rec print ?(indent = "") tree =
  match tree with
  | Package { name; kind; entry_point; children; aliases } ->
      Format.eprintf "%sPackage: %s (kind=%s, entry=%s, %d aliases)@." indent
        name
        (match kind with Binary _ -> "Binary" | Library -> "Library")
        (match entry_point with Some _ -> "yes" | None -> "no")
        (List.length aliases);
      List.iter (print ~indent:(indent ^ "  ")) children
  | Library { name; folder_interface; children; aliases } ->
      Format.eprintf "%sLibrary: %s -> %s (interface=%s, %d aliases)@." indent
        (Mod_name.module_name name)
        (Mod_name.qualified_name name)
        (match folder_interface with Some _ -> "yes" | None -> "no")
        (List.length aliases);
      List.iter (print ~indent:(indent ^ "  ")) children
  | Module (Generated { simple_name; kind }) ->
      let kind_str = match kind with
        | Static _ -> "(static)"
        | Dynamic _ -> "(dynamic)"
      in
      Format.eprintf "%sGenerated: %s %s@." indent simple_name kind_str
  | Module (Concrete { simple_name; namespaced_name; impl; intf }) ->
      Format.eprintf "%sModule: %s -> %s (impl=%s, intf=%s)@." indent
        simple_name namespaced_name
        (match impl with Some _ -> "yes" | None -> "no")
        (match intf with Some _ -> "yes" | None -> "no")
