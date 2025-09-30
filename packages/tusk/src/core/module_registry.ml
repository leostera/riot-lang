(** Module registry for tracking module graph nodes by name *)

open Std
open Model

type t = {
  modules : (Graph.SimpleGraph.Node_id.t, Module.t) Hashtbl.t;
  intf_by_name : (string, Graph.SimpleGraph.Node_id.t) Hashtbl.t;
  impl_by_name : (string, Graph.SimpleGraph.Node_id.t) Hashtbl.t;
}

let create () = {
  modules = Hashtbl.create 16;
  intf_by_name = Hashtbl.create 16;
  impl_by_name = Hashtbl.create 16;
}

let register t mod_ node_id =
  Hashtbl.add t.modules node_id mod_;
  let mod_name = Module.name mod_ |> Module_name.to_string in
  let table =
    match Module.kind mod_ with
    | Module.Implementation -> t.impl_by_name
    | Module.Interface -> t.intf_by_name
  in
  Hashtbl.add table mod_name node_id

let get t node_id = Hashtbl.find t.modules node_id

let get_by_name t name =
  let nodes = ref [] in
  (match Hashtbl.find_opt t.intf_by_name name with
  | Some node -> nodes := node :: !nodes
  | None -> ());
  (match Hashtbl.find_opt t.impl_by_name name with
  | Some node -> nodes := node :: !nodes
  | None -> ());
  match !nodes with
  | [] -> raise Not_found
  | nodes -> nodes
