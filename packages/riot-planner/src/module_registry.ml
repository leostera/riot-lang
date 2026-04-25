open Std
open Std.Collections
open Riot_model

module G = Std.Graph.SimpleGraph

type t = {
  by_name: (string, G.Node_id.t list) HashMap.t;
  by_qualified_name: (string, G.Node_id.t list) HashMap.t;
}

let create = fun () -> { by_name = HashMap.create (); by_qualified_name = HashMap.create () }

let insert = fun table key node_id ->
  match HashMap.get table ~key with
  | None ->
      let _ = HashMap.insert table ~key ~value:[ node_id ] in ()
  | Some ids ->
      let _ = HashMap.insert table ~key ~value:(node_id :: ids) in ()

let register = fun t mod_ node_id ->
  let name = Module.module_name mod_ |> Module_name.to_string in
  let qualified_name = Module.namespaced_name mod_ in
  let () = insert t.by_name name node_id in insert t.by_qualified_name qualified_name node_id

let register_qualified_name = fun t name node_id ->
  let () = insert t.by_name name node_id in insert t.by_qualified_name name node_id

let get_by_name = fun t name ->
  match HashMap.get t.by_name ~key:name with
  | None -> raise Not_found
  | Some ids -> ids

let get_by_qualified_name = fun t name ->
  match HashMap.get t.by_qualified_name ~key:name with
  | None -> raise Not_found
  | Some ids -> ids
