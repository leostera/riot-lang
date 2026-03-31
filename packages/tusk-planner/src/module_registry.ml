open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

type t = (string, G.Node_id.t list) HashMap.t

let create = fun () -> HashMap.create ()

let register = fun t mod_ node_id ->
  let name = Module.module_name mod_ |> Module_name.to_string in
  match HashMap.get t name with
  | None ->
      let _ = HashMap.insert t name [ node_id ] in
      ()
  | Some ids ->
      let _ = HashMap.insert t name (node_id :: ids) in
      ()

let get_by_name = fun t name ->
  match HashMap.get t name with
  | None -> raise Not_found
  | Some ids -> ids
