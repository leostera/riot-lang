open Std
open Tusk_model

module G = Std.Graph.SimpleGraph

type t = (string, G.Node_id.t list) Hashtbl.t

let create () = Hashtbl.create 32

let register t mod_ node_id =
  let name = Module.module_name mod_ |> Module_name.to_string in
  match Hashtbl.find_opt t name with
  | None -> Hashtbl.add t name [node_id]
  | Some ids -> Hashtbl.replace t name (node_id :: ids)

let get_by_name t name =
  match Hashtbl.find_opt t name with
  | None -> raise Not_found
  | Some ids -> ids
