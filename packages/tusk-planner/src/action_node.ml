open Std
open Std.Collections
open Tusk_model

module G = Std.Graph.SimpleGraph

type action_spec = {
  actions : Action.t list;
  outs : Path.t list;
  srcs : Path.t list;
  package : Package.t;
  toolchain : Toolchains.toolchain;
  hash : Crypto.hash;
}

type t = action_spec G.node

let hash_file path =
  match Fs.read path with
  | Ok contents -> Crypto.hash_string contents
  | Error _ -> Crypto.hash_string (Path.to_string path)

let make ~actions ~outs ~srcs ~(package : Package.t) ~toolchain ~dependency_hashes ~deps = 
  let open Crypto in
  let hasher = Sha256.create () in
  
  Sha256.write_string hasher package.Package.name;
  
  let toolchain_hash = Toolchains.hash toolchain in
  Sha256.write hasher (Digest.bytes toolchain_hash);
  
  let sorted_actions = List.sort (fun a b ->
    String.compare (Action.to_string a) (Action.to_string b)
  ) actions in
  
  List.iter (fun action ->
    Sha256.write_string hasher (Action.to_string action)
  ) sorted_actions;
  
  let sorted_srcs = List.sort (fun a b ->
    String.compare (Path.to_string a) (Path.to_string b)
  ) srcs in
  
  List.iter (fun source ->
    let source_hash = hash_file source in
    Sha256.write hasher (Digest.bytes source_hash)
  ) sorted_srcs;
  
  let sorted_outs = List.sort (fun a b ->
    String.compare (Path.to_string a) (Path.to_string b)
  ) outs in
  
  List.iter (fun output ->
    Sha256.write_string hasher (Path.to_string output)
  ) sorted_outs;
  
  let sorted_deps = List.sort (fun a b -> 
    G.Node_id.to_int a - G.Node_id.to_int b
  ) deps in
  
  List.iter (fun dep_id ->
    let dep_hash = dependency_hashes dep_id in
    Sha256.write hasher (Digest.bytes dep_hash)
  ) sorted_deps;
  
  let hash = Sha256.finish hasher in
  
  { actions; outs; srcs; package; toolchain; hash }

let get_hash (node : t) = node.value.hash
