open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

type action_spec = {
  actions : Action.t list;
  outs : Path.t list;
  srcs : Path.t list;
  package : Package.t;
  toolchain : Tusk_toolchain.t;
  hash : Crypto.hash;
}

type t = action_spec G.node

let hash_file path =
  match Fs.read path with
  | Ok contents -> Crypto.hash_string contents
  | Error _ -> Crypto.hash_string (Path.to_string path)

let make ~actions ~outs ~srcs ~(package : Package.t) ~toolchain
    ~dependency_hashes ~deps =
  let open Crypto in
  let hasher = Sha256.create () in

  Log.debug
    "[ACTION_NODE] make: package=%s, #actions=%d, #outs=%d, #srcs=%d, #deps=%d"
    package.Package.name (List.length actions) (List.length outs)
    (List.length srcs) (List.length deps);

  Sha256.write_string hasher package.Package.name;
  Log.debug "[ACTION_NODE]   hash_component: package_name=%s"
    package.Package.name;

  let toolchain_hash = Tusk_toolchain.hash toolchain in
  Sha256.write hasher (Digest.bytes toolchain_hash);
  Log.debug "[ACTION_NODE]   hash_component: toolchain=%s"
    (Digest.hex toolchain_hash);

  let sorted_actions =
    List.sort
      (fun a b -> String.compare (Action.to_string a) (Action.to_string b))
      actions
  in

  Log.debug "[ACTION_NODE]   sorted_actions (first 5):";
  List.iteri
    (fun i action ->
      if i < 5 then
        Log.debug "[ACTION_NODE]     %d. %s" i (Action.to_string action))
    sorted_actions;

  List.iter
    (fun action -> Sha256.write_string hasher (Action.to_string action))
    sorted_actions;

  let sorted_srcs =
    List.sort
      (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
      srcs
  in

  if List.length sorted_srcs > 0 then
    Log.debug "[ACTION_NODE]   sorted_srcs: [%s]"
      (String.concat ", " (List.map Path.to_string sorted_srcs));

  List.iter
    (fun source ->
      let source_hash = hash_file source in
      Log.debug "[ACTION_NODE]     source=%s hash=%s" (Path.to_string source)
        (Digest.hex source_hash);
      Sha256.write hasher (Digest.bytes source_hash))
    sorted_srcs;

  let sorted_outs =
    List.sort
      (fun a b -> String.compare (Path.to_string a) (Path.to_string b))
      outs
  in

  Log.debug "[ACTION_NODE]   sorted_outs (first 5): [%s]"
    (String.concat ", " (List.take 5 sorted_outs |> List.map Path.to_string));

  List.iter
    (fun output -> Sha256.write_string hasher (Path.to_string output))
    sorted_outs;

  let sorted_deps =
    List.sort (fun a b -> G.Node_id.to_int a - G.Node_id.to_int b) deps
  in

  Log.debug "[ACTION_NODE]   sorted_deps: [%s]"
    (String.concat ", " (List.map G.Node_id.to_string sorted_deps));

  List.iter
    (fun dep_id ->
      let dep_hash = dependency_hashes dep_id in
      Log.debug "[ACTION_NODE]     dep=%s hash=%s"
        (G.Node_id.to_string dep_id)
        (Digest.hex dep_hash);
      Sha256.write hasher (Digest.bytes dep_hash))
    sorted_deps;

  let hash = Sha256.finish hasher in

  Log.info "[ACTION_NODE]   FINAL HASH: %s" (Digest.hex hash);

  { actions; outs; srcs; package; toolchain; hash }

let get_hash (node : t) = node.value.hash

let to_json (node : t) =
  let open Data.Json in
  let spec = node.value in
  obj
    [
      ("id", int (G.Node_id.to_int node.id));
      ("actions", array (List.map Action.to_json spec.actions));
      ( "outputs",
        array (List.map (fun p -> string (Path.to_string p)) spec.outs) );
      ( "sources",
        array (List.map (fun p -> string (Path.to_string p)) spec.srcs) );
      ("package", string spec.package.Package.name);
      ("hash", string (Crypto.Digest.hex spec.hash));
      ( "dependencies",
        array (List.map (fun dep -> int (G.Node_id.to_int dep)) node.deps) );
    ]

let equal (n1 : t) (n2 : t) =
  let s1 = n1.value in
  let s2 = n2.value in

  Crypto.Digest.hex s1.hash = Crypto.Digest.hex s2.hash
  && s1.package.Package.name = s2.package.Package.name
  && List.length s1.actions = List.length s2.actions
  && List.length s1.outs = List.length s2.outs
  && List.length s1.srcs = List.length s2.srcs
  && (try List.for_all2 Action.equal s1.actions s2.actions with _ -> false)
  && (try List.for_all2 Path.equal s1.outs s2.outs with _ -> false)
  && try List.for_all2 Path.equal s1.srcs s2.srcs with _ -> false
