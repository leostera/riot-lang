open Std
open Std.Collections
open Tusk_model
module G = Std.Graph.SimpleGraph

type action_spec = {
  actions: Action.t list;
  outs: Path.t list;
  srcs: Path.t list;
  package: Package.t;
  toolchain: Tusk_toolchain.t;
  hash: Crypto.hash;
}

type t = action_spec G.node

let resolve_source_for_hash = fun ~(package:Package.t) ~src_path ->
  let workspace_root_candidate =
    let pkg_path_str = Path.to_string package.path in
    let rel_path_str = Path.to_string package.relative_path in
    if String.length rel_path_str > 0 && String.ends_with ~suffix:rel_path_str pkg_path_str then
      let root_len = String.length pkg_path_str - String.length rel_path_str in
      let raw_root = String.sub pkg_path_str 0 root_len in
      let normalized_root =
        if String.ends_with ~suffix:"/" raw_root then
          String.sub raw_root 0 (String.length raw_root - 1)
        else
          raw_root
      in
      if String.length normalized_root = 0 then
        Some (Path.v ".")
      else
        Some (Path.v normalized_root)
    else
      None
  in
  let candidates =
    if Path.is_absolute src_path then
      [ src_path ]
    else
      let package_relative = [ Path.join package.path src_path ] in
      let workspace_relative =
        match workspace_root_candidate with
        | Some root -> package_relative @ [ Path.join root src_path ]
        | None -> package_relative
      in
      workspace_relative @ [ src_path ]
  in
  let rec first_existing = function
    | [] -> src_path
    | path :: rest -> (
        match Fs.exists path with
        | Ok true -> path
        | Ok false
        | Error _ -> first_existing rest
      )
  in
  first_existing candidates

let hash_file = fun ~(package:Package.t) path ->
  let readable_path = resolve_source_for_hash ~package ~src_path:path in
  match Fs.read readable_path with
  | Ok contents -> Crypto.hash_string contents
  | Error _ -> Crypto.hash_string (Path.to_string path)

let make = fun ~actions ~outs ~srcs ~(package:Package.t) ~toolchain ~dependency_hashes ~deps ->
  let open Crypto in
    let hasher = Sha256.create () in
    Sha256.write_string hasher package.Package.name;
    let toolchain_hash = Tusk_toolchain.hash toolchain in
    Sha256.write hasher (Digest.bytes toolchain_hash);
    let sorted_actions =
      List.sort
        (fun a b ->
          let hash_a = Action.hash a in
          let hash_b = Action.hash b in
          Crypto.Hash.compare hash_a hash_b)
        actions
    in
    List.iter
      (fun action ->
        let action_hash = Action.hash action in
        Sha256.write hasher (Digest.bytes action_hash))
      sorted_actions;
    let sorted_srcs =
      List.sort
        (fun a b ->
          String.compare (Path.to_string a) (Path.to_string b))
        srcs
    in
    List.iter
      (fun source ->
        let source_hash = hash_file ~package source in
        Sha256.write hasher (Digest.bytes source_hash))
      sorted_srcs;
    let sorted_outs =
      List.sort
        (fun a b ->
          String.compare (Path.to_string a) (Path.to_string b))
        outs
    in
    List.iter
      (fun output ->
        Sha256.write_string hasher (Path.to_string output))
      sorted_outs;
    let sorted_deps =
      List.sort (fun a b -> G.Node_id.to_int a - G.Node_id.to_int b) deps
    in
    List.iter
      (fun dep_id ->
        let dep_hash = dependency_hashes dep_id in
        Sha256.write hasher (Digest.bytes dep_hash))
      sorted_deps;
    let hash = Sha256.finish hasher in
    {
      actions;
      outs;
      srcs;
      package;
      toolchain;
      hash;
    }

let get_hash = fun (node: t) -> node.value.hash

let to_json = fun (node: t) ->
  let open Data.Json in
    let spec = node.value in
    obj
      [
        ("id", int (G.Node_id.to_int node.id));
        ("actions", array (List.map Action.to_json spec.actions));
        ("outputs", array (List.map (fun p -> string (Path.to_string p)) spec.outs));
        ("sources", array (List.map (fun p -> string (Path.to_string p)) spec.srcs));
        ("package", string spec.package.Package.name);
        ("package_path", string (Path.to_string spec.package.Package.path));
        ("package_relative_path", string (Path.to_string spec.package.Package.relative_path));
        ("hash", string (Crypto.Digest.hex spec.hash));
        ("dependencies", array (List.map (fun dep -> int (G.Node_id.to_int dep)) node.deps));
      ]

let equal = fun (n1: t) (n2: t) ->
  let s1 = n1.value in
  let s2 = n2.value in
  Crypto.Digest.hex s1.hash = Crypto.Digest.hex s2.hash
  && s1.package.Package.name = s2.package.Package.name
  && List.length s1.actions = List.length s2.actions
  && List.length s1.outs = List.length s2.outs
  && List.length s1.srcs = List.length s2.srcs
  && (
    try List.for_all2 Action.equal s1.actions s2.actions with
    | _ -> false
  )
  && (
    try List.for_all2 Path.equal s1.outs s2.outs with
    | _ -> false
  )
  && try List.for_all2 Path.equal s1.srcs s2.srcs with
  | _ -> false
