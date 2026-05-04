open Std
open Std.Collections
open Riot_model

module G = Std.Graph.SimpleGraph

type action_spec = {
  actions: Action.t list;
  outs: Path.t list;
  srcs: Path.t list;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  hash: Crypto.hash;
}

type t = action_spec G.node

let resolve_source_for_hash = fun ~(package:Package.t) ~src_path ->
  let workspace_root_candidate =
    let pkg_path_str = Path.to_string package.path in
    let rel_path_str = Path.to_string package.relative_path in
    if String.length rel_path_str > 0 && String.ends_with ~suffix:rel_path_str pkg_path_str then
      let root_len = String.length pkg_path_str - String.length rel_path_str in
      let raw_root = String.sub pkg_path_str ~offset:0 ~len:root_len in
      let normalized_root =
        if String.ends_with ~suffix:"/" raw_root then
          String.sub raw_root ~offset:0 ~len:(String.length raw_root - 1)
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
  let rec first_existing = fun __tmp1 ->
    match __tmp1 with
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
  match Fs.File.open_read readable_path with
  | Error _ -> Crypto.hash_string (Path.to_string path)
  | Ok file ->
      let state = Crypto.Sha256.create () in
      let reader = Fs.File.to_reader file in
      let buffer = IO.Buffer.create ~size:16_384 in
      let rec loop () =
        IO.Buffer.clear buffer;
        match IO.Reader.read reader ~into:buffer with
        | Ok 0 -> true
        | Ok _ ->
            Crypto.Sha256.write_iovec state (IO.Buffer.to_iovec buffer);
            loop ()
        | Error _ -> false
      in
      let success = loop () in
      let _ = Fs.File.close file in
      if success then
        Crypto.Sha256.finish state
      else
        Crypto.hash_string (Path.to_string path)

let make = fun ~actions ~outs ~srcs ~(package:Package.t) ~toolchain ~dependency_hashes ~deps ->
  let open Crypto in
  let hasher = Sha256.create () in
  Sha256.write hasher (Package_name.to_string package.Package.name);
  let toolchain_hash = Riot_toolchain.hash toolchain in
  Sha256.write_hash hasher toolchain_hash;
  let sorted_actions =
    List.sort
      actions
      ~compare:(fun a b ->
        let hash_a = Action.hash a in
        let hash_b = Action.hash b in
        Crypto.Hash.compare hash_a hash_b)
  in
  List.for_each
    sorted_actions
    ~fn:(fun action ->
      let action_hash = Action.hash action in
      Sha256.write_hash hasher action_hash);
  let sorted_srcs =
    List.sort srcs ~compare:(fun a b -> String.compare (Path.to_string a) (Path.to_string b))
  in
  List.for_each
    sorted_srcs
    ~fn:(fun source ->
      let source_hash = hash_file ~package source in
      Sha256.write_hash hasher source_hash);
  let sorted_outs =
    List.sort outs ~compare:(fun a b -> String.compare (Path.to_string a) (Path.to_string b))
  in
  List.for_each sorted_outs ~fn:(fun output -> Sha256.write hasher (Path.to_string output));
  let sorted_deps =
    List.sort deps ~compare:(fun a b -> Int.compare (G.Node_id.to_int a) (G.Node_id.to_int b))
  in
  List.for_each
    sorted_deps
    ~fn:(fun dep_id ->
      let dep_hash = dependency_hashes dep_id in
      Sha256.write_hash hasher dep_hash);
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
      ("actions", array (List.map spec.actions ~fn:Action.to_json));
      ("outputs", array (List.map spec.outs ~fn:(fun p -> string (Path.to_string p))));
      ("sources", array (List.map spec.srcs ~fn:(fun p -> string (Path.to_string p))));
      ("package", string (Package_name.to_string spec.package.Package.name));
      ("package_path", string (Path.to_string spec.package.Package.path));
      ("package_relative_path", string (Path.to_string spec.package.Package.relative_path));
      ("hash", string (Crypto.Digest.hex spec.hash));
      ("dependencies", array (List.map node.deps ~fn:(fun dep -> int (G.Node_id.to_int dep))));
    ]

let equal = fun (n1: t) (n2: t) ->
  let s1 = n1.value in
  let s2 = n2.value in
  Crypto.Digest.hex s1.hash = Crypto.Digest.hex s2.hash
  && Package_name.equal s1.package.Package.name s2.package.Package.name
  && List.compare_lengths ~left:s1.actions ~right:s2.actions = 0
  && List.compare_lengths ~left:s1.outs ~right:s2.outs = 0
  && List.compare_lengths ~left:s1.srcs ~right:s2.srcs = 0
  && List.all (List.zip s1.actions s2.actions) ~fn:(fun (left, right) -> Action.equal left right)
  && List.all (List.zip s1.outs s2.outs) ~fn:(fun (left, right) -> Path.equal left right)
  && List.all (List.zip s1.srcs s2.srcs) ~fn:(fun (left, right) -> Path.equal left right)
