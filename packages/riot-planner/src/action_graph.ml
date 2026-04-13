open Std
open Std.Sync
open Std.Collections
open Std.Iter
open Riot_model
module G = Std.Graph.SimpleGraph

type t = {
  graph: Action_node.action_spec G.t;
}

let create = fun () -> { graph = G.make () }

let add_node = fun t node_value ->
  G.add_node t.graph node_value

let add_dependency = fun t node ~depends_on -> G.add_edge node ~depends_on

let topo_sort = fun t ->
  match G.topo_sort t.graph with
  | Ok sorted -> sorted
  | Error _cycle_ids ->
      (* Should never happen - cycles caught in module planning *)
      panic "Unexpected cycle in action graph"

let nodes = fun t -> topo_sort t

let graph = fun t -> t.graph

let to_action_list = fun t ->
  let sorted = topo_sort t in
  sorted |> List.map ~fn:(fun (node: Action_node.t) -> node.value.actions) |> List.concat

let hash_action_node = fun _t (node: Action_node.t) -> node.value.hash

let opens = fun mods ->
  List.filter_map
    mods
    ~fn:(fun (node: Module_node.t G.node) ->
      match node.value.kind with
      | ML mod_
      | MLI mod_ -> Some (Riot_toolchain.Ocamlc.Open (Module.namespaced_name mod_))
      | _ -> None)

(** Determine compiler flags for stdlib handling based on package dependencies *)
let stdlib_flags = fun (package: Package.t) ->
  (* Check if this package has stdlib as a dependency *)
  let has_stdlib_dep =
    List.any
      (Package.build_graph_dependencies package)
      ~fn:(fun (dep: Package.dependency) -> dep.name = "stdlib")
  in
  (* Always add -nopervasives to prevent automatic opening of Stdlib *)
  (* Add -nostdlib only if package doesn't depend on stdlib *)
  if has_stdlib_dep then
    [ Riot_toolchain.Ocamlc.NoPervasives ]
  else
    [ Riot_toolchain.Ocamlc.NoPervasives; Riot_toolchain.Ocamlc.NoStdlib ]

let profile_compile_flags = fun (profile: Profile.t) ->
  let flags = [] in
  let flags =
    if profile.no_alias_deps then
      Riot_toolchain.Ocamlc.NoAliasDeps :: flags
    else
      flags
  in
  let flags =
    match profile.inline with
    | Some threshold -> flags @ [ Riot_toolchain.Ocamlc.Inline threshold ]
    | None -> flags
  in
  let flags =
    if profile.no_assert then
      flags @ [ Riot_toolchain.Ocamlc.NoAssert ]
    else
      flags
  in
  let flags =
    if profile.compact then
      flags @ [ Riot_toolchain.Ocamlc.Compact ]
    else
      flags
  in
  let flags =
    if profile.unsafe then
      flags @ [ Riot_toolchain.Ocamlc.Unsafe ]
    else
      flags
  in
  let flags =
    if List.is_empty profile.warnings then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.Warning profile.warnings ]
  in
  let flags =
    if List.is_empty profile.errors then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.WarnError profile.errors ]
  in
  let compare_compiler_flag = fun left right ->
    let rec compare_flag_parts = fun left right ->
      match (left, right) with
      | ([], []) -> 0
      | ([], _) -> -1
      | (_, []) -> 1
      | (left :: left_rest, right :: right_rest) ->
          let compared = String.compare left right in
          if compared = 0 then
            compare_flag_parts left_rest right_rest
          else
            compared
    in
    compare_flag_parts
      (Riot_toolchain.Ocamlc.flags_to_string [ left ])
      (Riot_toolchain.Ocamlc.flags_to_string [ right ])
  in
  List.unique
    (flags
    @ List.map profile.open_modules ~fn:(fun mod_name -> Riot_toolchain.Ocamlc.Open mod_name)
    @ List.map profile.ocamlc_flags ~fn:(fun flag -> Riot_toolchain.Ocamlc.Raw flag))
    ~compare:compare_compiler_flag

let module_to_actions ~package ~profile ~ctx ~dep_includes ~get_dep_outputs ~get_dep_kind ~depset ~needs_unix ~needs_dynlink (
  module_node: Module_node.t
) (deps: G.Node_id.t list):
  Action.t list * Path.t list * Path.t list =
  let base_compile_flags = stdlib_flags package @ profile_compile_flags profile in
  match module_node with
  | { kind=MLI mod_; file=Concrete path; open_modules; _ } ->
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [ path ] in
      let compile = Action.CompileInterface {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules
      } in
      ([ compile ], outputs, sources)
  | { kind=ML mod_; file=Concrete path; open_modules; _ } ->
      let native_object_output = Module.o mod_ in
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output; native_object_output ] in
      let sources = [ path ] in
      let compile = Action.CompileImplementation {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules
      } in
      ([ compile ], outputs, sources)
  | { kind=ML mod_; file=Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { destination = path; content = contents } in
      let native_object_output = Module.o mod_ in
      let cmx_output = Module.cmx mod_ in
      let cmi_output = Module.cmi mod_ in
      let cmt_output = Module.cmt mod_ in
      let outputs = [ cmt_output; cmi_output; cmx_output; native_object_output ] in
      let sources = [] in
      let is_alias_file = String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path) in
      let flags =
        if is_alias_file then
          base_compile_flags
          @ (Riot_toolchain.Ocamlc.Impl path :: Riot_toolchain.Ocamlc.NoAliasDeps :: opens open_modules)
        else
          base_compile_flags @ opens open_modules
      in
      let compile_action = Action.CompileImplementation {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags
      } in
      ([ write_action; compile_action ], outputs, sources)
  | { kind=MLI mod_; file=Generated { path; contents }; open_modules; _ } ->
      let write_action = Action.WriteFile { destination = path; content = contents } in
      let cmi_output = Module.cmi mod_ in
      let cmti_output = Module.cmti mod_ in
      let outputs = [ cmti_output; cmi_output ] in
      let sources = [] in
      let compile_action = Action.CompileInterface {
        source = path;
        outputs;
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags @ opens open_modules
      } in
      ([ write_action; compile_action ], outputs, sources)
  | { kind=Native { files }; _ } ->
      let c_files =
        List.filter files ~fn:(fun path -> String.ends_with ~suffix:".c" (Path.to_string path))
      in
      (* Use cc_flags from the profile (already has target-specific flags applied) *)
      let base_ccflags = profile.Profile.cc_flags in
      (* Add sysroot if cross-compiling *)
      let ccflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None -> base_ccflags
      in
      Log.debug
        ("[ACTION_GRAPH] C compilation cc_flags for " ^ package.name ^ ": " ^ String.concat " " ccflags);
      let actions =
        List.map c_files ~fn:(fun c_file ->
          let obj_file = Path.remove_extension c_file |> Path.add_extension ~ext:"o" in
          let output_name = Path.basename obj_file |> Path.v in
          Action.CompileC { source = c_file; outputs = [ output_name ]; ccflags })
      in
      let outputs =
        List.map c_files ~fn:(fun c_file ->
          let obj_file = Path.remove_extension c_file |> Path.add_extension ~ext:"o" in
          Path.basename obj_file |> Path.v)
      in
      (actions, outputs, files)
  | { kind=C; file=Concrete _; _ }
  | { kind=C; file=Generated _; _ }
  | { kind=H; _ }
  | { kind=Root; _ }
  | { kind=Other _; _ } ->
      ([], [], [])
  | { kind=Library { name; includes }; _ } ->
      let library_name = Module_name.(of_string name |> cmxa) in
      let archive_name = Module_name.(of_string name |> a) in
      let sources = [] in
      let objects_with_duplicates =
        List.map deps ~fn:(fun dep_id ->
          let dep_outputs = get_dep_outputs dep_id in
          match get_dep_kind dep_id with
          | Some (Module_node.Native _) ->
              List.filter dep_outputs ~fn:(fun output -> Path.extension output = Some ".o")
          | _ ->
              List.filter dep_outputs ~fn:(fun output -> Path.extension output = Some ".cmx"))
        |> List.concat
      in
      (* Deduplicate objects without reordering them: OCaml link order must stay
         topological, with dependencies before dependents. *)
      let seen_objects = HashSet.create () in
      let objects =
        List.filter_map objects_with_duplicates ~fn:(fun object_path ->
          if HashSet.insert seen_objects ~value:object_path then
            Some object_path
          else
            None)
      in
      (* Create static library metadata (.cmxa). *)
      let create_lib = Action.CreateLibrary {
        outputs = [ library_name; archive_name ];
        objects;
        includes
      } in
      let all_outputs = [ library_name; archive_name ] in
      ([ create_lib ], all_outputs, sources)
  | { kind=Binary { name; source; libraries; includes }; _ } ->
      let binary_mod = Module.make ~namespace:Namespace.empty ~filename:source in
      let binary_cmx = Module.cmx binary_mod in
      let sources = [ source ] in
      let compile_action = Action.CompileImplementation {
        source;
        outputs = [ binary_cmx ];
        includes = Path.v "." :: dep_includes;
        flags = base_compile_flags
      } in
      (* Collect foreign library outputs for linking *)
      let cclibs =
        List.map package.foreign_dependencies ~fn:(fun (fdep: Package.foreign_dependency) ->
          (* Make foreign outputs absolute by joining with foreign dep path and normalizing *)
          List.map fdep.outputs ~fn:(fun out -> Path.normalize (Path.join fdep.path out)))
        |> List.concat
      in
      let binary_output = Path.v name in
      (* Use cc_flags and ld_flags from the profile (already has target-specific flags applied) *)
      let base_ccflags = profile.Profile.cc_flags in
      let base_ldflags = profile.Profile.ld_flags in
      (* Get target platform for looking up dependency flags *)
      let target_platform = Build_ctx.target_platform_name ctx in
      (* NOTE: Dependency ld_flags must be collected here during linking, not in the profile.
         The profile contains only the current package's target-specific flags (applied in 
         package_planner). When linking, we need flags from ALL dependencies transitively,
         which can only be determined at link-time based on the depset. *)
      let transitive_deps = Dependency.transitive_closure depset in
      let dep_ldflags =
        List.map transitive_deps ~fn:(fun (dep: Dependency.t) ->
          match
            List.find dep.package.compiler.target_overrides
              ~fn:(fun (platform, _) -> String.equal platform target_platform)
          with
          | Some (_, target_override) -> (
              match target_override.profile_override with
              | Some override -> (
                  match override.ld_flags with
                  | Profile.Override flags -> flags
                  | Inherit -> []
                )
              | None -> []
            )
          | None -> [])
        |> List.concat
      in
      (* Combine: current package + dependencies *)
      let merged_ldflags = base_ldflags @ dep_ldflags in
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ package.name
        ^ ": base ld_flags = ["
        ^ String.concat ", " base_ldflags
        ^ "]");
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ package.name
        ^ ": dependency ld_flags = ["
        ^ String.concat ", " dep_ldflags
        ^ "]");
      Log.debug
        ("[ACTION_GRAPH] Package "
        ^ package.name
        ^ ": merged ld_flags = ["
        ^ String.concat ", " merged_ldflags
        ^ "]");
      (* Add sysroot if cross-compiling *)
      let ccflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: base_ccflags
        | None -> base_ccflags
      in
      let ldflags =
        match Build_ctx.sysroot ctx with
        | Some sysroot ->
            let sysroot_flag = "--sysroot=" ^ Path.to_string sysroot in
            sysroot_flag :: merged_ldflags
        | None -> merged_ldflags
      in
      Log.debug
        ("[ACTION_GRAPH] Final ccopt_flags for linking " ^ name ^ ": " ^ String.concat " " ccflags);
      Log.debug
        ("[ACTION_GRAPH] Final cclib_flags for linking " ^ name ^ ": " ^ String.concat " " ldflags);
      (* Keep ccopt_flags and cclib_flags separate *)
      let ccopt_flags = ccflags in
      let cclib_flags = ldflags in
      let link_action = Action.CreateExecutable {
        outputs = [ binary_output ];
        objects = [ binary_cmx ];
        libraries;
        includes;
        cclibs;
        ccopt_flags;
        cclib_flags;
      }
      in
      ([ compile_action; link_action ], [ binary_output ], sources)

let from_module_graph ~package ~profile ~ctx ~toolchain ~store ~depset ~needs_unix ~needs_dynlink (
  module_graph: Module_node.t G.t
):
  t * Path.t list =
  let transitive_deps = Dependency.transitive_closure depset in
  (* Extract dependency cache include paths - no file copying needed! *)
  let dep_cache_includes =
    List.map transitive_deps ~fn:(fun (dep: Dependency.t) -> dep.artifact_dir)
  in
  (* Add stdlib includes if needed *)
  let stdlib_includes = (
    if needs_unix then
      [ Path.v "+unix" ]
    else
      []
  ) @ (
    if needs_dynlink then
      [ Path.v "+dynlink" ]
    else
      []
  )
  in
  let dep_includes = stdlib_includes @ dep_cache_includes in
  let action_graph = create () in
  let node_mapping = HashMap.create () in
  let action_spec_hashes = HashMap.create () in
  let node_outputs = HashMap.create () in
  let all_outputs = Cell.create [] in
  let sorted_modules =
    match G.topo_sort module_graph with
    | Ok sorted -> sorted
    | Error _cycle_ids ->
        (* Cycle should have been caught earlier in module planning *)
        panic "Unexpected cycle in action graph - should have been caught in module planning"
  in
  let get_dep_hash dep_id =
    match HashMap.get action_spec_hashes ~key:dep_id with
    | Some h -> h
    | None -> panic
      ("Dependency hash not found for node " ^ G.Node_id.to_string dep_id ^ ". Graph not in topological order!")
  in
  let get_dep_outputs dep_id =
    match HashMap.get node_outputs ~key:dep_id with
    | Some outs -> outs
    | None -> []
  in
  let module_kinds = HashMap.create () in
  List.for_each sorted_modules ~fn:(fun (module_node: Module_node.t G.node) ->
      let _ = HashMap.insert module_kinds ~key:module_node.id ~value:module_node.value.kind in
      ())
  ;
  let get_dep_kind dep_id = HashMap.get module_kinds ~key:dep_id in
  List.for_each sorted_modules ~fn:(fun (module_node: Module_node.t G.node) ->
      let actions, outputs, sources = module_to_actions
        ~package
        ~profile
        ~ctx
        ~dep_includes
        ~get_dep_outputs
        ~get_dep_kind
        ~depset
        ~needs_unix
        ~needs_dynlink
        module_node.value
        module_node.deps in
      let _ = HashMap.insert node_outputs ~key:module_node.id ~value:outputs in
      if actions = [] then
        let placeholder_hash = Crypto.hash_string
          ("no-actions:" ^ G.Node_id.to_string module_node.id) in
        let _ = HashMap.insert action_spec_hashes ~key:module_node.id ~value:placeholder_hash in
        ()
      else
        let action_spec = Action_node.make
          ~actions
          ~outs:outputs
          ~srcs:sources
          ~package
          ~toolchain
          ~dependency_hashes:get_dep_hash
          ~deps:module_node.deps in
        let action_node = add_node action_graph action_spec in
        List.for_each module_node.deps ~fn:(fun dep_id ->
            match HashMap.get node_mapping ~key:dep_id with
            | Some dep_action_node -> add_dependency action_graph action_node ~depends_on:dep_action_node
            | None -> ())
        ;
        let _ = HashMap.insert node_mapping ~key:module_node.id ~value:action_node in
        let _ = HashMap.insert action_spec_hashes ~key:module_node.id ~value:action_spec.hash in
        Cell.set all_outputs (outputs @ Cell.get all_outputs))
  ;
  (action_graph, Cell.get all_outputs)

let to_json = fun t ->
  let open Data.Json in
    let all_nodes = nodes t in
    let sorted_nodes =
      List.sort
        all_nodes
        ~compare:(fun (a: Action_node.t) (b: Action_node.t) -> G.Node_id.to_int a.id - G.Node_id.to_int b.id)
    in
    obj [ ("nodes", array (List.map sorted_nodes ~fn:Action_node.to_json)) ]

let from_json = fun json ->
  let open Data.Json in
    let parse_hash hash_json =
      let module Byte_buf = IO.Bytes in
      let hex_value c =
        if c >= '0' && c <= '9' then
          Some (Char.code c - Char.code '0')
        else if c >= 'a' && c <= 'f' then
          Some (10 + Char.code c - Char.code 'a')
        else if c >= 'A' && c <= 'F' then
          Some (10 + Char.code c - Char.code 'A')
        else
          None
      in
      match hash_json with
      | String hex ->
          let len = String.length hex in
          if not (len mod 2 = 0) then
            Error "hash must have even-length hex"
          else
            let out = Byte_buf.create ~size:(len / 2) in
            let rec fill i =
              if i >= len then
                Ok (Crypto.Hash.of_bytes out)
              else
                match
                  (
                    hex |> String.get_unchecked ~at:i |> hex_value,
                    hex |> String.get_unchecked ~at:(i + 1) |> hex_value
                  )
                with
                | Some hi, Some lo ->
                    let byte = Char.from_int_unchecked ((hi lsl 4) lor lo) in
                    let _ = Byte_buf.set out ~at:(i / 2) ~char:byte in
                    fill (i + 2)
                | _ -> Error "hash contains non-hex characters"
            in
            fill 0
      | _ -> Error "hash must be string"
    in
    match get_field "nodes" json with
    | None ->
        Error "Missing 'nodes' field"
    | Some (Array node_jsons) -> (
        let graph = create () in
        let id_to_node: (int, Action_node.t) HashMap.t = HashMap.create () in
        let dependencies_to_wire: (Action_node.t * int list) vec = vec [] in
        let parse_actions actions_json =
          match actions_json with
          | Array action_jsons ->
              List.fold_left
                action_jsons
                ~acc:(Ok [])
                ~fn:(fun acc action_json ->
                  match acc with
                  | Error _ -> acc
                  | Ok actions -> (
                      match Action.from_json action_json with
                      | Ok action -> Ok (action :: actions)
                      | Error err -> Error err
                    ))
              |> Result.map ~fn:List.reverse
          | _ -> Error "actions must be array"
        in
        let parse_paths paths_json =
          match paths_json with
          | Array path_jsons ->
              Ok (
                List.filter_map
                  path_jsons
                  ~fn:(
                    function
                    | String s -> Some (Path.v s)
                    | _ -> None
                  )
              )
          | _ -> Error "paths must be array"
        in
        let parse_dependencies deps_json =
          match deps_json with
          | Array dep_jsons ->
              List.fold_left
                dep_jsons
                ~acc:(Ok [])
                ~fn:(fun acc dep_json ->
                  match (acc, dep_json) with
                  | Error e, _ -> Error e
                  | Ok deps, Int dep_id -> Ok (dep_id :: deps)
                  | Ok _, _ -> Error "dependencies must be int array")
              |> Result.map ~fn:List.reverse
          | _ -> Error "dependencies must be array"
        in
        match
          List.fold_left
            node_jsons
            ~acc:(Ok ())
            ~fn:(fun acc node_json ->
              match acc with
              | Error _ -> acc
              | Ok () -> (
                  match (
                    get_field "package" node_json,
                    get_field "package_path" node_json,
                    get_field "package_relative_path" node_json,
                    get_field "actions" node_json,
                    get_field "outputs" node_json,
                    get_field "sources" node_json,
                    get_field "hash" node_json,
                    get_field "id" node_json,
                    get_field "dependencies" node_json
                  ) with
                  | (Some (String pkg_name), pkg_path_json, pkg_rel_path_json, Some actions_json, Some outputs_json, Some sources_json, Some hash_json, Some (Int legacy_id), Some deps_json) -> (
                      match parse_actions actions_json with
                      | Error err -> Error err
                      | Ok actions -> (
                          match (
                            parse_hash hash_json,
                            parse_paths outputs_json,
                            parse_paths sources_json,
                            parse_dependencies deps_json
                          ) with
                          | Ok hash, Ok outputs, Ok sources, Ok dependency_ids ->
                              let package_path =
                                match pkg_path_json with
                                | Some (String p) -> Path.v p
                                | _ -> Path.v "."
                              in
                              let package_relative_path =
                                match pkg_rel_path_json with
                                | Some (String p) -> Path.v p
                                | _ -> package_path
                              in
                              let package = Package.synthetic
                                ~name:pkg_name
                                ~path:package_path
                                ~relative_path:package_relative_path in
                              let toolchain = Riot_toolchain.init
                                ~config:Riot_model.Toolchain_config.default
                              |> Result.expect ~msg:"Failed to initialize toolchain" in
                              let action_spec: Action_node.action_spec = {
                                actions;
                                outs = outputs;
                                srcs = sources;
                                package;
                                toolchain;
                                hash;
                              }
                              in
                              let node = add_node graph action_spec in
                              let _ = HashMap.insert id_to_node ~key:legacy_id ~value:node in
                              Vector.push dependencies_to_wire ~value:(node, dependency_ids);
                              Ok ()
                          | (Error err, _, _, _)
                          | (_, Error err, _, _)
                          | (_, _, Error err, _)
                          | (_, _, _, Error err) -> Error err
                        )
                    )
                  | _ -> Error "Missing required node fields"
                ))
        with
        | Error err -> Error err
        | Ok () ->
            Vector.iter dependencies_to_wire |> Iterator.to_list |> List.for_each ~fn:(fun (node, dependency_ids) ->
                List.for_each (List.reverse dependency_ids) ~fn:(fun dep_id ->
                    match HashMap.get id_to_node ~key:dep_id with
                    | Some dep_node -> add_dependency graph node ~depends_on:dep_node
                    | None -> ())
              );
            Ok graph
      )
    | Some _ ->
        Error "nodes must be array"

let equal = fun g1 g2 ->
  let nodes1 = topo_sort g1 in
  let nodes2 = topo_sort g2 in
  List.compare_lengths ~left:nodes1 ~right:nodes2 = 0
  && List.all (List.zip nodes1 nodes2) ~fn:(fun (left, right) -> Action_node.equal left right)
