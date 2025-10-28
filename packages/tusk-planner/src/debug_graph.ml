open Std
open Std.Data
open Tusk_model

let main ~args =
  let cmd =
    ArgParser.command "debug-graph"
    |> ArgParser.about "Debug module and action graphs"
    |> ArgParser.arg
         (ArgParser.Arg.positional "package"
         |> ArgParser.Arg.required true
         |> ArgParser.Arg.help "Package name to debug")
  in

  let matches =
    ArgParser.get_matches cmd Env.args
    |> Result.expect ~msg:"Failed to parse arguments"
  in

  let package_name =
    ArgParser.get_one matches "package"
    |> Option.expect ~msg:"Package name required"
  in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Could not get current directory"
  in

  let workspace_root =
    Workspace_manager.find_workspace_root cwd
    |> Option.expect ~msg:"Could not find workspace root"
  in

  let workspace =
    Workspace_manager.scan workspace_root
    |> Result.expect ~msg:"Could not scan workspace"
  in

  let package =
    List.find_opt
      (fun (pkg : Package.t) -> pkg.name = package_name)
      workspace.packages
    |> Option.expect ~msg:(format "Package %s not found" package_name)
  in

  println "=== Package: %s ===" package.name;
  println "Path: %s" (Path.to_string package.path);
  println "";

  let toolchain =
    Tusk_toolchain.init ~config:Tusk_model.Toolchain_config.default
    |> Result.expect ~msg:"Could not init toolchain"
  in

  let store = Tusk_store.Store.create ~workspace in
  let package_graph = Tusk_planner.Package_graph.create workspace in
  let result =
    Tusk_planner.Package_planner.plan_package ~workspace ~toolchain ~store
      ~package_graph ~package
    |> Result.expect ~msg:"Planning failed"
  in

  match result with
  | Tusk_planner.Package_planner.Planned { module_graph; action_graph; _ } ->
      let nodes =
        Graph.SimpleGraph.map module_graph ~fn:(fun (id, node) ->
            let module_node = node.value in
            Json.obj
              [
                ("id", Json.int (Graph.SimpleGraph.Node_id.to_int id));
                ( "kind",
                  Json.string
                    (Tusk_planner.Module_node.kind_to_string module_node.kind)
                );
                ( "file",
                  Json.string
                    (Tusk_planner.Module_node.file_to_string module_node.file)
                );
                ( "deps",
                  Json.array
                    (List.map
                       (fun dep_id ->
                         Json.int (Graph.SimpleGraph.Node_id.to_int dep_id))
                       node.deps) );
              ])
      in
      let module_json = Json.obj [ ("nodes", Json.array nodes) ] in
      println "%s" (Json.to_string module_json);
      Ok ()

let () = Miniriot.run ~main ~args:Env.args
