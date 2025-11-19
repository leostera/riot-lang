open Std

let command =
  let open ArgParser in
  let open Arg in
  command "serve"
  |> about "Start Suri development server with auto-reload"
  |> args [
      positional "package"
      |> help "Package name (optional, inferred from current directory)"
      |> required false
    ]

let run matches =
  (* Get package name from args or infer from cwd *)
  let package_name = match ArgParser.get_value matches "package" with
    | Some pkg -> pkg
    | None ->
        (* Infer from current directory *)
        match Env.current_dir () with
        | Ok cwd -> Path.basename cwd
        | Error _ -> "app"
  in
  
  (* Load workspace *)
  let workspace = match Env.current_dir () with
    | Error e -> Error e
    | Ok cwd ->
        match Tusk_model.Workspace_manager.scan cwd with
        | Error e -> Error e
        | Ok (ws, _) -> Ok ws
  in
  let workspace = Result.expect workspace ~msg:"Not in a workspace" in
  
  (* Find package *)
  let package = Tusk_model.Workspace.find_package workspace package_name
    |> Option.expect ~msg:("Package not found: " ^ package_name) in
  
  (* Find a binary to run (first one, or one matching package name) *)
  let binary = match package.binaries with
    | [] -> failwith "Package has no binaries"
    | bins ->
        List.find_opt (fun (b : Tusk_model.Binary.t) -> b.name = package_name) bins
        |> Option.or_else (fun () -> Some (List.hd bins))
  in
  let binary = Option.expect binary ~msg:"No binary found" in
  
  (* Setup config *)
  let config = Dev_orchestrator.{
    package_name;
    watch_paths = [Path.join package.path "src"];
    binary_path = Path.join workspace.build_dir 
      (package_name ^ "/" ^ binary.name);
    binary_args = [];
  } in
  
  (* Start dev server *)
  Dev_orchestrator.start config
