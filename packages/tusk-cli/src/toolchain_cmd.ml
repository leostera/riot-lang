open Std
open Tusk_model

let command =
  let open ArgParser in
    let open Arg in command "toolchain"
    |> about "Manage OCaml toolchains"
    |> subcommands
    [
      command "list" |> about "List toolchains for this project";
      command "install" |> about "Install all missing toolchains";

    ]

let print_toolchain_status = fun info ->
  let open Tusk_toolchain in
    let status_icon =
      match info.status with
      | Installed _ -> "✓"
      | NotInstalled _ -> "✗"
      | Incomplete _ -> "⚠"
    in
    let status_text =
      match info.status with
      | Installed _ -> "ready"
      | NotInstalled _ -> "not installed"
      | Incomplete { missing; _ } -> "incomplete (missing: " ^ String.concat ", " missing ^ ")"
    in
    let host_label =
      if info.is_host then
        " (host)"
      else
        ""
    in
    println ("  " ^ status_icon ^ " " ^ info.target ^ host_label ^ " - " ^ status_text)

let run_list = fun workspace ->
  let config = Toolchain_config.from_workspace workspace in
  let toolchains = Tusk_toolchain.list_toolchains ~config in
  println "";
  println ("OCaml " ^ config.version ^ " toolchains for this project:");
  println "";
  List.iter print_toolchain_status toolchains;
  let missing_count =
    List.filter
      (fun info ->
        match info.Tusk_toolchain.status with
        | NotInstalled _
        | Incomplete _ -> true
        | Installed _ -> false)
      toolchains
    |> List.length
  in
  if missing_count > 0 then
    (
      println "";
      println "To add/remove targets, edit ocaml-toolchain.toml";
      println
      ("Use 'tusk toolchain install' to install " ^ Int.to_string missing_count ^ " missing toolchain(s)")
    );
  Ok ()

let run_install = fun workspace ->
  let config = Toolchain_config.from_workspace workspace in
  println "";
  println ("Installing OCaml " ^ config.version ^ " toolchains...");
  println "";
  match Tusk_toolchain.install_all_toolchains ~config with
  | Ok (installed, skipped) ->
      println "";
      if installed = 0 then
        println "All toolchains already installed!"
      else
        println
        ("All toolchains installed! ("
        ^ Int.to_string installed
        ^ " new, "
        ^ Int.to_string skipped
        ^ " existing)");
        Ok ()
  | Error msg ->
      println "";
      println ("❌ " ^ msg);
      Error (Failure msg)

let run = fun matches ->
  let open ArgParser in
    let cwd = Env.current_dir () |> Result.expect ~msg:"Failed to get cwd" in
    let (workspace, _) = Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace" in
    match get_subcommand matches with
    | Some ("list", _) ->
        run_list workspace
    | Some ("install", _) ->
        run_install workspace
    | _ ->
        println "Usage: tusk toolchain <list|install>";
        Error (Failure "Unknown subcommand")
