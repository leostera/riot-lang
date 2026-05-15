open Std
open Std.Result.Syntax
open Riot_model

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "toolchain"
  |> about "Manage OCaml toolchains"
  |> subcommands
    [
      command "list"
      |> about "List toolchains for this project";
      command "install"
      |> about "Install all missing toolchains";
      command "list-available"
      |> about "List published toolchains available for install";
    ]

let print_toolchain_status = fun info ->
  let open Riot_toolchain in
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
  println
    ("  "
    ^ status_icon
    ^ " "
    ^ Riot_model.Target.to_string info.target
    ^ host_label
    ^ " - "
    ^ status_text)

let run_list = fun workspace ->
  let config = Toolchain_config.from_root ~root:workspace.Workspace_manifest.root in
  let toolchains = Riot_toolchain.list_toolchains ~config in
  println "";
  println ("OCaml " ^ config.version ^ " toolchains for this project:");
  println "";
  List.for_each toolchains ~fn:print_toolchain_status;
  let missing_count =
    List.filter
      toolchains
      ~fn:(fun info ->
        match info.Riot_toolchain.status with
        | NotInstalled _
        | Incomplete _ -> true
        | Installed _ -> false)
    |> List.length
  in
  if missing_count > 0 then (
    println "";
    println "To add/remove targets, edit ocaml-toolchain.toml";
    println
      ("Use 'riot toolchain install' to install "
      ^ Int.to_string missing_count
      ^ " missing toolchain(s)")
  );
  Ok ()

let run_install = fun workspace ->
  let config = Toolchain_config.from_root ~root:workspace.Workspace_manifest.root in
  println "";
  println ("Installing OCaml " ^ config.version ^ " toolchains...");
  println "";
  match Riot_toolchain.install_all_toolchains ~config with
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

type available_toolchain_row = {
  version: string;
  host: Riot_model.Target.t;
  target: Riot_model.Target.t;
}

let sort_available_toolchain_rows = fun rows ->
  List.sort
    rows
    ~compare:(fun left right ->
      let by_version = String.compare right.version left.version in
      if by_version != Order.EQ then
        by_version
      else
        let by_host =
          String.compare
            (Riot_model.Target.to_string left.host)
            (Riot_model.Target.to_string right.host)
        in
        if by_host != Order.EQ then
          by_host
        else
          String.compare
            (Riot_model.Target.to_string left.target)
            (Riot_model.Target.to_string right.target))

let max_int = fun left right ->
  if left > right then
    left
  else
    right

let pad_right = fun width value ->
  let padding = max_int 0 (width - String.length value) in
  value ^ String.make ~len:padding ~char:' '

let available_toolchain_rows = fun toolchains ->
  toolchains
  |> List.map
    ~fn:(fun (toolchain: Riot_toolchain.available_toolchain) -> {
      version = toolchain.version;
      host = toolchain.host;
      target = toolchain.target;
    })
  |> sort_available_toolchain_rows

let table_widths = fun rows ->
  List.fold_left
    rows
    ~init:(String.length "version", String.length "host", String.length "target")
    ~fn:(fun (version_width, host_width, target_width) row ->
      let host = Riot_model.Target.to_string row.host in
      let target = Riot_model.Target.to_string row.target in
      (
        max_int version_width (String.length row.version),
        max_int host_width (String.length host),
        max_int target_width (String.length target)
      ))

let print_available_toolchain_table = fun toolchains ->
  let rows = available_toolchain_rows toolchains in
  let (version_width, host_width, target_width) = table_widths rows in
  let separator =
    String.make ~len:version_width ~char:'-'
    ^ "  "
    ^ String.make ~len:host_width ~char:'-'
    ^ "  "
    ^ String.make ~len:target_width ~char:'-'
  in
  println
    (pad_right version_width "version"
    ^ "  "
    ^ pad_right host_width "host"
    ^ "  "
    ^ pad_right target_width "target");
  println separator;
  List.for_each
    rows
    ~fn:(fun row ->
      let host = Riot_model.Target.to_string row.host in
      let target = Riot_model.Target.to_string row.target in
      println
        (pad_right version_width row.version ^ "  " ^ pad_right host_width host ^ "  " ^ target))

let run_list_available = fun () ->
  match Riot_toolchain.list_available_toolchains () with
  | Ok [] ->
      println "No published OCaml toolchains found.";
      Ok ()
  | Ok toolchains ->
      println "";
      println "Published OCaml toolchains:";
      println "";
      print_available_toolchain_table toolchains;
      Ok ()
  | Error msg ->
      eprintln ("❌ " ^ msg);
      Error (Failure msg)

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

let load_workspace_manifest = fun () ->
  let* cwd =
    Env.current_dir ()
    |> Result.map_err ~fn:(fun err -> Failure ("Failed to get cwd: " ^ path_error_message err))
  in
  let workspace_manager = Workspace_manager.create () in
  Workspace_manager.scan workspace_manager cwd
  |> Result.map_err
    ~fn:(fun err ->
      Failure ("Failed to scan workspace: " ^ Workspace_manager.scan_error_message err))

let run = fun matches ->
  let open ArgParser in
  match get_subcommand matches with
  | Some ("list-available", _) -> run_list_available ()
  | Some ("list", _) ->
      let* (workspace, _) = load_workspace_manifest () in
      run_list workspace
  | Some ("install", _) ->
      let* (workspace, _) = load_workspace_manifest () in
      run_install workspace
  | _ ->
      println "Usage: riot toolchain <list|install|list-available>";
      Error (Failure "Unknown subcommand")
