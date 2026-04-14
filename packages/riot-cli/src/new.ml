open Std
open Riot_init

let out = eprintln

let no_workspace_message = "Not in a riot workspace. Run `riot init` to create one first"

let command =
  let open ArgParser in
    let open Arg in command "new"
    |> about "Create a new package"
    |> args
      [
        positional "path" |> help "Path for new package";
        flag "lib" |> long "lib" |> help "Create a library package (default)";
        flag "bin" |> long "bin" |> help "Create a binary package";
      ]

let fail = fun message ->
  out ("\027[1;31mError\027[0m: " ^ message);
  Error (Failure message)

let run_without_workspace = fun _matches -> fail no_workspace_message

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

let run = fun matches ->
  let open ArgParser in
    let path = get_one matches "path" |> Option.expect ~msg:"path required" in
    let is_library =
      if get_flag matches "bin" then
        false
      else
        true
    in
    let path_obj =
      match Path.from_string path with
      | Ok p -> p
      | Error _ -> Path.v path
    in
    let name = Path.basename path_obj in
    match Env.current_dir () with
    | Error err -> fail ("Failed to get current directory: " ^ path_error_message err)
    | Ok cwd ->
        let workspace_manager = Riot_model.Workspace_manager.create () in
        (
          match Riot_model.Workspace_manager.scan workspace_manager cwd with
          | Error "No workspace root found" ->
              run_without_workspace matches
          | Error err ->
              fail ("Failed to scan workspace: " ^ err)
          | Ok (workspace, _load_errors) ->
              let package_kind =
                if is_library then
                  "library"
                else
                  "binary"
              in
              println ("Creating new " ^ package_kind ^ " '" ^ name ^ "' in '" ^ path ^ "'");
              match Riot_init.new_package ~workspace ~path:path_obj ~name ~is_library with
              | Ok (created_path, created_name) ->
                  println
                    (String.capitalize_ascii package_kind
                    ^ " '"
                    ^ created_name
                    ^ "' created at '"
                    ^ created_path
                    ^ "'");
                  Ok ()
              | Error e -> fail ("Package creation failed: " ^ e)
        )
