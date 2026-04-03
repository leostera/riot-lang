open Std

type install_script_source =
  | Local of Path.t
  | Remote of string

let out = eprintln

let default_install_script_url = "https://cdn.pkgs.ml/riot/install.sh"

let command =
  let open ArgParser in
  let open Arg in
  command "upgrade"
  |> about "Upgrade riot by rerunning the published installer"
  |> args
    [
      option "version" |> long "version" |> help "Version to install (default: latest)";
    ]

let path_error_message = function
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> "system call '"
  ^ syscall
  ^ "' returned invalid UTF-8 path: "
  ^ path
  | Path.SystemError error -> error

let installer_env = fun ?version () ->
  match version with
  | Some version -> [ ("RIOT_VERSION", version) ]
  | None -> []

let command_failed_message = fun ~action ~cmd status ->
  action ^ " failed with status " ^ Int.to_string status ^ ": " ^ Command.to_string cmd

let run_install_script = fun ?(env = []) ?version ~script_path () ->
  let cmd = Command.make "sh" ~args:[ Path.to_string script_path ] ~env:(env @ installer_env ?version ()) in
  match Command.status cmd with
  | Ok 0 -> Ok ()
  | Ok status -> Error (command_failed_message ~action:"installer" ~cmd status)
  | Error (Command.SystemError msg) -> Error ("failed to execute installer: " ^ msg)

let run_downloader = fun ~program ~args ~script_url ->
  let cmd = Command.make program ~args in
  match Command.output cmd with
  | Ok { status = 0; _ } -> Ok ()
  | Ok { status; stderr; _ } ->
      let detail =
        if String.equal (String.trim stderr) "" then
          command_failed_message ~action:program ~cmd status
        else
          program ^ " failed with status " ^ Int.to_string status ^ ": " ^ String.trim stderr
      in
      Error detail
  | Error (Command.SystemError msg) ->
      Error ("failed to start " ^ program ^ " while downloading " ^ script_url ^ ": " ^ msg)

let download_install_script = fun ~script_url ~dst ->
  let dst_str = Path.to_string dst in
  match run_downloader ~program:"curl" ~args:[ "-fsSL"; "-o"; dst_str; script_url ] ~script_url with
  | Ok () -> Ok ()
  | Error curl_error when String.contains curl_error "failed to start curl" -> (
      match run_downloader ~program:"wget" ~args:[ "-q"; "-O"; dst_str; script_url ] ~script_url with
      | Ok () -> Ok ()
      | Error wget_error when String.contains wget_error "failed to start wget" ->
          Error "riot upgrade requires curl or wget to download the installer"
      | Error wget_error -> Error wget_error
    )
  | Error error -> Error error

let resolve_install_script_source = fun () ->
  match Env.var Env.String ~name:"RIOT_INSTALL_SCRIPT_PATH" with
  | Some raw_path -> (
      match Path.of_string raw_path with
      | Ok path -> Ok (Local path)
      | Error err -> Error ("invalid RIOT_INSTALL_SCRIPT_PATH: " ^ path_error_message err)
    )
  | None ->
      let script_url =
        Env.var Env.String ~name:"RIOT_INSTALL_SCRIPT_URL"
        |> Option.unwrap_or ~default:default_install_script_url
      in
      Ok (Remote script_url)

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let run = fun matches ->
  let version = ArgParser.get_one matches "version" in
  match resolve_install_script_source () with
  | Error message ->
      out ("\027[1;31mError\027[0m: " ^ message);
      Error (Failure message)
  | Ok (Local script_path) -> (
      match run_install_script ?version ~script_path () with
      | Ok () -> Ok ()
      | Error message ->
          out ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
    )
  | Ok (Remote script_url) -> (
      out ("  \027[1;32mUpgrading\027[0m riot");
      match with_tempdir_result "riot-upgrade" (fun tempdir ->
        let script_path = Path.(tempdir / Path.v "install.sh") in
        match download_install_script ~script_url ~dst:script_path with
        | Error _ as err -> err
        | Ok () -> run_install_script ?version ~script_path ()) with
      | Ok () -> Ok ()
      | Error message ->
          out ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
    )
