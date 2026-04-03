open Std
open Riot_model

let ( let* ) = Result.and_then

let registry_name = "pkgs.ml"

let command =
  let open ArgParser in
    let open Arg in command "login"
    |> about "Save your pkgs.ml API token"
    |> args [ option "token" |> long "token" |> help "API token to save without prompting"; ]

let fail = fun message -> Error (Failure message)

let config_path = fun () -> Riot_dirs.config_path ()

let ensure_riot_dirs = fun () ->
  Riot_dirs.ensure_created () |> Result.map_error (fun exn -> Failure (Exception.to_string exn))

let load_config = fun path ->
  match Fs.exists path with
  | Error io_error -> fail
    ("failed to read config '" ^ Path.to_string path ^ "': " ^ IO.error_message io_error)
  | Ok false -> Ok User_config.default
  | Ok true -> User_config.load path
  |> Result.map_error (fun err -> Failure (User_config.message err))

let save_config = fun path config ->
  User_config.save config path |> Result.map_error (fun err -> Failure (User_config.message err))

let prompt_for_token = fun () ->
  print "pkgs.ml API token: ";
  match Tty.make ~stdin:IO.stdin () with
  | Error Tty.NoTtyConnected ->
      fail "failed to read API token: no tty connected"
  | Error (Tty.SystemError io_error) ->
      fail ("failed to read API token: " ^ IO.error_message io_error)
  | Ok tty -> (
      match Tty.read_line tty with
      | Error io_error ->
          Tty.restore tty;
          fail ("failed to read API token: " ^ IO.error_message io_error)
      | Ok token ->
          Tty.restore tty;
          let token = String.trim token in
          if String.equal token "" then
            fail "API token cannot be empty"
          else
            Ok token
    )

let run = fun matches ->
  let token_arg = ArgParser.get_one matches "token" in
  let* () = ensure_riot_dirs () in
  let* token =
    match token_arg with
    | Some token when not (String.equal (String.trim token) "") -> Ok (String.trim token)
    | Some _ -> fail "API token cannot be empty"
    | None -> prompt_for_token ()
  in
  let path = config_path () in
  let* config = load_config path in
  let config = User_config.set_api_token config ~registry_name token in
  let* () = save_config path config in
  eprintln ("Saved pkgs.ml API token in " ^ Path.to_string path);
  Ok ()
