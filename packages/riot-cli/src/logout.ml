open Std
open Riot_model

let ( let* ) value fn = Result.and_then value ~fn

let registry_name = "pkgs.ml"

let command = let open ArgParser in
command "logout" |> about "Remove your saved pkgs.ml API token"

let fail = fun message -> Error (Failure message)

let config_path = fun () -> Riot_dirs.config_path ()

let ensure_riot_dirs = fun () -> Riot_dirs.ensure_created () |> Result.map_err ~fn:(
  fun exn -> Failure (Exception.to_string exn)
)

let load_config = fun path ->
  match Fs.exists path with
  | Error io_error -> fail ("failed to read config '" ^ Path.to_string path ^ "': " ^ IO.error_message io_error)
  | Ok false -> Ok User_config.default
  | Ok true -> User_config.load path |> Result.map_err ~fn:(
    fun err -> Failure (User_config.message err)
  )

let save_config = fun path config -> User_config.save config path |> Result.map_err ~fn:(
  fun err -> Failure (User_config.message err)
)

let run = fun _matches ->
  let* () = ensure_riot_dirs ()
  in
  let path = config_path () in
  let* config = load_config path
  in
  let config = User_config.clear_api_token config ~registry_name in
  let* () = save_config path config
  in
  eprintln ("Removed pkgs.ml API token from " ^ Path.to_string path);
  Ok ()
