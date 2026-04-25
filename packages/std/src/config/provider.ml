open Global

type env = Loader.env

type t =
  | Empty
  | Env of { env: env }
  | Path of { path: Path.t }
  | Static of { toml_string: string }

let empty = Empty

let env = fun ?env () ->
  let e =
    match env with
    | Some e -> e
    | None -> Loader.detect_env ()
  in
  Env { env = e }

let file = fun path -> Path { path }

let static = fun toml_string -> Static { toml_string }

let load = function
  | Empty -> Error "Cannot load from empty provider"
  | Env { env } -> Loader.load_for_env env
  | Path { path } -> Loader.load_file (Path.to_string path)
  | Static { toml_string } ->
      match Data.Toml.parse toml_string with
      | Error err -> Error ("TOML parse error: " ^ Data.Toml.error_to_string err)
      | Ok toml -> Ok toml
