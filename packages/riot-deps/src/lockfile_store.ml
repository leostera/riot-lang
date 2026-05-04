open Std

type error =
  | StatFailed of {
      path: Path.t;
      error: IO.error;
    }
  | ReadFailed of {
      path: Path.t;
      error: IO.error;
    }
  | TomlParseFailed of {
      path: Path.t;
      error: Std.Data.Toml.error;
    }
  | DecodeFailed of {
      path: Path.t;
      error: Riot_model.Lockfile.error;
    }
  | WriteFailed of {
      path: Path.t;
      error: IO.error;
    }

let error_message = fun __tmp1 ->
  match __tmp1 with
  | StatFailed { path; error } ->
      "failed to check lockfile '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | ReadFailed { path; error } ->
      "failed to read lockfile '" ^ Path.to_string path ^ "': " ^ IO.error_message error
  | TomlParseFailed { path; error } ->
      "failed to parse lockfile TOML '"
      ^ Path.to_string path
      ^ "': "
      ^ Std.Data.Toml.error_to_string error
  | DecodeFailed { path; error } ->
      "failed to decode lockfile '"
      ^ Path.to_string path
      ^ "': "
      ^ Riot_model.Lockfile.error_message error
  | WriteFailed { path; error } ->
      "failed to write lockfile '" ^ Path.to_string path ^ "': " ^ IO.error_message error

let read = fun ~workspace_root ->
  let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
  match Fs.exists lock_path with
  | Error err -> Error (StatFailed { path = lock_path; error = err })
  | Ok false -> Ok None
  | Ok true -> (
      match Fs.read lock_path with
      | Error err -> Error (ReadFailed { path = lock_path; error = err })
      | Ok source -> (
          match Std.Data.Toml.parse source with
          | Error err -> Error (TomlParseFailed { path = lock_path; error = err })
          | Ok toml -> (
              match Riot_model.Lockfile.from_toml toml with
              | Ok lockfile -> Ok (Some lockfile)
              | Error err -> Error (DecodeFailed { path = lock_path; error = err })
            )
        )
    )

let write = fun ~workspace_root lockfile ->
  let lock_path = Riot_model.Riot_dirs.package_lock_path ~workspace_root in
  let source = Riot_model.Lockfile.to_string lockfile in
  match Fs.write source lock_path with
  | Ok () -> Ok ()
  | Error err -> Error (WriteFailed { path = lock_path; error = err })
