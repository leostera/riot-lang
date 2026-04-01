open Std

let read = fun ~workspace_root ->
  let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
  match Fs.exists lock_path with
  | Error err ->
      Error
        ("failed to check lockfile '"
        ^ Path.to_string lock_path
        ^ "': "
        ^ IO.error_message err)
  | Ok false -> Ok None
  | Ok true -> (
      match Fs.read lock_path with
      | Error err ->
          Error
            ("failed to read lockfile '"
            ^ Path.to_string lock_path
            ^ "': "
            ^ IO.error_message err)
      | Ok source -> (
          match Std.Data.Toml.parse source with
          | Error err ->
              Error
                ("failed to parse lockfile TOML '"
                ^ Path.to_string lock_path
                ^ "': "
                ^ Std.Data.Toml.error_to_string err)
          | Ok toml -> (
              match Tusk_model.Lockfile.of_toml toml with
              | Ok lockfile -> Ok (Some lockfile)
              | Error err ->
                  Error
                    ("failed to decode lockfile '"
                    ^ Path.to_string lock_path
                    ^ "': "
                    ^ err)
            )
        )
    )

let write = fun ~workspace_root lockfile ->
  let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
  let source = Tusk_model.Lockfile.to_string lockfile in
  match Fs.write source lock_path with
  | Ok () -> Ok ()
  | Error err ->
      Error
        ("failed to write lockfile '"
        ^ Path.to_string lock_path
        ^ "': "
        ^ IO.error_message err)
