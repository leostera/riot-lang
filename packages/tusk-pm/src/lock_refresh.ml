open Std

let metadata_modified = fun path ->
  match Fs.metadata path with
  | Ok metadata -> Ok (Fs.Metadata.modified metadata)
  | Error err -> Error ("failed to stat '" ^ Path.to_string path ^ "': " ^ IO.error_message err)

let rec any_manifest_newer_than = fun ~lock_mtime manifests ->
  match manifests with
  | [] -> Ok false
  | manifest_path :: rest -> (
      match metadata_modified manifest_path with
      | Error _ as err -> err
      | Ok manifest_mtime ->
          if manifest_mtime > lock_mtime then
            Ok true
          else
            any_manifest_newer_than ~lock_mtime rest
    )

let needs_refresh = fun ~workspace_root ~manifest_paths ->
  let lock_path = Tusk_model.Tusk_dirs.package_lock_path ~workspace_root in
  match Fs.exists lock_path with
  | Error err ->
      Error ("failed to check lockfile '" ^ Path.to_string lock_path ^ "': " ^ IO.error_message err)
  | Ok false ->
      Ok true
  | Ok true -> (
      match metadata_modified lock_path with
      | Error _ as err -> err
      | Ok lock_mtime -> any_manifest_newer_than ~lock_mtime manifest_paths
    )
