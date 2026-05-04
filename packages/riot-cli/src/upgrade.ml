open Std

let ( let* ) value fn = Result.and_then value ~fn

type archive_source =
  | Local of Path.t
  | Remote of string

type metadata_source =
  | Metadata_local of Path.t
  | Metadata_remote of string

let out = eprintln

let default_archive_base_url = "https://cdn.pkgs.ml"

let default_metadata_base_url = "https://cdn.pkgs.ml"

let default_issues_url = "https://github.com/leostera/riot/issues"

let command =
  let open ArgParser in
  let open ArgParser.Arg in
  command "upgrade"
  |> about "Upgrade the globally installed riot binary"
  |> args
    [
      option "version"
      |> long "version"
      |> help "Version to install (default: latest)";
    ]

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid UTF-8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } ->
      "system call '" ^ syscall ^ "' returned invalid UTF-8 path: " ^ path
  | Path.SystemError error -> error

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

let version_label = fun version -> Option.unwrap_or ~default:"latest" version

let archive_url = fun ?version ~target ~base_url () ->
  let version = version_label version in
  base_url ^ "/riot/riot-" ^ version ^ "-" ^ target ^ ".tar.gz"

let metadata_url = fun ?version ~base_url () ->
  match version with
  | Some version -> base_url ^ "/riot/riot-" ^ version ^ ".json"
  | None -> base_url ^ "/riot/latest.json"

let installed_binary_path = fun () ->
  let* riot_home = Riot_model.Riot_dirs.user_riot_dir () in
  Ok Path.(riot_home / Path.v "bin" / Path.v "riot")

let install_temp_path = fun dst ->
  let dir = Path.dirname dst in
  let name = Path.basename dst in
  let pid = Process.id () in
  let nonce =
    Random.bits ()
    |> Result.expect ~msg:"failed to generate upgrade nonce"
  in
  Path.(dir / Path.v ("." ^ name ^ ".upgrade-" ^ Int32.to_string pid ^ "-" ^ Int.to_string nonce))

let cleanup_temp_file = fun path ->
  match Fs.remove_file path with
  | Ok () -> ()
  | Error _ -> ()

let install_binary_atomically = fun ~src ~dst ->
  let temp_path = install_temp_path dst in
  match Fs.copy ~src ~dst:temp_path with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.set_permissions temp_path Fs.Permissions.executable with
      | Error err ->
          cleanup_temp_file temp_path;
          Error (IO.error_message err)
      | Ok () -> (
          match Fs.rename ~src:temp_path ~dst with
          | Ok () -> Ok ()
          | Error err ->
              cleanup_temp_file temp_path;
              Error (IO.error_message err)
        )
    )

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let run_downloader = fun ~program ~args ~url ->
  let cmd = Command.make program ~args in
  match Command.output cmd with
  | Ok { status = 0; _ } -> Ok ()
  | Ok { status; stderr; _ } ->
      let detail =
        if String.equal (String.trim stderr) "" then
          program ^ " failed with status " ^ Int.to_string status ^ ": " ^ Command.to_string cmd
        else
          program ^ " failed with status " ^ Int.to_string status ^ ": " ^ String.trim stderr
      in
      Error detail
  | Error (Command.SystemError msg) ->
      Error ("failed to start " ^ program ^ " while downloading " ^ url ^ ": " ^ msg)

let download_text = fun ~url ->
  let run program args =
    let cmd = Command.make program ~args in
    match Command.output cmd with
    | Ok { status = 0; stdout; _ } -> Ok stdout
    | Ok { status; stderr; _ } ->
        let detail =
          if String.equal (String.trim stderr) "" then
            program ^ " failed with status " ^ Int.to_string status ^ ": " ^ Command.to_string cmd
          else
            program ^ " failed with status " ^ Int.to_string status ^ ": " ^ String.trim stderr
        in
        Error detail
    | Error (Command.SystemError msg) ->
        Error ("failed to start " ^ program ^ " while downloading " ^ url ^ ": " ^ msg)
  in
  match run "curl" [ "-fsSL"; url ] with
  | Ok content -> Ok content
  | Error curl_error when String.contains curl_error "failed to start curl" -> (
      match run "wget" [ "-q"; "-O-"; url ] with
      | Ok content -> Ok content
      | Error wget_error when String.contains wget_error "failed to start wget" ->
          Error "riot upgrade requires curl or wget to download releases"
      | Error wget_error -> Error wget_error
    )
  | Error error -> Error error

let download_archive = fun ~url ~dst ->
  let dst_str = Path.to_string dst in
  match run_downloader ~program:"curl" ~args:[ "-fsSL"; "-o"; dst_str; url; ] ~url with
  | Ok () -> Ok ()
  | Error curl_error when String.contains curl_error "failed to start curl" -> (
      match run_downloader ~program:"wget" ~args:[ "-q"; "-O"; dst_str; url; ] ~url with
      | Ok () -> Ok ()
      | Error wget_error when String.contains wget_error "failed to start wget" ->
          Error "riot upgrade requires curl or wget to download releases"
      | Error wget_error -> Error wget_error
    )
  | Error error -> Error error

let tar_error_message = fun __tmp1 ->
  match __tmp1 with
  | Archive.Tar.Engine_error err -> Archive.Tar.error_to_string (Archive.Tar.Engine_error err)
  | Archive.Tar.Invalid_path path -> "invalid archive path: " ^ path
  | Archive.Tar.Unsafe_path path -> "unsafe archive path: " ^ path
  | Archive.Tar.Unsupported_entry_kind _ -> "unsupported archive entry kind"
  | Archive.Tar.Duplicate_entry path -> "duplicate archive entry: " ^ Path.to_string path

let extract_archive = fun ~archive_path ~into ->
  match Fs.File.open_read archive_path with
  | Error err -> Error (Fs.File.error_to_string err)
  | Ok file ->
      protect
        ~finally:(fun () ->
          match Fs.File.close file with
          | Ok () -> ()
          | Error err -> out ("warning: failed to close archive: " ^ Fs.File.error_to_string err))
        (fun () ->
          let reader = Compress.Gzip.to_reader (Fs.File.to_reader file) in
          match Archive.Tar.extract reader ~into with
          | Ok () -> Ok ()
          | Error (Archive.Tar.Extract_source_error err) ->
              Error ("failed to read archive: " ^ IO.error_message err)
          | Error (Archive.Tar.Extract_fs_error err) ->
              Error ("failed to unpack archive: " ^ IO.error_message err)
          | Error (Archive.Tar.Extract_error err) ->
              Error ("failed to decode archive: " ^ tar_error_message err))

let read_file = fun path ->
  match Fs.read path with
  | Ok data -> Ok data
  | Error err -> Error (IO.error_message err)

let same_file_contents = fun ~left ~right ->
  match (read_file left, read_file right) with
  | (Ok left_data, Ok right_data) -> Ok (String.equal left_data right_data)
  | (Error message, _)
  | (_, Error message) -> Error message

let binary_version = fun path ->
  let cmd = Command.make (Path.to_string path) ~args:[ "--version" ] in
  match Command.output cmd with
  | Ok { status = 0; stdout; _ } -> Ok (String.trim stdout)
  | Ok { status; stderr; _ } ->
      let detail =
        if String.equal (String.trim stderr) "" then
          "riot --version failed with status " ^ Int.to_string status
        else
          String.trim stderr
      in
      Error detail
  | Error (Command.SystemError msg) -> Error ("failed to execute riot binary: " ^ msg)

let resolve_metadata_source = fun ?version () ->
  match Env.get Env.String ~var:"RIOT_UPGRADE_METADATA_PATH" with
  | Some raw_path -> (
      match Path.from_string raw_path with
      | Ok path -> Ok (Metadata_local path)
      | Error err -> Error ("invalid RIOT_UPGRADE_METADATA_PATH: " ^ path_error_message err)
    )
  | None ->
      let base_url =
        Env.get Env.String ~var:"RIOT_UPGRADE_METADATA_BASE_URL"
        |> Option.unwrap_or ~default:default_metadata_base_url
      in
      Ok (Metadata_remote (metadata_url ?version ~base_url ()))

let load_metadata = fun source ->
  let* content =
    match source with
    | Metadata_local path ->
        Fs.read path
        |> Result.map_err ~fn:IO.error_message
    | Metadata_remote url -> download_text ~url
  in
  Version_info.from_json_string content

let display_label = fun metadata -> Version_info.release_label metadata

let read_extracted_metadata = fun ~extract_dir ->
  let path = Path.(extract_dir / Path.v "release.json") in
  match Fs.exists path with
  | Ok true ->
      Version_info.from_path path
      |> Result.map ~fn:Option.some
  | Ok false
  | Error _ -> Ok None

let metadata_from_binary_version = fun path ->
  let* version = binary_version path in
  match Version_info.from_version_string version with
  | Some metadata -> Ok metadata
  | None ->
      Ok {
        Version_info.release_id = version;
        build_sha = "unknown";
        notes_url = None;
        compare_url = None;
        issues_url = Some default_issues_url;
      }

let resolved_metadata = fun ~latest_metadata ~extract_dir ~downloaded_binary ->
  match latest_metadata with
  | Some metadata -> Ok metadata
  | None ->
      let* extracted = read_extracted_metadata ~extract_dir in
      match extracted with
      | Some metadata -> Ok metadata
      | None -> metadata_from_binary_version downloaded_binary

let resolve_archive_source = fun ?version ~target () ->
  match Env.get Env.String ~var:"RIOT_UPGRADE_ARCHIVE_PATH" with
  | Some raw_path -> (
      match Path.from_string raw_path with
      | Ok path -> Ok (Local path)
      | Error err -> Error ("invalid RIOT_UPGRADE_ARCHIVE_PATH: " ^ path_error_message err)
    )
  | None ->
      let base_url =
        Env.get Env.String ~var:"RIOT_UPGRADE_ARCHIVE_BASE_URL"
        |> Option.unwrap_or ~default:default_archive_base_url
      in
      Ok (Remote (archive_url ?version ~target ~base_url ()))

let ensure_install_dir = fun () ->
  let* riot_home = Riot_model.Riot_dirs.user_riot_dir () in
  let* () =
    Fs.create_dir_all riot_home
    |> Result.map_err ~fn:IO.error_message
  in
  Fs.create_dir_all Path.(riot_home / Path.v "bin")
  |> Result.map_err ~fn:IO.error_message

let copy_local_archive = fun ~src ~dst ->
  match Fs.copy ~src ~dst with
  | Ok () -> Ok ()
  | Error err -> Error (IO.error_message err)

let write_newer_release_message = fun ~current ~next ->
  out
    ("Riot " ^ display_label next ^ " is out! You're on " ^ display_label current)

let write_unchanged_message = fun metadata ->
  out
    ("Congrats! You're already on the latest version of Riot (which is "
    ^ display_label metadata
    ^ ")")

let write_upgraded_message = fun ~duration_ms ~metadata ->
  let duration =
    Time.Duration.from_millis duration_ms
    |> Time.Duration.to_secs_string ~precision:2
  in
  out ("[" ^ duration ^ "s] Upgraded.");
  out "";
  out ("Welcome to Riot " ^ display_label metadata ^ "!");
  (
    match metadata.Version_info.notes_url with
    | Some url ->
        out "";
        out ("What's new in Riot " ^ display_label metadata ^ ":");
        out "";
        out ("    " ^ url)
    | None -> ()
  );
  (
    match metadata.issues_url with
    | Some url ->
        out "";
        out "Report any bugs:";
        out "";
        out ("    " ^ url)
    | None -> ()
  );
  (
    match metadata.compare_url with
    | Some url ->
        out "";
        out "Commit log:";
        out "";
        out ("    " ^ url)
    | None -> ()
  )

let run = fun matches ->
  let started_at = Time.Instant.now () in
  let version = ArgParser.get_one matches "version" in
  let target = Riot_model.Target.to_string (Riot_model.Riot_dirs.host_target ()) in
  let current_metadata = Version_info.read_installed () in
  match ensure_install_dir () with
  | Error message ->
      out ("\027[1;31mError\027[0m: " ^ message);
      Error (Failure message)
  | Ok () -> (
      let requested_metadata =
        if Env.get Env.String ~var:"RIOT_UPGRADE_ARCHIVE_PATH"
        |> Option.is_some then
          Ok None
        else
          match resolve_metadata_source ?version () with
          | Error message -> Error message
          | Ok (Metadata_local path) ->
              Version_info.from_path path
              |> Result.map ~fn:Option.some
          | Ok (Metadata_remote url) -> (
              match download_text ~url with
              | Ok content ->
                  Version_info.from_json_string content
                  |> Result.map ~fn:Option.some
              | Error _ -> Ok None
            )
      in
      let* latest_metadata =
        requested_metadata
        |> Result.map
          ~fn:(Option.map
            ~fn:(fun (metadata: Version_info.t) -> {
              metadata with
              issues_url = Option.or_else
                metadata.issues_url
                ~fn:(fun () -> Some default_issues_url);
            }))
        |> Result.map_err ~fn:(fun message -> Failure message)
      in
      match resolve_archive_source
        ?version:(
          match latest_metadata with
          | Some metadata -> Some metadata.release_id
          | None -> version
        )
        ~target
        () with
      | Error message ->
          out ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
      | Ok archive_source ->
          let* current_binary =
            installed_binary_path ()
            |> Result.map_err ~fn:(fun message -> Failure message)
          in
          let current_exists =
            Fs.exists current_binary
            |> Result.unwrap_or ~default:false
          in
          (
            match (current_metadata, latest_metadata) with
            | (Some current, Some latest) when current_exists
            && Version_info.same_identity current latest ->
                write_unchanged_message latest;
                Ok ()
            | _ ->
                match with_tempdir_result
                  "riot-upgrade"
                  (fun tempdir ->
                    let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
                    let extract_dir = Path.(tempdir / Path.v "extract") in
                    let downloaded_binary = Path.(extract_dir / Path.v "riot") in
                    let* () =
                      match archive_source with
                      | Local path -> copy_local_archive ~src:path ~dst:archive_path
                      | Remote url -> download_archive ~url ~dst:archive_path
                    in
                    let* () =
                      Fs.create_dir_all extract_dir
                      |> Result.map_err ~fn:IO.error_message
                    in
                    let* () = extract_archive ~archive_path ~into:extract_dir in
                    let* metadata =
                      resolved_metadata ~latest_metadata ~extract_dir ~downloaded_binary
                    in
                    let unchanged =
                      if current_exists then
                        same_file_contents ~left:current_binary ~right:downloaded_binary
                      else
                        Ok false
                    in
                    let* unchanged = unchanged in
                    if unchanged then (
                      let* () =
                        match current_metadata with
                        | Some current when Version_info.same_identity current metadata -> Ok ()
                        | _ -> Version_info.write_installed metadata
                      in
                      write_unchanged_message metadata;
                      Ok ()
                    ) else (
                      let () =
                        match current_metadata with
                        | Some current when not (Version_info.same_identity current metadata) ->
                            write_newer_release_message ~current ~next:metadata
                        | Some _
                        | None -> ()
                      in
                      let install_dir = Path.dirname current_binary in
                      let* () =
                        Fs.create_dir_all install_dir
                        |> Result.map_err ~fn:IO.error_message
                      in
                      let* () = install_binary_atomically ~src:downloaded_binary ~dst:current_binary in
                      let* () = Version_info.write_installed metadata in
                      let duration =
                        Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ())
                        |> Time.Duration.to_millis
                      in
                      write_upgraded_message ~duration_ms:duration ~metadata;
                      Ok ()
                    )) with
                | Ok () -> Ok ()
                | Error message ->
                    out ("\027[1;31mError\027[0m: " ^ message);
                    Error (Failure message)
          )
    )
