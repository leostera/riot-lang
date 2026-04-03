open Std

let ( let* ) = Result.and_then

type archive_source =
  | Local of Path.t
  | Remote of string

let out = eprintln

let default_cdn_base_url = "https://cdn.pkgs.ml"

let command =
  let open ArgParser in
  let open Arg in
  command "upgrade"
  |> about "Upgrade the globally installed riot binary"
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

let kernel_tar_error_message = function
  | Kernel.Archive.Tar.Invalid_header message -> "invalid tar header: " ^ message
  | Kernel.Archive.Tar.Entry_in_progress -> "tar entry is still in progress"
  | Kernel.Archive.Tar.Invalid_state message -> "invalid tar reader state: " ^ message
  | Kernel.Archive.Tar.Unexpected_eof -> "unexpected end of tar archive"
  | Kernel.Archive.Tar.Out_of_memory -> "tar reader ran out of memory"
  | Kernel.Archive.Tar.Unknown_error message -> message

let kernel_gzip_error_message = function
  | Kernel.Compress.Gzip.Invalid_data -> "invalid gzip data"
  | Kernel.Compress.Gzip.Need_dictionary -> "gzip stream requires a dictionary"
  | Kernel.Compress.Gzip.Buffer_error -> "gzip decoder hit a buffer error"
  | Kernel.Compress.Gzip.Out_of_memory -> "gzip decoder ran out of memory"
  | Kernel.Compress.Gzip.Unknown_error message -> message

let gzip_error_message = function
  | Compress.Gzip.Kernel_error err -> kernel_gzip_error_message err
  | Compress.Gzip.Truncated_input -> "truncated gzip input"

let gzip_read_error_message = function
  | Compress.Gzip.Source_error err -> IO.error_message err
  | Compress.Gzip.Gzip_error err -> gzip_error_message err

let version_label = fun version -> Option.unwrap_or ~default:"latest" version

let pretty_version = fun version ->
  if String.starts_with ~prefix:"riot " version then
    String.sub version 5 (String.length version - 5)
  else
    version

let archive_url = fun ?version ~target ~base_url () ->
  let version = version_label version in
  base_url ^ "/riot/riot-" ^ version ^ "-" ^ target ^ ".tar.gz"

let riot_home_dir = fun () ->
  match Env.home_dir () with
  | Some home -> Ok Path.(home / Path.v ".riot")
  | None -> Error "failed to determine home directory"

let installed_binary_path = fun () ->
  let* riot_home = riot_home_dir () in
  Ok Path.(riot_home / Path.v "bin" / Path.v "riot")

let install_temp_path = fun dst ->
  let dir = Path.dirname dst in
  let name = Path.basename dst in
  let pid = System.OsProcess.current_pid () in
  let nonce = Random.bits () in
  Path.(dir / Path.v ("." ^ name ^ ".upgrade-" ^ Int.to_string pid ^ "-" ^ Int.to_string nonce))

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

let download_archive = fun ~url ~dst ->
  let dst_str = Path.to_string dst in
  match run_downloader ~program:"curl" ~args:[ "-fsSL"; "-o"; dst_str; url ] ~url with
  | Ok () -> Ok ()
  | Error curl_error when String.contains curl_error "failed to start curl" -> (
      match run_downloader ~program:"wget" ~args:[ "-q"; "-O"; dst_str; url ] ~url with
      | Ok () -> Ok ()
      | Error wget_error when String.contains wget_error "failed to start wget" ->
          Error "riot upgrade requires curl or wget to download releases"
      | Error wget_error -> Error wget_error
    )
  | Error error -> Error error

let tar_error_message = function
  | Archive.Tar.Kernel_error err -> kernel_tar_error_message err
  | Archive.Tar.Invalid_path path -> "invalid archive path: " ^ path
  | Archive.Tar.Unsafe_path path -> "unsafe archive path: " ^ path
  | Archive.Tar.Unsupported_entry_kind _ -> "unsupported archive entry kind"
  | Archive.Tar.Duplicate_entry path -> "duplicate archive entry: " ^ Path.to_string path

let extract_archive = fun ~archive_path ~into ->
  match Fs.File.open_read archive_path with
  | Error err -> Error (IO.error_message err)
  | Ok file ->
      Kernel.Fun.protect
        ~finally:(fun () -> ignore (Fs.File.close file))
        (fun () ->
          let reader = Compress.Gzip.to_reader (Fs.File.to_reader file) in
          match Archive.Tar.extract reader ~into with
          | Ok () -> Ok ()
          | Error (Archive.Tar.Extract_source_error err) ->
              Error ("failed to read archive: " ^ gzip_read_error_message err)
          | Error (Archive.Tar.Extract_fs_error err) ->
              Error ("failed to unpack archive: " ^ IO.error_message err)
          | Error (Archive.Tar.Extract_error err) ->
              Error ("failed to decode archive: " ^ tar_error_message err))

let read_file = fun path ->
  match Fs.read path with
  | Ok data -> Ok data
  | Error err -> Error (IO.error_message err)

let same_file_contents = fun ~left ~right ->
  match read_file left, read_file right with
  | Ok left_data, Ok right_data -> Ok (String.equal left_data right_data)
  | Error message, _
  | _, Error message -> Error message

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
  | Error (Command.SystemError msg) ->
      Error ("failed to execute riot binary: " ^ msg)

let resolve_archive_source = fun ?version ~target () ->
  match Env.var Env.String ~name:"RIOT_UPGRADE_ARCHIVE_PATH" with
  | Some raw_path -> (
      match Path.of_string raw_path with
      | Ok path -> Ok (Local path)
      | Error err -> Error ("invalid RIOT_UPGRADE_ARCHIVE_PATH: " ^ path_error_message err)
    )
  | None ->
      let base_url =
        Env.var Env.String ~name:"RIOT_UPGRADE_BASE_URL"
        |> Option.unwrap_or ~default:default_cdn_base_url
      in
      Ok (Remote (archive_url ?version ~target ~base_url ()))

let ensure_install_dir = fun () ->
  let* riot_home = riot_home_dir () in
  let* () = Fs.create_dir_all riot_home |> Result.map_error IO.error_message in
  Fs.create_dir_all Path.(riot_home / Path.v "bin") |> Result.map_error IO.error_message

let copy_local_archive = fun ~src ~dst ->
  match Fs.copy ~src ~dst with
  | Ok () -> Ok ()
  | Error err -> Error (IO.error_message err)

let write_header = fun ?current_version ~next_version () ->
  let next_version = pretty_version next_version in
  match current_version with
  | Some current when not (String.equal (pretty_version current) next_version) ->
      let current = pretty_version current in
      out ("Riot " ^ next_version ^ " is out! You're on " ^ current)
  | Some current ->
      out ("Checking Riot " ^ pretty_version current ^ " for updates...")
  | None ->
      out ("Installing Riot " ^ next_version ^ "...")

let write_unchanged_message = fun version ->
  out ("Riot is already up to date (" ^ pretty_version version ^ ").")

let write_upgraded_message = fun ~duration_ms ~version ->
  let duration = Time.Duration.from_millis duration_ms |> Time.Duration.to_secs_string ~precision:2 in
  out ("[" ^ duration ^ "s] Upgraded to " ^ pretty_version version ^ ".")

let run = fun matches ->
  let started_at = Time.Instant.now () in
  let version = ArgParser.get_one matches "version" in
  let target = Riot_model.Riot_dirs.host_target () in
  match ensure_install_dir () with
  | Error message ->
      out ("\027[1;31mError\027[0m: " ^ message);
      Error (Failure message)
  | Ok () -> (
      match resolve_archive_source ?version ~target () with
      | Error message ->
          out ("\027[1;31mError\027[0m: " ^ message);
          Error (Failure message)
      | Ok archive_source ->
          let* current_binary = installed_binary_path () |> Result.map_error (fun message -> Failure message) in
          let current_exists = Fs.exists current_binary |> Result.unwrap_or ~default:false in
          let current_version =
            if current_exists then
              match binary_version current_binary with
              | Ok version -> Some version
              | Error _ -> None
            else
              None
          in
          match with_tempdir_result "riot-upgrade"
            (fun tempdir ->
              let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
              let extract_dir = Path.(tempdir / Path.v "extract") in
              let downloaded_binary = Path.(extract_dir / Path.v "riot") in
              let* () =
                match archive_source with
                | Local path -> copy_local_archive ~src:path ~dst:archive_path
                | Remote url -> download_archive ~url ~dst:archive_path
              in
              let* () = Fs.create_dir_all extract_dir |> Result.map_error IO.error_message in
              let* () = extract_archive ~archive_path ~into:extract_dir in
              let* next_version = binary_version downloaded_binary in
              let () = write_header ?current_version ~next_version () in
              let unchanged =
                if current_exists then
                  same_file_contents ~left:current_binary ~right:downloaded_binary
                else
                  Ok false
              in
              let* unchanged = unchanged in
              if unchanged then (
                write_unchanged_message next_version;
                Ok ()
              ) else (
                let install_dir = Path.dirname current_binary in
                let* () = Fs.create_dir_all install_dir |> Result.map_error IO.error_message in
                let* () = install_binary_atomically ~src:downloaded_binary ~dst:current_binary in
                let duration =
                  Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ())
                  |> Time.Duration.to_millis
                in
                write_upgraded_message ~duration_ms:duration ~version:next_version;
                Ok ()
              )) with
          | Ok () -> Ok ()
          | Error message ->
              out ("\027[1;31mError\027[0m: " ^ message);
              Error (Failure message)
    )
