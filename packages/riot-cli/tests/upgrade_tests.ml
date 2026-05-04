open Std

module Test = Std.Test

let ( let* ) value fn = Result.and_then value ~fn

let tar_block_size = 512

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

let write_file = fun path content ->
  match Fs.create_dir_all (Path.dirname path) with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.write content path with
      | Ok () -> Ok ()
      | Error err -> Error (IO.error_message err)
    )

let make_metadata = fun ~release_id ~build_sha ?notes_url ?compare_url ?issues_url () ->
  Riot_cli.Version_info.{
    release_id;
    build_sha;
    notes_url;
    compare_url;
    issues_url;
  }

let metadata_json_string = fun (metadata: Riot_cli.Version_info.t) ->
  Data.Json.(Object [
    ("release_id", String metadata.release_id);
    ("build_sha", String metadata.build_sha);
    ("notes_url", match metadata.notes_url with
    | Some value -> String value
    | None -> Null);
    ("compare_url", match metadata.compare_url with
    | Some value -> String value
    | None -> Null);
    ("issues_url", match metadata.issues_url with
    | Some value -> String value
    | None -> Null);
  ]
  |> Data.Json.to_string_pretty)

let copy_executable_file = fun ~src ~dst ->
  let* () = Result.map_err (Fs.create_dir_all (Path.dirname dst)) ~fn:IO.error_message in
  let* () = Result.map_err (Fs.copy ~src ~dst) ~fn:IO.error_message in
  Result.map_err (Fs.set_permissions dst Fs.Permissions.executable) ~fn:IO.error_message

let bytes_set_string = fun dst ~offset ~width value ->
  let copy_len = min width (String.length value) in
  IO.Bytes.blit_string value ~src_offset:0 ~dst ~dst_offset:offset ~len:copy_len

let octal_string = fun value ->
  let rec loop acc remaining =
    if Int64.equal remaining 0L then
      acc
    else
      let digit = Int64.to_int (Int64.rem remaining 8L) in
      let ch = Char.from_int_unchecked (Char.code '0' + digit) in
      loop (String.make ~len:1 ~char:ch ^ acc) (Int64.div remaining 8L)
  in
  if Int64.equal value 0L then
    "0"
  else
    loop "" value

let zero_pad_left = fun width value ->
  if String.length value >= width then
    String.sub value ~offset:(String.length value - width) ~len:width
  else
    String.make ~len:(width - String.length value) ~char:'0' ^ value

let bytes_set_octal = fun dst ~offset ~width value ->
  let digits_width = max 1 (width - 1) in
  let trimmed = zero_pad_left digits_width (octal_string value) in
  bytes_set_string dst ~offset ~width:(width - 1) trimmed;
  IO.Bytes.set_unchecked dst ~at:(offset + width - 1) ~char:'\000'

let compute_checksum = fun header ->
  let sum = ref 0 in
  for index = 0 to tar_block_size - 1 do
    sum := !sum + Char.code (IO.Bytes.get_unchecked header ~at:index)
  done;
  !sum

let make_header = fun ~name ~mode ~size ->
  let header = IO.Bytes.create ~size:tar_block_size in
  IO.Bytes.fill header ~offset:0 ~len:tar_block_size ~char:'\000';
  bytes_set_string header ~offset:0 ~width:100 name;
  bytes_set_octal header ~offset:100 ~width:8 mode;
  bytes_set_octal header ~offset:108 ~width:8 0L;
  bytes_set_octal header ~offset:116 ~width:8 0L;
  bytes_set_octal header ~offset:124 ~width:12 size;
  bytes_set_octal header ~offset:136 ~width:12 0L;
  bytes_set_string header ~offset:148 ~width:8 "        ";
  IO.Bytes.set_unchecked header ~at:156 ~char:'0';
  bytes_set_string header ~offset:257 ~width:6 "ustar";
  bytes_set_string header ~offset:263 ~width:2 "00";
  let checksum = compute_checksum header in
  let checksum_field = zero_pad_left 6 (octal_string (Int64.from_int checksum)) ^ "\000 " in
  bytes_set_string header ~offset:148 ~width:8 checksum_field;
  header

let pad_data = fun data ->
  let len = String.length data in
  let remainder = Int.rem len tar_block_size in
  if remainder = 0 then
    ""
  else
    String.make ~len:(tar_block_size - remainder) ~char:'\000'

let build_archive = fun ~metadata ~binary ->
  let size = Int64.from_int (String.length binary) in
  let buffer = IO.Buffer.create ~size:2_048 in
  IO.Buffer.add_bytes buffer (make_header ~name:"riot" ~mode:0o755L ~size);
  IO.Buffer.add_string buffer binary;
  IO.Buffer.add_string buffer (pad_data binary);
  (
    match metadata with
    | Some metadata ->
        let content = metadata_json_string metadata ^ "\n" in
        let size = Int64.from_int (String.length content) in
        IO.Buffer.add_bytes buffer (make_header ~name:"release.json" ~mode:0o644L ~size);
        IO.Buffer.add_string buffer content;
        IO.Buffer.add_string buffer (pad_data content)
    | None -> ()
  );
  IO.Buffer.add_string buffer (String.make ~len:(tar_block_size * 2) ~char:'\000');
  IO.Buffer.contents buffer

let write_upgrade_archive = fun ~metadata ~path ~binary ->
  let tar_archive = build_archive ~metadata ~binary in
  match Compress.Gzip.compress_string tar_archive with
  | Error _ -> Error "failed to gzip upgrade archive"
  | Ok gzip_payload -> write_file path gzip_payload

let with_env = fun ~name ~value fn ->
  let previous = Env.get Env.String ~var:name in
  protect
    ~finally:(fun () ->
      match previous with
      | Some old ->
          let _ = Env.set ~var:name ~value:old in
          ()
      | None ->
          let _ = Env.remove ~var:name in
          ())
    (fun () ->
      let _ = Env.set ~var:name ~value in
      fn ())

let parse_upgrade = fun args ->
  match ArgParser.get_matches Riot_cli.Upgrade.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let test_upgrade_accepts_version_flag = fun _ctx ->
  match parse_upgrade [ "upgrade"; "--version"; "abc123" ] with
  | Error err -> Error ("expected upgrade args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "abc123") ~actual:(ArgParser.get_one matches "version");
      Ok ()

let test_upgrade_installs_downloaded_archive = fun ctx ->
  with_tempdir_result
    "upgrade-install"
    (fun tempdir ->
      let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
      let home_dir = Path.(tempdir / Path.v "home") in
      let installed = Path.(home_dir / Path.v ".riot" / Path.v "bin" / Path.v "riot") in
      let old_binary_path = Path.v "/bin/sh" in
      let metadata =
        make_metadata
          ~release_id:"v9.9.9"
          ~build_sha:"deadbeefcafe"
          ~issues_url:"https://github.com/leostera/riot/issues"
          ()
      in
      let* riot_binary_path = Test.Context.require_binary ctx "riot" in
      let* next_binary = Result.map_err (Fs.read riot_binary_path) ~fn:IO.error_message in
      let* () = copy_executable_file ~src:old_binary_path ~dst:installed in
      let* () =
        write_upgrade_archive ~metadata:(Some metadata) ~path:archive_path ~binary:next_binary
      in
      let* matches = parse_upgrade [ "upgrade" ] in
      with_env
        ~name:"HOME"
        ~value:(Path.to_string home_dir)
        (fun () ->
          with_env
            ~name:"RIOT_UPGRADE_ARCHIVE_PATH"
            ~value:(Path.to_string archive_path)
            (fun () ->
              match Riot_cli.Upgrade.run matches with
              | Error exn -> Error (Exception.to_string exn)
              | Ok () -> (
                  match Fs.read installed with
                  | Error err -> Error (IO.error_message err)
                  | Ok actual ->
                      Test.assert_equal ~expected:next_binary ~actual;
                      match Riot_cli.Version_info.read_installed () with
                      | Some actual_metadata when actual_metadata = metadata -> Ok ()
                      | Some actual_metadata ->
                          Error ("unexpected installed metadata: expected "
                          ^ metadata_json_string metadata
                          ^ " but got "
                          ^ metadata_json_string actual_metadata)
                      | None -> Error "expected installed metadata to be written"
                ))))

let test_upgrade_skips_when_binary_is_unchanged = fun ctx ->
  with_tempdir_result
    "upgrade-noop"
    (fun tempdir ->
      let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
      let home_dir = Path.(tempdir / Path.v "home") in
      let installed = Path.(home_dir / Path.v ".riot" / Path.v "bin" / Path.v "riot") in
      let metadata =
        make_metadata
          ~release_id:"v1.2.3"
          ~build_sha:"feedface1234"
          ~issues_url:"https://github.com/leostera/riot/issues"
          ()
      in
      let* riot_binary_path = Test.Context.require_binary ctx "riot" in
      let* binary = Result.map_err (Fs.read riot_binary_path) ~fn:IO.error_message in
      let* () = copy_executable_file ~src:riot_binary_path ~dst:installed in
      let* () = write_upgrade_archive ~metadata:(Some metadata) ~path:archive_path ~binary in
      let* before = Result.map_err (Fs.read installed) ~fn:IO.error_message in
      let* matches = parse_upgrade [ "upgrade" ] in
      let* installed_metadata =
        with_env
          ~name:"HOME"
          ~value:(Path.to_string home_dir)
          (fun () ->
            with_env
              ~name:"RIOT_UPGRADE_ARCHIVE_PATH"
              ~value:(Path.to_string archive_path)
              (fun () ->
                match Riot_cli.Upgrade.run matches with
                | Error exn -> Error (Exception.to_string exn)
                | Ok () -> Ok (Riot_cli.Version_info.read_installed ())))
      in
      let* after = Result.map_err (Fs.read installed) ~fn:IO.error_message in
      Test.assert_equal ~expected:before ~actual:after;
      match installed_metadata with
      | Some actual_metadata when actual_metadata = metadata -> Ok ()
      | Some actual_metadata ->
          Error ("unexpected installed metadata: expected "
          ^ metadata_json_string metadata
          ^ " but got "
          ^ metadata_json_string actual_metadata)
      | None -> Error "expected installed metadata to be written")

let test_version_string_uses_installed_metadata = fun _ctx ->
  with_tempdir_result
    "upgrade-version"
    (fun tempdir ->
      let home_dir = Path.(tempdir / Path.v "home") in
      let metadata =
        make_metadata
          ~release_id:"v3.2.1"
          ~build_sha:"cafebabe1234"
          ~issues_url:"https://github.com/leostera/riot/issues"
          ()
      in
      with_env
        ~name:"HOME"
        ~value:(Path.to_string home_dir)
        (fun () ->
          let* () = Riot_cli.Version_info.write_installed metadata in
          Test.assert_equal
            ~expected:"riot v3.2.1 (build cafebabe1234)"
            ~actual:(Riot_cli.Version_info.version_string ());
          Ok ()))

let test_version_string_uses_riot_dir_metadata = fun _ctx ->
  with_tempdir_result
    "upgrade-version-riot-dir"
    (fun tempdir ->
      let home_dir = Path.(tempdir / Path.v "home") in
      let riot_dir = Path.(tempdir / Path.v "custom-riot") in
      let metadata =
        make_metadata
          ~release_id:"v4.5.6"
          ~build_sha:"facefeed1234"
          ~issues_url:"https://github.com/leostera/riot/issues"
          ()
      in
      with_env
        ~name:"HOME"
        ~value:(Path.to_string home_dir)
        (fun () ->
          with_env
            ~name:"RIOT_DIR"
            ~value:(Path.to_string riot_dir)
            (fun () ->
              let* () = Riot_cli.Version_info.write_installed metadata in
              let* metadata_exists =
                Fs.exists Path.(riot_dir / Path.v "release.json")
                |> Result.map_err ~fn:IO.error_message
              in
              Test.assert_equal ~expected:true ~actual:metadata_exists;
              Test.assert_equal
                ~expected:"riot v4.5.6 (build facefeed1234)"
                ~actual:(Riot_cli.Version_info.version_string ());
              Ok ())))

let test_version_string_roundtrips_into_metadata = fun _ctx ->
  let expected = make_metadata ~release_id:"v7.8.9" ~build_sha:"beaded123456" () in
  match Riot_cli.Version_info.from_version_string "riot v7.8.9 (build beaded123456)" with
  | Some actual when actual = expected -> Ok ()
  | Some actual ->
      Error ("unexpected parsed version metadata: expected "
      ^ metadata_json_string expected
      ^ " but got "
      ^ metadata_json_string actual)
  | None -> Error "expected version string to parse into release metadata"

let tests =
  Test.[
    case "upgrade: parse --version flag" test_upgrade_accepts_version_flag;
    case
      "upgrade: version string uses installed metadata"
      test_version_string_uses_installed_metadata;
    case
      "upgrade: version string uses RIOT_DIR metadata"
      test_version_string_uses_riot_dir_metadata;
    case
      "upgrade: version string roundtrips into metadata"
      test_version_string_roundtrips_into_metadata;
    case ~size:Large "upgrade: installs downloaded archive" test_upgrade_installs_downloaded_archive;
    case
      ~size:Large
      "upgrade: skips when binary is unchanged"
      test_upgrade_skips_when_binary_is_unchanged;
  ]

let name = "Riot CLI Upgrade Tests"

let main ~args = Test.Cli.main ~execution_mode:Test.Cli.Linear ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
