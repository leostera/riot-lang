open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let tar_block_size = 512

let with_tempdir_result = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let write_file = fun path content ->
  match Fs.create_dir_all (Path.dirname path) with
  | Error err -> Error (IO.error_message err)
  | Ok () -> (
      match Fs.write content path with
      | Ok () -> Ok ()
      | Error err -> Error (IO.error_message err)
    )

let copy_executable_file = fun ~src ~dst ->
  let* () = Fs.create_dir_all (Path.dirname dst) |> Result.map_error IO.error_message in
  let* () = Fs.copy ~src ~dst |> Result.map_error IO.error_message in
  Fs.set_permissions dst Fs.Permissions.executable |> Result.map_error IO.error_message

let bytes_set_string = fun dst ~offset ~width value ->
  let bytes = IO.Bytes.of_string value in
  let copy_len = min width (IO.Bytes.length bytes) in
  IO.Bytes.blit bytes 0 dst offset copy_len

let octal_string = fun value ->
  let rec loop acc remaining =
    if Int64.equal remaining 0L then
      acc
    else
      let digit = Int64.to_int (Int64.rem remaining 8L) in
      let ch = Char.chr (Char.code '0' + digit) in
      loop (String.make 1 ch ^ acc) (Int64.div remaining 8L)
  in
  if Int64.equal value 0L then
    "0"
  else
    loop "" value

let zero_pad_left = fun width value ->
  if String.length value >= width then
    String.sub value (String.length value - width) width
  else
    String.make (width - String.length value) '0' ^ value

let bytes_set_octal = fun dst ~offset ~width value ->
  let digits_width = max 1 (width - 1) in
  let trimmed = zero_pad_left digits_width (octal_string value) in
  bytes_set_string dst ~offset ~width:(width - 1) trimmed;
  IO.Bytes.set dst (offset + width - 1) '\000'

let compute_checksum = fun header ->
  let sum = ref 0 in
  for index = 0 to tar_block_size - 1 do
    sum := !sum + Char.code (IO.Bytes.get header index)
  done;
  !sum

let make_header = fun ~name ~mode ~size ->
  let header = IO.Bytes.make tar_block_size '\000' in
  bytes_set_string header ~offset:0 ~width:100 name;
  bytes_set_octal header ~offset:100 ~width:8 mode;
  bytes_set_octal header ~offset:108 ~width:8 0L;
  bytes_set_octal header ~offset:116 ~width:8 0L;
  bytes_set_octal header ~offset:124 ~width:12 size;
  bytes_set_octal header ~offset:136 ~width:12 0L;
  bytes_set_string header ~offset:148 ~width:8 "        ";
  IO.Bytes.set header 156 '0';
  bytes_set_string header ~offset:257 ~width:6 "ustar";
  bytes_set_string header ~offset:263 ~width:2 "00";
  let checksum = compute_checksum header in
  let checksum_field = zero_pad_left 6 (octal_string (Int64.of_int checksum)) ^ "\000 " in
  bytes_set_string header ~offset:148 ~width:8 checksum_field;
  header

let pad_data = fun data ->
  let len = String.length data in
  let remainder = Int.rem len tar_block_size in
  if remainder = 0 then
    ""
  else
    String.make (tar_block_size - remainder) '\000'

let build_archive = fun ~binary ->
  let size = Int64.of_int (String.length binary) in
  let buffer = IO.Buffer.create 2_048 in
  IO.Buffer.add_bytes buffer (make_header ~name:"riot" ~mode:0o755L ~size);
  IO.Buffer.add_string buffer binary;
  IO.Buffer.add_string buffer (pad_data binary);
  IO.Buffer.add_string buffer (String.make (tar_block_size * 2) '\000');
  IO.Buffer.contents buffer

let write_upgrade_archive = fun ~path ~binary ->
  let tar_archive = build_archive ~binary in
  match Compress.Gzip.compress_string tar_archive with
  | Error _ -> Error "failed to gzip upgrade archive"
  | Ok gzip_payload -> write_file path gzip_payload

let with_env = fun ~name ~value fn ->
  let previous =
    try Some (Kernel.Env.getenv_exn name) with
    | Not_found -> None
  in
  Kernel.Fun.protect
    ~finally:(fun () ->
      match previous with
      | Some old -> Kernel.Env.putenv name old
      | None -> Kernel.Env.unsetenv name)
    (fun () ->
      Kernel.Env.putenv name value;
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

let test_upgrade_installs_downloaded_archive = fun _ctx ->
  with_tempdir_result "upgrade-install"
    (fun tempdir ->
      let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
      let home_dir = Path.(tempdir / Path.v "home") in
      let installed = Path.(home_dir / Path.v ".riot" / Path.v "bin" / Path.v "riot") in
      let riot_binary_path = Path.v "/Users/leostera/.riot/bin/riot" in
      let old_binary_path = Path.v "/bin/sh" in
      let* next_binary = Fs.read riot_binary_path |> Result.map_error IO.error_message in
      let* () = copy_executable_file ~src:old_binary_path ~dst:installed in
      let* () = write_upgrade_archive ~path:archive_path ~binary:next_binary in
      let* matches = parse_upgrade [ "upgrade" ] in
      with_env ~name:"HOME" ~value:(Path.to_string home_dir)
        (fun () ->
          with_env ~name:"RIOT_UPGRADE_ARCHIVE_PATH" ~value:(Path.to_string archive_path)
            (fun () ->
              match Riot_cli.Upgrade.run matches with
              | Error exn -> Error (Exception.to_string exn)
              | Ok () -> (
                  match Fs.read installed with
                  | Error err -> Error (IO.error_message err)
                  | Ok actual ->
                      Test.assert_equal ~expected:next_binary ~actual;
                      Ok ()
                ))))

let test_upgrade_skips_when_binary_is_unchanged = fun _ctx ->
  with_tempdir_result "upgrade-noop"
    (fun tempdir ->
      let archive_path = Path.(tempdir / Path.v "riot.tar.gz") in
      let home_dir = Path.(tempdir / Path.v "home") in
      let installed = Path.(home_dir / Path.v ".riot" / Path.v "bin" / Path.v "riot") in
      let riot_binary_path = Path.v "/Users/leostera/.riot/bin/riot" in
      let* binary = Fs.read riot_binary_path |> Result.map_error IO.error_message in
      let* () = copy_executable_file ~src:riot_binary_path ~dst:installed in
      let* () = write_upgrade_archive ~path:archive_path ~binary in
      let* before = Fs.read installed |> Result.map_error IO.error_message in
      let* matches = parse_upgrade [ "upgrade" ] in
      let* () =
        with_env ~name:"HOME" ~value:(Path.to_string home_dir)
          (fun () ->
            with_env ~name:"RIOT_UPGRADE_ARCHIVE_PATH" ~value:(Path.to_string archive_path)
              (fun () ->
                match Riot_cli.Upgrade.run matches with
                | Error exn -> Error (Exception.to_string exn)
                | Ok () -> Ok ()))
      in
      let* after = Fs.read installed |> Result.map_error IO.error_message in
      Test.assert_equal ~expected:before ~actual:after;
      Ok ())

let tests =
  Test.[
    case "upgrade: parse --version flag" test_upgrade_accepts_version_flag;
    case "upgrade: installs downloaded archive" test_upgrade_installs_downloaded_archive;
    case "upgrade: skips when binary is unchanged" test_upgrade_skips_when_binary_is_unchanged;
  ]

let name = "Riot CLI Upgrade Tests"

let () = Actors.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
