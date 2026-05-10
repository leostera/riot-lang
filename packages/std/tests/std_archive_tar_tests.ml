open Std
open Std.Archive

let tar_block_size = 512

let bytes_set_string = fun dst ~offset ~width value ->
  let copy_len = Int.min width (String.length value) in
  IO.Bytes.blit_string value ~src_offset:0 ~dst ~dst_offset:offset ~len:copy_len

let octal_string = fun value ->
  let rec loop acc remaining =
    if Int64.equal remaining 0L then
      acc
    else
      let digit = Int64.to_int (Int64.rem remaining 8L) in
      let ch = Char.from_int_unchecked (Char.to_int '0' + digit) in
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
  let digits_width = Int.max 1 (width - 1) in
  let trimmed = zero_pad_left digits_width (octal_string value) in
  bytes_set_string dst ~offset ~width:(width - 1) trimmed;
  IO.Bytes.set_unchecked dst ~at:(offset + width - 1) ~char:'\000'

let compute_checksum = fun header ->
  let sum = ref 0 in
  for index = 0 to tar_block_size - 1 do
    sum := !sum + Char.to_int (IO.Bytes.get_unchecked header ~at:index)
  done;
  !sum

let make_header = fun ~name ~kind ~mode ~size ->
  let header = IO.Bytes.create ~size:tar_block_size in
  IO.Bytes.fill header ~offset:0 ~len:tar_block_size ~char:'\000';
  bytes_set_string header ~offset:0 ~width:100 name;
  bytes_set_octal header ~offset:100 ~width:8 mode;
  bytes_set_octal header ~offset:108 ~width:8 0L;
  bytes_set_octal header ~offset:116 ~width:8 0L;
  bytes_set_octal header ~offset:124 ~width:12 size;
  bytes_set_octal header ~offset:136 ~width:12 0L;
  bytes_set_string header ~offset:148 ~width:8 "        ";
  IO.Bytes.set_unchecked header ~at:156 ~char:kind;
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

let build_archive = fun entries ->
  let buffer = IO.Buffer.create ~size:2_048 in
  List.for_each
    entries
    ~fn:(fun (name, kind, mode, data) ->
      let size = Int64.from_int (String.length data) in
      IO.Buffer.add_bytes buffer (make_header ~name ~kind ~mode ~size);
      IO.Buffer.add_string buffer data;
      IO.Buffer.add_string buffer (pad_data data));
  IO.Buffer.add_string buffer (String.make ~len:(tar_block_size * 2) ~char:'\000');
  IO.Buffer.contents buffer

let test_entries_lists_archive_members = fun _ctx ->
  let archive =
    build_archive [ ("src/", '5', 0o755L, ""); ("src/hello.txt", '0', 0o644L, "hello tar\n"); ]
  in
  match Tar.entries (IO.Reader.from_string archive) with
  | Error _ -> Error "failed to list tar entries"
  | Ok entries ->
      let paths = List.map entries ~fn:(fun (entry: Tar.entry) -> Path.to_string entry.path) in
      if paths = [ "src/"; "src/hello.txt" ] || paths = [ "src"; "src/hello.txt" ] then
        Ok ()
      else
        Error ("unexpected tar entries: " ^ String.concat ", " paths)

let with_temp_dir = fun label fn ->
  let temp_root =
    Path.join (Path.v "/tmp") (Path.v ("riot_" ^ label ^ "_" ^ UUID.to_string (UUID.v4 ())))
  in
  match Fs.create_dir_all temp_root with
  | Error err -> Error ("failed to create temp dir: " ^ IO.error_message err)
  | Ok () ->
      let result = fn temp_root in
      let cleanup = Fs.remove_dir_all temp_root in
      (
        match (result, cleanup) with
        | (Error err, _) -> Error err
        | (Ok (), Ok ()) -> Ok ()
        | (Ok (), Error err) -> Error ("failed to remove temp dir: " ^ IO.error_message err)
      )

let test_extract_writes_regular_files = fun _ctx ->
  let archive =
    build_archive [ ("pkg/", '5', 0o755L, ""); ("pkg/README.md", '0', 0o644L, "Hello from tar\n"); ]
  in
  with_temp_dir
    "tar_extract"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive"
      | Ok () ->
          let readme = Path.join (Path.join dir (Path.v "pkg")) (Path.v "README.md") in
          match Fs.read_to_string readme with
          | Ok content when content = "Hello from tar\n" -> Ok ()
          | Ok content -> Error ("unexpected extracted content: " ^ content)
          | Error err -> Error ("failed to read extracted file: " ^ IO.error_message err))

let test_extract_allows_dot_root_directory_entry = fun _ctx ->
  let archive =
    build_archive
      [
        ("./", '5', 0o755L, "");
        ("./src/", '5', 0o755L, "");
        ("./src/std.ml", '0', 0o644L, "let answer = 42\n");
      ]
  in
  with_temp_dir
    "tar_dot_root"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive with dot root entry"
      | Ok () ->
          let extracted = Path.join (Path.join dir (Path.v "src")) (Path.v "std.ml") in
          match Fs.read_to_string extracted with
          | Ok "let answer = 42\n" -> Ok ()
          | Ok text -> Error ("unexpected extracted dot-root content: " ^ text)
          | Error err -> Error ("failed to read dot-root extracted file: " ^ IO.error_message err))

let test_extract_rejects_path_traversal = fun _ctx ->
  let archive = build_archive [ ("../escape.txt", '0', 0o644L, "bad"); ] in
  with_temp_dir
    "tar_traversal"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error (Tar.Extract_error (Tar.Unsafe_path _)) -> Ok ()
      | Error _ -> Error "expected unsafe-path rejection"
      | Ok () -> Error "tar extraction should reject path traversal")

let test_extract_skips_pax_extended_headers = fun _ctx ->
  let archive =
    build_archive
      [
        ("paxheader", 'x', 0o644L, "25 path=./src/std.ml\n");
        ("src/", '5', 0o755L, "");
        ("src/std.ml", '0', 0o644L, "let answer = 42\n");
      ]
  in
  with_temp_dir
    "tar_pax_header"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive with pax header"
      | Ok () ->
          let extracted = Path.join (Path.join dir (Path.v "src")) (Path.v "std.ml") in
          match Fs.read_to_string extracted with
          | Ok "let answer = 42\n" -> Ok ()
          | Ok text -> Error ("unexpected extracted pax-header content: " ^ text)
          | Error err -> Error ("failed to read pax-header extracted file: " ^ IO.error_message err))

let test_extract_skips_appledouble_entries = fun _ctx ->
  let archive =
    build_archive
      [
        ("src/", '5', 0o755L, "");
        ("src/._std.ml", '0', 0o644L, "appledouble");
        ("src/std.ml", '0', 0o644L, "let answer = 42\n");
        ("__MACOSX/", '5', 0o755L, "");
        ("__MACOSX/.DS_Store", '0', 0o644L, "junk");
      ]
  in
  with_temp_dir
    "tar_appledouble"
    (fun dir ->
      match Tar.extract (IO.Reader.from_string archive) ~into:dir with
      | Error _ -> Error "failed to extract tar archive with AppleDouble entries"
      | Ok () ->
          let extracted = Path.join (Path.join dir (Path.v "src")) (Path.v "std.ml") in
          let skipped = Path.join (Path.join dir (Path.v "src")) (Path.v "._std.ml") in
          match (Fs.read_to_string extracted, Fs.exists skipped) with
          | (Ok "let answer = 42\n", Ok false) -> Ok ()
          | (Ok text, _) -> Error ("unexpected extracted AppleDouble content: " ^ text)
          | (Error err, _) -> Error ("failed to read extracted file: " ^ IO.error_message err))

let tests =
  Test.[
    case "tar entries lists archive members" test_entries_lists_archive_members;
    case "tar extract writes regular files" test_extract_writes_regular_files;
    case "tar extract allows dot root directory entry" test_extract_allows_dot_root_directory_entry;
    case "tar extract rejects path traversal" test_extract_rejects_path_traversal;
    case "tar extract skips pax extended headers" test_extract_skips_pax_extended_headers;
    case "tar extract skips AppleDouble entries" test_extract_skips_appledouble_entries;
  ]

let main ~args = Test.Cli.main ~name:"std_archive_tar" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
