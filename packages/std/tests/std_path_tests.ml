open Std

module Test = Std.Test
module Bytes = Kernel.Bytes

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let invalid_utf8_string = fun () ->
  let bytes = Bytes.create ~size:1 in
  Bytes.set_unchecked bytes ~at:0 ~char:(Char.from_int_unchecked 255);
  Bytes.to_string bytes

let test_from_string_accepts_valid_utf8 = fun _ctx ->
  match Path.from_string "/tmp/alpha" with
  | Ok path when String.equal (Path.to_string path) "/tmp/alpha" -> Ok ()
  | Ok _ -> Error "Path.from_string returned the wrong path"
  | Error _ -> Error "Path.from_string should accept valid UTF-8"

let test_from_string_rejects_invalid_utf8 = fun _ctx ->
  match Path.from_string (invalid_utf8_string ()) with
  | Error (Path.InvalidUtf8 _) -> Ok ()
  | Error _ -> Error "Path.from_string returned the wrong error for invalid UTF-8"
  | Ok _ -> Error "Path.from_string should reject invalid UTF-8"

let test_to_string_roundtrips_literal_paths = fun _ctx ->
  let path = Path.v "/tmp/roundtrip.txt" in
  if String.equal (Path.to_string path) "/tmp/roundtrip.txt" then
    Ok ()
  else
    Error "Path.to_string should return the original path string"

let test_join_appends_relative_paths = fun _ctx ->
  let joined = Path.join (Path.v "/usr") (Path.v "local/bin") in
  if String.equal (Path.to_string joined) "/usr/local/bin" then
    Ok ()
  else
    Error "Path.join should append relative paths"

let test_join_replaces_with_absolute_paths = fun _ctx ->
  let joined = Path.join (Path.v "/usr/local") (Path.v "/etc/hosts") in
  if String.equal (Path.to_string joined) "/etc/hosts" then
    Ok ()
  else
    Error "Path.join should replace the base when the second path is absolute"

let test_infix_join_chains_components = fun _ctx ->
  let joined = Path.(Path.v "/home" / Path.v "user" / Path.v "docs") in
  if String.equal (Path.to_string joined) "/home/user/docs" then
    Ok ()
  else
    Error "Path.( / ) should chain components naturally"

let test_parent_of_nested_absolute_path = fun _ctx ->
  match Path.parent (Path.v "/a/b/c.txt") with
  | Some parent when String.equal (Path.to_string parent) "/a/b" -> Ok ()
  | Some _ -> Error "Path.parent returned the wrong parent"
  | None -> Error "Path.parent should return the containing directory"

let test_parent_of_root_is_none = fun _ctx ->
  match Path.parent (Path.v "/") with
  | None -> Ok ()
  | Some _ -> Error "Path.parent should return None for the filesystem root"

let test_basename_returns_last_component = fun _ctx ->
  if String.equal (Path.basename (Path.v "/var/log/system.log")) "system.log" then
    Ok ()
  else
    Error "Path.basename should return the last component"

let test_dirname_returns_directory_part = fun _ctx ->
  if String.equal (Path.to_string (Path.dirname (Path.v "/var/log/system.log"))) "/var/log" then
    Ok ()
  else
    Error "Path.dirname should return the directory portion"

let test_remove_extension_strips_only_the_last_suffix = fun _ctx ->
  let path = Path.remove_extension (Path.v "/tmp/archive.tar.gz") in
  if String.equal (Path.to_string path) "/tmp/archive.tar" then
    Ok ()
  else
    Error "Path.remove_extension should strip only the last suffix"

let test_add_extension_adds_missing_dot = fun _ctx ->
  let path = Path.add_extension (Path.v "report") ~ext:"txt" in
  if String.equal (Path.to_string path) "report.txt" then
    Ok ()
  else
    Error "Path.add_extension should add a leading dot when needed"

let test_add_extension_preserves_existing_dot = fun _ctx ->
  let path = Path.add_extension (Path.v "report") ~ext:".txt" in
  if String.equal (Path.to_string path) "report.txt" then
    Ok ()
  else
    Error "Path.add_extension should not duplicate an existing leading dot"

let test_replace_extension_swaps_the_last_suffix = fun _ctx ->
  let path = Path.replace_extension (Path.v "/tmp/report.txt") ~ext:"md" in
  if String.equal (Path.to_string path) "/tmp/report.md" then
    Ok ()
  else
    Error "Path.replace_extension should replace the last suffix"

let test_is_absolute_recognizes_rooted_paths = fun _ctx ->
  if Path.is_absolute (Path.v "/tmp") && not (Path.is_absolute (Path.v "tmp")) then
    Ok ()
  else
    Error "Path.is_absolute should recognize rooted paths only"

let test_is_relative_recognizes_non_rooted_paths = fun _ctx ->
  if
    Path.is_relative (Path.v "tmp/file.txt") && not (Path.is_relative (Path.v "/tmp/file.txt"))
  then
    Ok ()
  else
    Error "Path.is_relative should recognize non-rooted paths only"

let test_components_preserve_root_and_segments = fun _ctx ->
  let parts =
    Path.components (Path.v "/usr/local/bin")
    |> List.map ~fn:Path.to_string
  in
  if parts = [ "/"; "usr"; "local"; "bin"; ] then
    Ok ()
  else
    Error "Path.components should preserve the root marker and each segment"

let test_normalize_resolves_dot_and_dotdot_segments = fun _ctx ->
  let normalized = Path.normalize (Path.v "/home/user/../admin/./config") in
  if String.equal (Path.to_string normalized) "/home/admin/config" then
    Ok ()
  else
    Error "Path.normalize should resolve dot and dotdot segments"

let test_equal_compares_normalized_paths = fun _ctx ->
  if Path.equal (Path.v "/home/./user") (Path.v "/home/user") then
    Ok ()
  else
    Error "Path.equal should compare normalized paths"

let test_compare_compares_normalized_paths = fun _ctx ->
  if Path.compare (Path.v "/home/./user") (Path.v "/home/user") = Order.EQ then
    Ok ()
  else
    Error "Path.compare should compare normalized paths"

let test_strip_prefix_returns_relative_remainder = fun _ctx ->
  match Path.strip_prefix (Path.v "/home/user/docs/file.txt") ~prefix:(Path.v "/home/user") with
  | Ok remainder when String.equal (Path.to_string remainder) "docs/file.txt" -> Ok ()
  | Ok _ -> Error "Path.strip_prefix returned the wrong remainder"
  | Error _ -> Error "Path.strip_prefix should accept valid prefixes"

let test_strip_prefix_rejects_non_prefixes = fun _ctx ->
  match Path.strip_prefix (Path.v "/home/user/docs/file.txt") ~prefix:(Path.v "/tmp") with
  | Error _ -> Ok ()
  | Ok _ -> Error "Path.strip_prefix should reject non-prefix paths"

let test_exists_is_false_for_missing_paths = fun _ctx ->
  with_tempdir
    "std_path_missing"
    (fun dir ->
      let missing = Path.(dir / Path.v "missing.txt") in
      if not (Path.exists missing) then
        Ok ()
      else
        Error "Path.exists should be false for missing paths")

let test_is_directory_is_true_for_created_directories = fun _ctx ->
  with_tempdir
    "std_path_dir"
    (fun dir ->
      if Path.exists dir && Path.is_directory dir && not (Path.is_file dir) then
        Ok ()
      else
        Error "Path.is_directory should report the temp directory as a directory")

let test_is_file_is_true_for_created_files = fun _ctx ->
  with_tempdir
    "std_path_file"
    (fun dir ->
      let file = Path.(dir / Path.v "payload.txt") in
      match Fs.write "payload" file with
      | Error err -> Error ("failed to create file: " ^ IO.error_message err)
      | Ok () ->
          if Path.exists file && Path.is_file file && not (Path.is_directory file) then
            Ok ()
          else
            Error "Path.is_file should report created files as files")

let tests =
  Test.[
    case "from_string accepts valid UTF-8" test_from_string_accepts_valid_utf8;
    case "from_string rejects invalid UTF-8" test_from_string_rejects_invalid_utf8;
    case "to_string roundtrips path literals" test_to_string_roundtrips_literal_paths;
    case "join appends relative paths" test_join_appends_relative_paths;
    case "join replaces with absolute paths" test_join_replaces_with_absolute_paths;
    case "infix join chains components" test_infix_join_chains_components;
    case "parent returns the containing directory" test_parent_of_nested_absolute_path;
    case "parent of root is None" test_parent_of_root_is_none;
    case "basename returns the last component" test_basename_returns_last_component;
    case "dirname returns the directory portion" test_dirname_returns_directory_part;
    case "remove_extension strips the last suffix" test_remove_extension_strips_only_the_last_suffix;
    case "add_extension inserts a missing dot" test_add_extension_adds_missing_dot;
    case "add_extension preserves an existing dot" test_add_extension_preserves_existing_dot;
    case "replace_extension swaps the last suffix" test_replace_extension_swaps_the_last_suffix;
    case "is_absolute recognizes rooted paths" test_is_absolute_recognizes_rooted_paths;
    case "is_relative recognizes non-rooted paths" test_is_relative_recognizes_non_rooted_paths;
    case "components preserve root and segments" test_components_preserve_root_and_segments;
    case
      "normalize resolves dot and dotdot segments"
      test_normalize_resolves_dot_and_dotdot_segments;
    case "equal compares normalized paths" test_equal_compares_normalized_paths;
    case "compare compares normalized paths" test_compare_compares_normalized_paths;
    case "strip_prefix returns the relative remainder" test_strip_prefix_returns_relative_remainder;
    case "strip_prefix rejects non-prefixes" test_strip_prefix_rejects_non_prefixes;
    case "exists is false for missing paths" test_exists_is_false_for_missing_paths;
    case "is_directory reports directories" test_is_directory_is_true_for_created_directories;
    case "is_file reports files" test_is_file_is_true_for_created_files;
  ]

let main ~args = Test.Cli.main ~name:"path" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
