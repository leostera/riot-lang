open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.of_string (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_temp_path = fun prefix filename fn ->
  match
    Fs.with_tempdir ~prefix
      (fun tempdir ->
        let path = Kernel.Path.(Path.to_string tempdir / filename) in
        fn path)
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_file = fun file fn ->
  protect
    ~finally:(fun () ->
      let _ = Kernel.Fs.File.close file in
      ())
    fn

let array_contains = fun values target ->
  let rec loop index =
    if index = Kernel.Array.length values then
      false
    else if Kernel.String.equal (Kernel.Array.get values index) target then
      true
    else
      loop (index + 1)
  in
  loop 0

let array_has_exact_members = fun actual expected ->
  let rec members_present index =
    if index = Kernel.Array.length expected then
      true
    else if array_contains actual (Kernel.Array.get expected index) then
      members_present (index + 1)
    else
      false
  in
  Kernel.Array.length actual = Kernel.Array.length expected && members_present 0

let test_file_scalar_write_roundtrips = fun _ctx ->
  with_temp_path "kernel_new_file" "scalar.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.of_string "hello kernel-new" in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected full scalar write")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.create (Kernel.Bytes.length payload) in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf 0 read))
      in
      if Kernel.String.equal actual "hello kernel-new" then
        Ok ()
      else
        Error "expected scalar file roundtrip to preserve payload")

let test_file_vectored_write_roundtrips = fun _ctx ->
  with_temp_path "kernel_new_file" "vectored.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.IO.Iovec.of_string_array [|"hello"; " "; "vectored"; " "; "world"|] in
      let* () =
        with_file file
          (fun () ->
            let expected = Kernel.IO.Iovec.length payload in
            let* written = lift (Kernel.Fs.File.write_vectored file payload) in
            if written = expected then
              Ok ()
            else
              Error "expected full vectored write")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.create 64 in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf 0 read))
      in
      if Kernel.String.equal actual "hello vectored world" then
        Ok ()
      else
        Error "expected vectored file roundtrip to preserve payload")

let test_file_read_and_write_respect_pos_and_len = fun _ctx ->
  with_temp_path "kernel_new_file" "slice.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.of_string "__payload__" in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file ~pos:2 ~len:7 payload) in
            if written = 7 then
              Ok ()
            else
              Error "expected partial file write to write the requested slice")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.of_string "xxx_______yyy" in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file ~pos:3 ~len:7 buf) in
            Ok (read, Kernel.Bytes.to_string buf))
      in
      let read, contents = actual in
      if read = 7 && Kernel.String.equal contents "xxxpayloadyyy" then
        Ok ()
      else
        Error "expected partial file read to fill only the requested slice")

let test_create_dir_and_read_dir_names = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let child_dir = Kernel.Path.(tempdir / "child") in
      let child_file = Kernel.Path.(tempdir / "alpha.txt") in
      let* () = lift (Kernel.Fs.File.create_dir child_dir ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "a")) in
            Ok ())
      in
      let* names = lift (Kernel.Fs.File.read_dir_names tempdir) in
      let has_child =
        Kernel.Array.fold_left (fun found name -> found || Kernel.String.equal name "child") false names
      in
      let has_file =
        Kernel.Array.fold_left
          (fun found name -> found || Kernel.String.equal name "alpha.txt")
          false
          names
      in
      let* metadata = lift (Kernel.Fs.File.metadata child_dir) in
      if has_child && has_file && Kernel.Fs.File.Metadata.is_dir metadata then
        Ok ()
      else
        Error "expected directory creation and read_dir_names to expose entries")

let test_symlink_metadata_and_canonicalize = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "latest") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "kernel")) in
            Ok ())
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_file target in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
          let* link_target = lift (Kernel.Fs.File.read_link link) in
          let* followed = lift (Kernel.Fs.File.metadata link) in
          let* raw = lift (Kernel.Fs.File.symlink_metadata link) in
          let* canonical = lift (Kernel.Fs.File.canonicalize link) in
          let* canonical_target = lift (Kernel.Fs.File.canonicalize target) in
          let link_target_matches = Kernel.String.equal
            (Kernel.Path.to_string link_target)
            (Kernel.Path.to_string target) in
          let canonical_matches = Kernel.String.equal
            (Kernel.Path.to_string canonical)
            (Kernel.Path.to_string canonical_target) in
          let followed_is_file = Kernel.Fs.File.Metadata.is_file followed in
          let raw_is_symlink = Kernel.Fs.File.Metadata.is_symlink raw in
          if link_target_matches && followed_is_file && raw_is_symlink && canonical_matches then
            Ok ()
          else
            Error "expected symlink metadata and canonicalize to distinguish link from target"))

let test_dangling_symlink_still_has_symlink_metadata = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "dangling") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "kernel")) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.remove_file target) in
          let* exists = lift (Kernel.Fs.File.exists link) in
          let* metadata = lift (Kernel.Fs.File.symlink_metadata link) in
          if not exists && Kernel.Fs.File.Metadata.is_symlink metadata then
            Ok ()
          else
            Error "expected dangling symlink_metadata to preserve symlink kind"))

let test_lstat_matches_symlink_metadata = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "kernel")) in
            Ok ())
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_file target in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
          let* by_lstat = lift (Kernel.Fs.File.lstat link) in
          let* by_symlink_metadata = lift (Kernel.Fs.File.symlink_metadata link) in
          if
            Kernel.Fs.File.Metadata.is_symlink by_lstat
            && Kernel.Fs.File.Metadata.is_symlink by_symlink_metadata
            && Kernel.Fs.File.Metadata.ino by_lstat = Kernel.Fs.File.Metadata.ino by_symlink_metadata
          then
            Ok ()
          else
            Error "expected lstat to match symlink_metadata for a symbolic link"))

let test_metadata_follows_symlink_but_remove_only_unlinks_symlink = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "kernel")) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
      let* followed = lift (Kernel.Fs.File.metadata link) in
      let* raw = lift (Kernel.Fs.File.symlink_metadata link) in
      let* () = lift (Kernel.Fs.File.remove_file link) in
      let* target_exists = lift (Kernel.Fs.File.exists target) in
      let* link_exists = lift (Kernel.Fs.File.exists link) in
      if
        Kernel.Fs.File.Metadata.is_file followed
        && Kernel.Fs.File.Metadata.is_symlink raw
        && target_exists
        && not link_exists
      then
        Ok ()
      else
        Error "expected metadata to follow symlinks while removing the symlink leaves the target intact")

let test_copy_and_rename_roundtrip = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let copied = Kernel.Path.(tempdir / "copied.txt") in
      let renamed = Kernel.Path.(tempdir / "renamed.txt") in
      let payload = Kernel.Bytes.of_string "copy me" in
      let* file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file payload) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.copy ~src:source ~dst:copied) in
      let* () = lift (Kernel.Fs.File.rename ~src:copied ~dst:renamed) in
      let* file = lift (Kernel.Fs.File.open_read renamed) in
      let buf = Kernel.Bytes.create 16 in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf 0 read))
      in
      let* exists = lift (Kernel.Fs.File.exists renamed) in
      if exists && Kernel.String.equal actual "copy me" then
        Ok ()
      else
        Error "expected copy and rename to preserve payload")

let test_fstat_matches_path_metadata = fun _ctx ->
  with_temp_path "kernel_new_file" "metadata.bin"
    (fun path ->
      let payload = Kernel.Bytes.of_string "metadata" in
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* (path_metadata, fd_metadata) =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file payload) in
            let* fd_metadata = lift (Kernel.Fs.File.fstat file) in
            let* path_metadata = lift (Kernel.Fs.File.metadata path) in
            Ok (path_metadata, fd_metadata))
      in
      if
        Kernel.Fs.File.Metadata.ino path_metadata = Kernel.Fs.File.Metadata.ino fd_metadata
        && Kernel.Fs.File.Metadata.len path_metadata = Kernel.Fs.File.Metadata.len fd_metadata
        && Kernel.Fs.File.Metadata.nlink path_metadata = Kernel.Fs.File.Metadata.nlink fd_metadata
      then
        Ok ()
      else
        Error "expected fstat metadata to match path metadata for the same file")

let test_hard_link_updates_link_count_and_remove_ops = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let link = Kernel.Path.(tempdir / "linked.txt") in
      let directory = Kernel.Path.(tempdir / "child") in
      let* file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "linked")) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.hard_link ~src:source ~dst:link) in
      let* source_metadata = lift (Kernel.Fs.File.metadata source) in
      let* link_metadata = lift (Kernel.Fs.File.metadata link) in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      let* () = lift (Kernel.Fs.File.remove_file link) in
      let* link_exists = lift (Kernel.Fs.File.exists link) in
      let* () = lift (Kernel.Fs.File.remove_dir directory) in
      let* directory_exists = lift (Kernel.Fs.File.exists directory) in
      if
        Kernel.Fs.File.Metadata.nlink source_metadata = 2
        && Kernel.Fs.File.Metadata.ino source_metadata = Kernel.Fs.File.Metadata.ino link_metadata
        && not link_exists
        && not directory_exists
      then
        Ok ()
      else
        Error "expected hard_link to share metadata and remove ops to clean up paths")

let test_remove_nonempty_dir_reports_resource_busy = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "child") in
      let child_file = Kernel.Path.(directory / "entry.txt") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "x")) in
            Ok ())
      in
      match Kernel.Fs.File.remove_dir directory with
      | Kernel.Result.Error (Kernel.Fs.File.System Kernel.SystemError.DirectoryNotEmpty) -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () -> Error "expected removing a non-empty directory to fail")

let test_exists_and_is_directory_report_expected_kinds = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "dir") in
      let file_path = Kernel.Path.(tempdir / "entry.txt") in
      let missing = Kernel.Path.(tempdir / "missing.txt") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write file_path) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "x")) in
            Ok ())
      in
      let* dir_exists = lift (Kernel.Fs.File.exists directory) in
      let* file_exists = lift (Kernel.Fs.File.exists file_path) in
      let* missing_exists = lift (Kernel.Fs.File.exists missing) in
      let* dir_is_directory = lift (Kernel.Fs.File.is_directory directory) in
      let* file_is_directory = lift (Kernel.Fs.File.is_directory file_path) in
      if
        dir_exists && file_exists && not missing_exists && dir_is_directory && not file_is_directory
      then
        Ok ()
      else
        Error "expected exists and is_directory to distinguish directories, files, and missing paths")

let test_read_vectored_roundtrips = fun _ctx ->
  with_temp_path "kernel_new_file" "readv.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.of_string "hello vectored read" in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected scalar write to seed vectored read fixture")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let iov = Kernel.IO.Iovec.create ~count:3 ~size:(Kernel.Bytes.length payload) () in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read_vectored file iov) in
            Ok (read, Kernel.IO.Iovec.into_string iov))
      in
      let read, contents = actual in
      let prefix = Kernel.Bytes.sub_string (Kernel.Bytes.of_string contents) 0 read in
      if read = Kernel.Bytes.length payload && Kernel.String.equal prefix "hello vectored read" then
        Ok ()
      else
        Error "expected read_vectored to preserve payload across iovec segments")

let test_is_tty_is_false_for_files_and_pipes = fun _ctx ->
  with_temp_path "kernel_new_file" "tty.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let file_is_tty =
        with_file file (fun () -> Ok (Kernel.Fs.File.is_tty file))
      in
      let* file_is_tty = file_is_tty in
      let* pipe = lift (Kernel.Fs.File.pipe ()) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close pipe.read_end in
          let _ = Kernel.Fs.File.close pipe.write_end in
          ())
        (fun () ->
          let pipe_is_tty = Kernel.Fs.File.is_tty pipe.read_end in
          if not file_is_tty && not pipe_is_tty then
            Ok ()
          else
            Error "expected regular files and pipes to report non-tty"))

let test_open_read_missing_file_maps_error = fun _ctx ->
  with_temp_path "kernel_new_file" "missing.bin"
    (fun path ->
      match Kernel.Fs.File.open_read path with
      | Kernel.Result.Ok _ -> Error "expected opening a missing file to fail"
      | Kernel.Result.Error (Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory) -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.String.append
        "expected no-such-file error, got "
        (Kernel.Fs.File.error_to_string error)))

let test_remove_missing_paths_report_no_such_file = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let missing_file = Kernel.Path.(tempdir / "missing.txt") in
      let missing_dir = Kernel.Path.(tempdir / "missing-dir") in
      match (Kernel.Fs.File.remove_file missing_file, Kernel.Fs.File.remove_dir missing_dir) with
      | (Kernel.Result.Error (Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory), Kernel.Result.Error (Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory)) -> Ok ()
      | _ -> Error "expected removing missing file and dir to report no-such-file")

let test_repeated_pipe_open_and_close_stays_healthy = fun _ctx ->
  let rec loop remaining =
    if remaining = 0 then
      Ok ()
    else
      let* pipe = lift (Kernel.Fs.File.pipe ()) in
      let* () = lift (Kernel.Fs.File.close pipe.read_end) in
      let* () = lift (Kernel.Fs.File.close pipe.write_end) in
      loop (remaining - 1)
  in
  loop 256

let test_read_dir_names_skips_dot_entries_and_is_order_agnostic = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let nested = Kernel.Path.(tempdir / "nested") in
      let file_path = Kernel.Path.(tempdir / "note.txt") in
      let* () = lift (Kernel.Fs.File.create_dir nested ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write file_path) in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "x")) in
            if written = 1 then
              Ok ()
            else
              Error "expected directory fixture file write to make progress")
      in
      let* names = lift (Kernel.Fs.File.read_dir_names tempdir) in
      let expected = [|"nested"; "note.txt"|] in
      if
        not (array_contains names ".")
        && not (array_contains names "..")
        && array_has_exact_members names expected
      then
        Ok ()
      else
        Error "expected read_dir_names to skip dot entries and preserve visible contents")

let test_nested_symlink_chain_canonicalizes_cleanly = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "link") in
      let nested_link = Kernel.Path.(tempdir / "nested-link") in
      let target_ref = Kernel.Path.of_string "target.txt" in
      let link_ref = Kernel.Path.of_string "link" in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "kernel")) in
            if written = 6 then
              Ok ()
            else
              Error "expected target file write to make progress")
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file nested_link in
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_file target in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target_ref ~dst:link) in
          let* () = lift (Kernel.Fs.File.symlink ~src:link_ref ~dst:nested_link) in
          let* canonical = lift (Kernel.Fs.File.canonicalize nested_link) in
          let* canonical_target = lift (Kernel.Fs.File.canonicalize target) in
          if
            not
              (Kernel.String.equal
                (Kernel.Path.to_string canonical)
                (Kernel.Path.to_string canonical_target))
          then
            Error "expected nested symlink canonicalize to resolve to the final target"
          else
            let* target_path = lift (Kernel.Fs.File.read_link nested_link) in
            if not (Kernel.String.equal (Kernel.Path.to_string target_path) "link") then
              Error "expected read_link to preserve the immediate nested symlink target"
            else
              Ok ()))

let test_hard_link_rename_preserves_remaining_link_count = fun _ctx ->
  with_tempdir "kernel_new_file"
    (fun tempdir ->
      let original = Kernel.Path.(tempdir / "original.txt") in
      let alias = Kernel.Path.(tempdir / "alias.txt") in
      let moved = Kernel.Path.(tempdir / "moved.txt") in
      let* file = lift (Kernel.Fs.File.open_write original) in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.of_string "linked")) in
            if written = 6 then
              Ok ()
            else
              Error "expected hard-link fixture write to make progress")
      in
      let* () = lift (Kernel.Fs.File.hard_link ~src:original ~dst:alias) in
      let* () = lift (Kernel.Fs.File.rename ~src:original ~dst:moved) in
      let* moved_stats = lift (Kernel.Fs.File.metadata moved) in
      let* alias_stats = lift (Kernel.Fs.File.metadata alias) in
      if
        Kernel.Fs.File.Metadata.nlink moved_stats != 2
        || Kernel.Fs.File.Metadata.nlink alias_stats != 2
      then
        Error "expected renamed hard links to keep a link count of two"
      else
        let* () = lift (Kernel.Fs.File.remove_file alias) in
        let* remaining = lift (Kernel.Fs.File.metadata moved) in
        let* original_exists = lift (Kernel.Fs.File.exists original) in
        if Kernel.Fs.File.Metadata.nlink remaining = 1 && not original_exists then
          Ok ()
        else
          Error "expected removing one hard-link path to leave a single remaining name")

let test_vectored_write_subslice_roundtrips = fun _ctx ->
  with_temp_path "kernel_new_file" "vectored-subslice.bin"
    (fun path ->
      let payload = Kernel.IO.Iovec.of_string_array [|"__"; "hello"; " "; "kernel"; "__"|] in
      let slice = Kernel.IO.Iovec.sub ~pos:2 ~len:12 payload in
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* () =
        with_file file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write_vectored file slice) in
            if written = 12 then
              Ok ()
            else
              Error "expected vectored subslice write to write the selected slice only")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.create 32 in
      let* actual =
        with_file file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buffer) in
            Ok (Kernel.Bytes.sub_string buffer 0 read))
      in
      if Kernel.String.equal actual "hello kernel" then
        Ok ()
      else
        Error "expected vectored subslice roundtrip to preserve the selected payload")

let tests = [
  Test.case "Fs.File scalar write roundtrips" test_file_scalar_write_roundtrips;
  Test.case "Fs.File vectored write roundtrips" test_file_vectored_write_roundtrips;
  Test.case "Fs.File read and write respect pos and len" test_file_read_and_write_respect_pos_and_len;
  Test.case "Fs.File create_dir and read_dir_names" test_create_dir_and_read_dir_names;
  Test.case "Fs.File symlink metadata and canonicalize" test_symlink_metadata_and_canonicalize;
  Test.case "Fs.File dangling symlink still reports symlink metadata" test_dangling_symlink_still_has_symlink_metadata;
  Test.case "Fs.File lstat matches symlink_metadata" test_lstat_matches_symlink_metadata;
  Test.case "Fs.File metadata follows symlink but remove_file only unlinks the symlink" test_metadata_follows_symlink_but_remove_only_unlinks_symlink;
  Test.case "Fs.File copy and rename roundtrips" test_copy_and_rename_roundtrip;
  Test.case "Fs.File fstat matches path metadata" test_fstat_matches_path_metadata;
  Test.case "Fs.File hard_link and remove ops update filesystem state" test_hard_link_updates_link_count_and_remove_ops;
  Test.case "Fs.File remove non-empty dir reports an error" test_remove_nonempty_dir_reports_resource_busy;
  Test.case "Fs.File exists and is_directory report expected kinds" test_exists_and_is_directory_report_expected_kinds;
  Test.case "Fs.File read_vectored roundtrips" test_read_vectored_roundtrips;
  Test.case "Fs.File is_tty is false for files and pipes" test_is_tty_is_false_for_files_and_pipes;
  Test.case "Fs.File missing read maps kernel error" test_open_read_missing_file_maps_error;
  Test.case "Fs.File remove missing paths reports no-such-file" test_remove_missing_paths_report_no_such_file;
  Test.case "Fs.File read_dir_names skips dot entries and is order agnostic" test_read_dir_names_skips_dot_entries_and_is_order_agnostic;
  Test.case "Fs.File nested symlink chains canonicalize cleanly" test_nested_symlink_chain_canonicalizes_cleanly;
  Test.case "Fs.File hard-link rename preserves remaining link counts" test_hard_link_rename_preserves_remaining_link_count;
  Test.case "Fs.File vectored write subslices roundtrip" test_vectored_write_subslice_roundtrips;
  Test.case ~size:Test.Large "Fs.File repeated pipe open and close stays healthy" test_repeated_pipe_open_and_close_stays_healthy;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_file_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
