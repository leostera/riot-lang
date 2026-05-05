open Std

module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

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
  match Fs.with_tempdir
    ~prefix
    (fun tempdir -> fn (Kernel.Path.from_string (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let with_temp_path = fun prefix filename fn ->
  match Fs.with_tempdir
    ~prefix
    (fun tempdir ->
      let path = Kernel.Path.(Path.to_string tempdir / filename) in
      fn path) with
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
    else if Kernel.String.equal (Kernel.Array.get_unchecked values ~at:index) target then
      true
    else
      loop (index + 1)
  in
  loop 0

let array_has_exact_members = fun actual expected ->
  let rec members_present index =
    if index = Kernel.Array.length expected then
      true
    else if array_contains actual (Kernel.Array.get_unchecked expected ~at:index) then
      members_present (index + 1)
    else
      false
  in
  Kernel.Array.length actual = Kernel.Array.length expected && members_present 0

let test_file_scalar_write_roundtrips = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "scalar.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.from_string "hello kernel-new" in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected full scalar write")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.create ~size:(Kernel.Bytes.length payload) in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf ~offset:0 ~len:read))
      in
      if Kernel.String.equal actual "hello kernel-new" then
        Ok ()
      else
        Error "expected scalar file roundtrip to preserve payload")

let test_file_vectored_write_roundtrips = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "vectored.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload =
        Kernel.IO.IoVec.from_string_array [|"hello"; " "; "vectored"; " "; "world"|]
        |> Result.unwrap
      in
      let* () =
        with_file
          file
          (fun () ->
            let expected = Kernel.IO.IoVec.length payload in
            let* written = lift (Kernel.Fs.File.write_vectored file payload) in
            if written = expected then
              Ok ()
            else
              Error "expected full vectored write")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.create ~size:64 in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf ~offset:0 ~len:read))
      in
      if Kernel.String.equal actual "hello vectored world" then
        Ok ()
      else
        Error "expected vectored file roundtrip to preserve payload")

let test_file_read_and_write_respect_pos_and_len = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "slice.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.from_string "__payload__" in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file ~pos:2 ~len:7 payload) in
            if written = 7 then
              Ok ()
            else
              Error "expected partial file write to write the requested slice")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buf = Kernel.Bytes.from_string "xxx_______yyy" in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file ~pos:3 ~len:7 buf) in
            Ok (read, Kernel.Bytes.to_string buf))
      in
      let (read, contents) = actual in
      if read = 7 && Kernel.String.equal contents "xxxpayloadyyy" then
        Ok ()
      else
        Error "expected partial file read to fill only the requested slice")

let test_create_dir_and_read_dir_names = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let child_dir = Kernel.Path.(tempdir / "child") in
      let child_file = Kernel.Path.(tempdir / "alpha.txt") in
      let* () = lift (Kernel.Fs.File.create_dir child_dir ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "a")) in
            Ok ())
      in
      let* names = lift (Kernel.Fs.File.read_dir_names tempdir) in
      let has_child =
        Kernel.Array.fold_left
          names
          ~acc:false
          ~fn:(fun found name -> found || Kernel.String.equal name "child")
      in
      let has_file =
        Kernel.Array.fold_left
          names
          ~acc:false
          ~fn:(fun found name -> found || Kernel.String.equal name "alpha.txt")
      in
      let* metadata = lift (Kernel.Fs.File.metadata child_dir) in
      if has_child && has_file && Kernel.Fs.File.Metadata.is_dir metadata then
        Ok ()
      else
        Error "expected directory creation and read_dir_names to expose entries")

let test_symlink_metadata_and_canonicalize = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "latest") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
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
          let link_target_matches =
            Kernel.String.equal (Kernel.Path.to_string link_target) (Kernel.Path.to_string target)
          in
          let canonical_matches =
            Kernel.String.equal
              (Kernel.Path.to_string canonical)
              (Kernel.Path.to_string canonical_target)
          in
          let followed_is_file = Kernel.Fs.File.Metadata.is_file followed in
          let raw_is_symlink = Kernel.Fs.File.Metadata.is_symlink raw in
          if link_target_matches && followed_is_file && raw_is_symlink && canonical_matches then
            Ok ()
          else
            Error "expected symlink metadata and canonicalize to distinguish link from target"))

let test_dangling_symlink_still_has_symlink_metadata = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "dangling") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
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

let test_metadata_reports_missing_target_for_dangling_symlink = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "dangling") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.remove_file target) in
          let* raw = lift (Kernel.Fs.File.symlink_metadata link) in
          match Kernel.Fs.File.metadata link with
          | Kernel.Result.Error (
            Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
          ) when Kernel.Fs.File.Metadata.is_symlink raw ->
              Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ ->
              Error "expected metadata to fail on a dangling symlink while symlink_metadata still succeeds"))

let test_lstat_matches_symlink_metadata = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
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
            && Kernel.Fs.File.Metadata.ino by_lstat
            = Kernel.Fs.File.Metadata.ino by_symlink_metadata
          then
            Ok ()
          else
            Error "expected lstat to match symlink_metadata for a symbolic link"))

let test_metadata_follows_symlink_but_remove_only_unlinks_symlink = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let copied = Kernel.Path.(tempdir / "copied.txt") in
      let renamed = Kernel.Path.(tempdir / "renamed.txt") in
      let payload = Kernel.Bytes.from_string "copy me" in
      let* file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file payload) in
            Ok ())
      in
      let* () = lift (Kernel.Fs.File.copy ~src:source ~dst:copied) in
      let* () = lift (Kernel.Fs.File.rename ~src:copied ~dst:renamed) in
      let* file = lift (Kernel.Fs.File.open_read renamed) in
      let buf = Kernel.Bytes.create ~size:16 in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buf) in
            Ok (Kernel.Bytes.sub_string buf ~offset:0 ~len:read))
      in
      let* exists = lift (Kernel.Fs.File.exists renamed) in
      if exists && Kernel.String.equal actual "copy me" then
        Ok ()
      else
        Error "expected copy and rename to preserve payload")

let test_clone_copies_payload_and_overwrites_destination = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let destination = Kernel.Path.(tempdir / "destination.txt") in
      let* source_file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          source_file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write source_file (Kernel.Bytes.from_string "new")) in
            if written = 3 then
              Ok ()
            else
              Error "expected source fixture write to make progress")
      in
      let* destination_file = lift (Kernel.Fs.File.open_write destination) in
      let* () =
        with_file
          destination_file
          (fun () ->
            let* written =
              lift (Kernel.Fs.File.write destination_file (Kernel.Bytes.from_string "old-old"))
            in
            if written = 7 then
              Ok ()
            else
              Error "expected destination fixture write to make progress")
      in
      let* () = lift (Kernel.Fs.File.clone ~src:source ~dst:destination) in
      let* destination_file = lift (Kernel.Fs.File.open_read destination) in
      let buffer = Kernel.Bytes.create ~size:16 in
      let* payload =
        with_file
          destination_file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read destination_file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      if payload = "new" then
        Ok ()
      else
        Error "expected clone to behave like copy for existing destinations")

let test_clone_copies_payload_to_new_destination = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let destination = Kernel.Path.(tempdir / "destination.txt") in
      let* source_file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          source_file
          (fun () ->
            let payload = Kernel.Bytes.from_string "native clone candidate" in
            let* written = lift (Kernel.Fs.File.write source_file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected source fixture write to write the whole payload")
      in
      let* () = lift (Kernel.Fs.File.clone ~src:source ~dst:destination) in
      let* destination_file = lift (Kernel.Fs.File.open_read destination) in
      let buffer = Kernel.Bytes.create ~size:64 in
      let* payload =
        with_file
          destination_file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read destination_file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      if payload = "native clone candidate" then
        Ok ()
      else
        Error "expected clone to copy source contents to a fresh destination")

let test_fstat_matches_path_metadata = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "metadata.bin"
    (fun path ->
      let payload = Kernel.Bytes.from_string "metadata" in
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* (path_metadata, fd_metadata) =
        with_file
          file
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let link = Kernel.Path.(tempdir / "linked.txt") in
      let directory = Kernel.Path.(tempdir / "child") in
      let* file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "linked")) in
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "child") in
      let child_file = Kernel.Path.(directory / "entry.txt") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "x")) in
            Ok ())
      in
      match Kernel.Fs.File.remove_dir directory with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.DirectoryNotEmpty
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () -> Error "expected removing a non-empty directory to fail")

let test_exists_and_is_directory_report_expected_kinds = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "dir") in
      let file_path = Kernel.Path.(tempdir / "entry.txt") in
      let missing = Kernel.Path.(tempdir / "missing.txt") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write file_path) in
      let* () =
        with_file
          file
          (fun () ->
            let* _ = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "x")) in
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
  with_temp_path
    "kernel_new_file"
    "readv.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let payload = Kernel.Bytes.from_string "hello vectored read" in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected scalar write to seed vectored read fixture")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let iov =
        Kernel.IO.IoVec.create ~count:3 ~size:(Kernel.Bytes.length payload) ()
        |> Result.unwrap
      in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read_vectored file iov) in
            Ok (read, Kernel.IO.IoVec.to_string iov))
      in
      let (read, contents) = actual in
      let prefix =
        Kernel.Bytes.sub_string (Kernel.Bytes.from_string contents) ~offset:0 ~len:read
      in
      if
        read = Kernel.Bytes.length payload && Kernel.String.equal prefix "hello vectored read"
      then
        Ok ()
      else
        Error "expected read_vectored to preserve payload across iovec segments")

let test_is_tty_is_false_for_files_and_pipes = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "tty.bin"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let file_is_tty = with_file file (fun () -> Ok (Kernel.Fs.File.is_tty file)) in
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
  with_temp_path
    "kernel_new_file"
    "missing.bin"
    (fun path ->
      match Kernel.Fs.File.open_read path with
      | Kernel.Result.Ok _ -> Error "expected opening a missing file to fail"
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
      ) ->
          Ok ()
      | Kernel.Result.Error error ->
          Error (Kernel.String.append
            "expected no-such-file error, got "
            (Kernel.Fs.File.error_to_string error)))

let test_remove_missing_paths_report_no_such_file = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let missing_file = Kernel.Path.(tempdir / "missing.txt") in
      let missing_dir = Kernel.Path.(tempdir / "missing-dir") in
      match (Kernel.Fs.File.remove_file missing_file, Kernel.Fs.File.remove_dir missing_dir) with
      | (
          Kernel.Result.Error (
            Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
          ),
          Kernel.Result.Error (
            Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
          )
        ) -> Ok ()
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let nested = Kernel.Path.(tempdir / "nested") in
      let file_path = Kernel.Path.(tempdir / "note.txt") in
      let* () = lift (Kernel.Fs.File.create_dir nested ~perm:0o755) in
      let* file = lift (Kernel.Fs.File.open_write file_path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "x")) in
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "link") in
      let nested_link = Kernel.Path.(tempdir / "nested-link") in
      let target_ref = Kernel.Path.from_string "target.txt" in
      let link_ref = Kernel.Path.from_string "link" in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
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
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let original = Kernel.Path.(tempdir / "original.txt") in
      let alias = Kernel.Path.(tempdir / "alias.txt") in
      let moved = Kernel.Path.(tempdir / "moved.txt") in
      let* file = lift (Kernel.Fs.File.open_write original) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "linked")) in
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
  with_temp_path
    "kernel_new_file"
    "vectored-subslice.bin"
    (fun path ->
      let payload =
        Kernel.IO.IoVec.from_string_array [|"__"; "hello"; " "; "kernel"; "__"|]
        |> Result.unwrap
      in
      let slice =
        Kernel.IO.IoVec.sub ~pos:2 ~len:12 payload
        |> Result.unwrap
      in
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write_vectored file slice) in
            if written = 12 then
              Ok ()
            else
              Error "expected vectored subslice write to write the selected slice only")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.create ~size:32 in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      if Kernel.String.equal actual "hello kernel" then
        Ok ()
      else
        Error "expected vectored subslice roundtrip to preserve the selected payload")

let test_metadata_and_lstat_are_explicit_for_symlinked_directory = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target_dir = Kernel.Path.(tempdir / "dir-target") in
      let link = Kernel.Path.(tempdir / "dir-link") in
      let* () = lift (Kernel.Fs.File.create_dir target_dir ~perm:0o755) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_dir target_dir in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target_dir ~dst:link) in
          let* followed = lift (Kernel.Fs.File.metadata link) in
          let* raw = lift (Kernel.Fs.File.lstat link) in
          let* by_alias = lift (Kernel.Fs.File.symlink_metadata link) in
          if
            Kernel.Fs.File.Metadata.is_dir followed
            && Kernel.Fs.File.Metadata.is_symlink raw
            && Kernel.Fs.File.Metadata.is_symlink by_alias
          then
            Ok ()
          else
            Error "expected metadata to follow symlink targets while lstat inspects the link itself"))

let test_read_dir_names_returns_fresh_snapshots = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let alpha = Kernel.Path.(tempdir / "alpha.txt") in
      let beta = Kernel.Path.(tempdir / "beta.txt") in
      let gamma = Kernel.Path.(tempdir / "gamma.txt") in
      let* alpha_file = lift (Kernel.Fs.File.open_write alpha) in
      let* () =
        with_file
          alpha_file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write alpha_file (Kernel.Bytes.from_string "a")) in
            if written = 1 then
              Ok ()
            else
              Error "expected alpha fixture write to make progress")
      in
      let* beta_file = lift (Kernel.Fs.File.open_write beta) in
      let* () =
        with_file
          beta_file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write beta_file (Kernel.Bytes.from_string "b")) in
            if written = 1 then
              Ok ()
            else
              Error "expected beta fixture write to make progress")
      in
      let* first = lift (Kernel.Fs.File.read_dir_names tempdir) in
      let* () = lift (Kernel.Fs.File.rename ~src:alpha ~dst:gamma) in
      let* () = lift (Kernel.Fs.File.remove_file beta) in
      let* second = lift (Kernel.Fs.File.read_dir_names tempdir) in
      if
        array_has_exact_members first [|"alpha.txt"; "beta.txt"|]
        && array_has_exact_members second [|"gamma.txt"|]
      then
        Ok ()
      else
        Error "expected read_dir_names to return fresh snapshots across repeated calls")

let test_renamed_target_turns_symlink_into_dangling_path = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let moved = Kernel.Path.(tempdir / "moved.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
            if written = 6 then
              Ok ()
            else
              Error "expected symlink target fixture write to make progress")
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_file moved in
          let _ = Kernel.Fs.File.remove_file target in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
          let* () = lift (Kernel.Fs.File.rename ~src:target ~dst:moved) in
          let* raw = lift (Kernel.Fs.File.symlink_metadata link) in
          match Kernel.Fs.File.metadata link with
          | Kernel.Result.Error (
            Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
          ) when Kernel.Fs.File.Metadata.is_symlink raw ->
              Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ ->
              Error "expected moved symlink target to leave a dangling link behind"))

let test_renaming_broken_symlink_preserves_the_link_itself = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target.txt") in
      let link = Kernel.Path.(tempdir / "alias.txt") in
      let moved_link = Kernel.Path.(tempdir / "renamed-alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write target) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "kernel")) in
            if written = 6 then
              Ok ()
            else
              Error "expected broken symlink fixture write to make progress")
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file moved_link in
          let _ = Kernel.Fs.File.remove_file link in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
          let* () = lift (Kernel.Fs.File.remove_file target) in
          let* () = lift (Kernel.Fs.File.rename ~src:link ~dst:moved_link) in
          let* raw = lift (Kernel.Fs.File.symlink_metadata moved_link) in
          let* link_target = lift (Kernel.Fs.File.read_link moved_link) in
          if
            Kernel.Fs.File.Metadata.is_symlink raw
            && Kernel.String.equal
              (Kernel.Path.to_string link_target)
              (Kernel.Path.to_string target)
          then
            Ok ()
          else
            Error "expected renaming a broken symlink to preserve the link itself and its target text"))

let test_hard_link_remove_original_preserves_alias_and_decrements_link_count = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let original = Kernel.Path.(tempdir / "original.txt") in
      let alias = Kernel.Path.(tempdir / "alias.txt") in
      let* file = lift (Kernel.Fs.File.open_write original) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "linked")) in
            if written = 6 then
              Ok ()
            else
              Error "expected hard-link alias fixture write to make progress")
      in
      let* () = lift (Kernel.Fs.File.hard_link ~src:original ~dst:alias) in
      let* () = lift (Kernel.Fs.File.remove_file original) in
      let* alias_metadata = lift (Kernel.Fs.File.metadata alias) in
      let* alias_file = lift (Kernel.Fs.File.open_read alias) in
      let buffer = Kernel.Bytes.create ~size:16 in
      let* payload =
        with_file
          alias_file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read alias_file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      let* original_exists = lift (Kernel.Fs.File.exists original) in
      let* alias_exists = lift (Kernel.Fs.File.exists alias) in
      if
        not original_exists
        && alias_exists
        && Kernel.Fs.File.Metadata.nlink alias_metadata = 1
        && Kernel.String.equal payload "linked"
      then
        Ok ()
      else
        Error "expected removing the original hard-link path to leave one remaining alias")

let test_scalar_partial_io_slice_matrix_roundtrips = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let payload = Kernel.Bytes.from_string "0123456789abcdef" in
      let cases = [ (0, 1); (0, 4); (2, 5); (4, 8); (12, 4); ] in
      let rec loop index cases =
        match cases with
        | [] -> Ok ()
        | (pos, len) :: rest ->
            let path =
              Kernel.Path.(tempdir
              / String.concat "" [ "scalar-slice-"; Int.to_string index; ".bin" ])
            in
            let* file = lift (Kernel.Fs.File.open_write path) in
            let* () =
              with_file
                file
                (fun () ->
                  let* written = lift (Kernel.Fs.File.write file ~pos ~len payload) in
                  if written = len then
                    Ok ()
                  else
                    Error "expected scalar slice write matrix to write the requested length")
            in
            let* file = lift (Kernel.Fs.File.open_read path) in
            let buffer = Kernel.Bytes.create ~size:32 in
            let* actual =
              with_file
                file
                (fun () ->
                  let* read = lift (Kernel.Fs.File.read file buffer) in
                  Ok (read, Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
            in
            let (read, contents) = actual in
            let expected = Kernel.Bytes.sub_string payload ~offset:pos ~len in
            if read != len || not (Kernel.String.equal contents expected) then
              Error "expected scalar partial file io matrix to preserve each selected slice"
            else
              loop (index + 1) rest
      in
      loop 0 cases)

let test_vectored_partial_io_slice_matrix_roundtrips = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let payload =
        Kernel.IO.IoVec.from_string_array [|"__"; "alpha"; "-"; "beta"; "__"|]
        |> Result.unwrap
      in
      let flattened = Kernel.IO.IoVec.to_string payload in
      let cases = [ (2, 5); (2, 10); (4, 4); (7, 4); (2, 12); ] in
      let rec loop index cases =
        match cases with
        | [] -> Ok ()
        | (pos, len) :: rest ->
            let path =
              Kernel.Path.(tempdir
              / String.concat "" [ "vectored-slice-"; Int.to_string index; ".bin" ])
            in
            let slice =
              Kernel.IO.IoVec.sub ~pos ~len payload
              |> Result.unwrap
            in
            let* file = lift (Kernel.Fs.File.open_write path) in
            let* () =
              with_file
                file
                (fun () ->
                  let* written = lift (Kernel.Fs.File.write_vectored file slice) in
                  if written = len then
                    Ok ()
                  else
                    Error "expected vectored slice write matrix to write the requested length")
            in
            let* file = lift (Kernel.Fs.File.open_read path) in
            let buffer = Kernel.Bytes.create ~size:32 in
            let* actual =
              with_file
                file
                (fun () ->
                  let* read = lift (Kernel.Fs.File.read file buffer) in
                  Ok (read, Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
            in
            let (read, contents) = actual in
            let expected = String.sub flattened ~offset:pos ~len in
            if read != len || not (Kernel.String.equal contents expected) then
              Error "expected vectored partial file io matrix to preserve each selected slice"
            else
              loop (index + 1) rest
      in
      loop 0 cases)

let test_canonicalize_rejects_symlink_loops = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let left = Kernel.Path.(tempdir / "left") in
      let right = Kernel.Path.(tempdir / "right") in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file left in
          let _ = Kernel.Fs.File.remove_file right in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.symlink ~src:right ~dst:left) in
          let* () = lift (Kernel.Fs.File.symlink ~src:left ~dst:right) in
          match Kernel.Fs.File.canonicalize left with
          | Kernel.Result.Error _ -> Ok ()
          | Kernel.Result.Ok _ -> Error "expected canonicalize to reject a symlink loop"))

let test_read_dir_names_handles_larger_snapshots_with_renames_and_removes = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let name_for prefix index = String.concat "" [ prefix; "-"; Int.to_string index; ".txt"; ] in
      let path_for prefix index = Kernel.Path.(tempdir / name_for prefix index) in
      let rec create_many index =
        if index = 32 then
          Ok ()
        else
          let path = path_for "entry" index in
          let* file = lift (Kernel.Fs.File.open_write path) in
          let* () =
            with_file
              file
              (fun () ->
                let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "x")) in
                if written = 1 then
                  Ok ()
                else
                  Error "expected larger snapshot fixture write to make progress")
          in
          create_many (index + 1)
      in
      let rec rename_prefix index =
        if index = 8 then
          Ok ()
        else
          let* () =
            lift
              (Kernel.Fs.File.rename ~src:(path_for "entry" index) ~dst:(path_for "renamed" index))
          in
          rename_prefix (index + 1)
      in
      let rec remove_suffix index =
        if index = 8 then
          Ok ()
        else
          let* () = lift (Kernel.Fs.File.remove_file (path_for "entry" (index + 8))) in
          remove_suffix (index + 1)
      in
      let* () = create_many 0 in
      let* first = lift (Kernel.Fs.File.read_dir_names tempdir) in
      let* () = rename_prefix 0 in
      let* () = remove_suffix 0 in
      let* second = lift (Kernel.Fs.File.read_dir_names tempdir) in
      if
        Kernel.Array.length first = 32
        && Kernel.Array.length second = 24
        && array_contains first "entry-0.txt"
        && array_contains first "entry-31.txt"
        && array_contains second "renamed-0.txt"
        && array_contains second "entry-31.txt"
        && not (array_contains second "entry-0.txt")
        && not (array_contains second "entry-8.txt")
      then
        Ok ()
      else
        Error "expected larger read_dir_names snapshots to reflect renames and removals across calls")

let test_close_twice_reports_bad_file_descriptor = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "close-twice.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* () = lift (Kernel.Fs.File.close file) in
      match Kernel.Fs.File.close file with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.BadFileDescriptor
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () ->
          Error "expected closing the same file twice to report bad_file_descriptor")

let test_open_write_without_create_rejects_missing_paths = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let missing = Kernel.Path.(tempdir / "missing.txt") in
      match Kernel.Fs.File.open_write ~create:false missing with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok file ->
          let _ = Kernel.Fs.File.close file in
          Error "expected open_write ~create:false to reject a missing path")

let test_open_write_append_preserves_existing_bytes = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "append.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write ~create:true ~truncate:true path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "old")) in
            if written = 3 then
              Ok ()
            else
              Error "expected append fixture write to make progress")
      in
      let* file = lift (Kernel.Fs.File.open_write ~create:false ~truncate:false ~append:true path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "new")) in
            if written = 3 then
              Ok ()
            else
              Error "expected append write to make progress")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.create ~size:16 in
      let* payload =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      if payload = "oldnew" then
        Ok ()
      else
        Error "expected append mode to preserve the existing bytes and extend the file")

let test_read_len_zero_is_a_no_op = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "read-zero.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")) in
            if written = 4 then
              Ok ()
            else
              Error "expected zero-read fixture write to make progress")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.from_string "unchanged" in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read file ~pos:2 ~len:0 buffer) in
            Ok (read, Kernel.Bytes.to_string buffer))
      in
      match actual with
      | (0, "unchanged") -> Ok ()
      | _ -> Error "expected File.read ~len:0 to leave the buffer unchanged and return zero")

let test_write_len_zero_is_a_no_op = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "write-zero.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* written =
        with_file
          file
          (fun () ->
            lift
              (Kernel.Fs.File.write file ~pos:1 ~len:0 (Kernel.Bytes.from_string "riot")))
      in
      if written != 0 then
        Error "expected File.write ~len:0 to report zero bytes written"
      else
        let* file = lift (Kernel.Fs.File.open_read path) in
        let buffer = Kernel.Bytes.create ~size:8 in
        let* read = with_file file (fun () -> lift (Kernel.Fs.File.read file buffer)) in
        if read = 0 then
          Ok ()
        else
          Error "expected File.write ~len:0 to leave file contents unchanged")

let test_read_rejects_negative_pos = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "read-negative-pos.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* _ =
        with_file
          file
          (fun () -> lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")))
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.create ~size:4 in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close file in
          ())
        (fun () ->
          match Kernel.Fs.File.read file ~pos:(-1) buffer with
          | Kernel.Result.Error (Kernel.Fs.File.InvalidSlice { pos = (-1); _ }) -> Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ -> Error "expected File.read to reject a negative buffer offset"))

let test_write_rejects_negative_len = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "write-negative-len.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close file in
          ())
        (fun () ->
          match Kernel.Fs.File.write file ~len:(-1) (Kernel.Bytes.from_string "riot") with
          | Kernel.Result.Error (Kernel.Fs.File.InvalidSlice { len = (-1); _ }) -> Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ -> Error "expected File.write to reject a negative slice length"))

let test_read_rejects_slices_past_the_buffer_end = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "read-overflow.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* _ =
        with_file
          file
          (fun () -> lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")))
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let buffer = Kernel.Bytes.create ~size:4 in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close file in
          ())
        (fun () ->
          match Kernel.Fs.File.read file ~pos:2 ~len:3 buffer with
          | Kernel.Result.Error (
            Kernel.Fs.File.InvalidSlice { pos = 2; len = 3; buffer_len = 4 }
          ) ->
              Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ ->
              Error "expected File.read to reject slices that extend past the buffer end"))

let test_write_rejects_slices_past_the_buffer_end = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "write-overflow.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.close file in
          ())
        (fun () ->
          match Kernel.Fs.File.write file ~pos:2 ~len:3 (Kernel.Bytes.create ~size:4) with
          | Kernel.Result.Error (
            Kernel.Fs.File.InvalidSlice { pos = 2; len = 3; buffer_len = 4 }
          ) ->
              Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
          | Kernel.Result.Ok _ ->
              Error "expected File.write to reject slices that extend past the buffer end"))

let test_read_vectored_ignores_zero_length_segments = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "read-vectored-zero-segments.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* () =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")) in
            if written = 4 then
              Ok ()
            else
              Error "expected vectored zero-segment fixture write to make progress")
      in
      let* file = lift (Kernel.Fs.File.open_read path) in
      let iov =
        Kernel.IO.IoVec.from_bytes_array
          [|
            Kernel.Bytes.create ~size:0;
            Kernel.Bytes.create ~size:2;
            Kernel.Bytes.create ~size:0;
            Kernel.Bytes.create ~size:2;
          |]
        |> Result.unwrap
      in
      let* actual =
        with_file
          file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read_vectored file iov) in
            Ok (read, Kernel.IO.IoVec.to_string iov))
      in
      match actual with
      | (4, "riot") -> Ok ()
      | _ -> Error "expected read_vectored to ignore zero-length segments and preserve the payload")

let test_write_vectored_zero_total_length_is_a_no_op = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "write-vectored-zero.txt"
    (fun path ->
      let iov =
        Kernel.IO.IoVec.from_bytes_array
          [|Kernel.Bytes.create ~size:0; Kernel.Bytes.create ~size:0|]
        |> Result.unwrap
      in
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* written = with_file file (fun () -> lift (Kernel.Fs.File.write_vectored file iov)) in
      if written != 0 then
        Error "expected write_vectored with zero total length to report zero bytes written"
      else
        let* file = lift (Kernel.Fs.File.open_read path) in
        let buffer = Kernel.Bytes.create ~size:8 in
        let* read = with_file file (fun () -> lift (Kernel.Fs.File.read file buffer)) in
        if read = 0 then
          Ok ()
        else
          Error "expected zero-length write_vectored to leave the file empty")

let test_create_dir_on_existing_directory_reports_already_exists = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "existing") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      match Kernel.Fs.File.create_dir directory ~perm:0o755 with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.AlreadyExists
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () -> Error "expected create_dir on an existing directory to fail")

let test_remove_dir_on_regular_file_reports_not_directory = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "remove-dir-regular.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* _ =
        with_file
          file
          (fun () -> lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")))
      in
      match Kernel.Fs.File.remove_dir path with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.NotDirectory
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () -> Error "expected remove_dir on a regular file to report not_directory")

let test_remove_file_on_directory_reports_a_directory_error = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let directory = Kernel.Path.(tempdir / "dir") in
      let* () = lift (Kernel.Fs.File.create_dir directory ~perm:0o755) in
      match Kernel.Fs.File.remove_file directory with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.IsDirectory
      ) ->
          Ok ()
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.PermissionDenied
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok () ->
          Error "expected remove_file on a directory to fail with a directory-related error")

let test_read_link_on_non_symlink_reports_invalid_argument = fun _ctx ->
  with_temp_path
    "kernel_new_file"
    "not-a-link.txt"
    (fun path ->
      let* file = lift (Kernel.Fs.File.open_write path) in
      let* _ =
        with_file
          file
          (fun () -> lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "riot")))
      in
      match Kernel.Fs.File.read_link path with
      | Kernel.Result.Error (
        Kernel.Fs.File.System Kernel.SystemError.InvalidArgument
      ) ->
          Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
      | Kernel.Result.Ok _ -> Error "expected read_link on a non-symlink path to fail cleanly")

let test_is_directory_reports_false_for_dangling_symlinks = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let target = Kernel.Path.(tempdir / "target-dir") in
      let link = Kernel.Path.(tempdir / "dangling-dir-link") in
      let* () = lift (Kernel.Fs.File.create_dir target ~perm:0o755) in
      let* () = lift (Kernel.Fs.File.symlink ~src:target ~dst:link) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file link in
          let _ = Kernel.Fs.File.remove_dir target in
          ())
        (fun () ->
          let* () = lift (Kernel.Fs.File.remove_dir target) in
          let* is_directory = lift (Kernel.Fs.File.is_directory link) in
          if not is_directory then
            Ok ()
          else
            Error "expected is_directory on a dangling symlink to report false"))

let test_copy_overwrites_existing_destination_bytes = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source.txt") in
      let destination = Kernel.Path.(tempdir / "destination.txt") in
      let* source_file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          source_file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write source_file (Kernel.Bytes.from_string "new")) in
            if written = 3 then
              Ok ()
            else
              Error "expected source fixture write to make progress")
      in
      let* destination_file = lift (Kernel.Fs.File.open_write destination) in
      let* () =
        with_file
          destination_file
          (fun () ->
            let* written =
              lift (Kernel.Fs.File.write destination_file (Kernel.Bytes.from_string "old-old"))
            in
            if written = 7 then
              Ok ()
            else
              Error "expected destination fixture write to make progress")
      in
      let* () = lift (Kernel.Fs.File.copy ~src:source ~dst:destination) in
      let* destination_file = lift (Kernel.Fs.File.open_read destination) in
      let buffer = Kernel.Bytes.create ~size:16 in
      let* payload =
        with_file
          destination_file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read destination_file buffer) in
            Ok (Kernel.Bytes.sub_string buffer ~offset:0 ~len:read))
      in
      if payload = "new" then
        Ok ()
      else
        Error "expected copy to overwrite the destination contents instead of appending")

let test_copy_preserves_large_payloads_beyond_the_internal_chunk_size = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source-large.bin") in
      let destination = Kernel.Path.(tempdir / "destination-large.bin") in
      let payload = Kernel.Bytes.create ~size:80_000 in
      let rec fill index =
        if index = Kernel.Bytes.length payload then
          ()
        else (
          Kernel.Bytes.set_unchecked
            payload
            ~at:index
            ~char:(Kernel.Char.from_int_unchecked (65 + (index mod 26)));
          fill (index + 1)
        )
      in
      fill 0;
      let* source_file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          source_file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write source_file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected large copy fixture write to write the whole payload")
      in
      let* () = lift (Kernel.Fs.File.copy ~src:source ~dst:destination) in
      let* destination_file = lift (Kernel.Fs.File.open_read destination) in
      let buffer = Kernel.Bytes.create ~size:(Kernel.Bytes.length payload) in
      let* actual =
        with_file
          destination_file
          (fun () ->
            let* read = lift (Kernel.Fs.File.read destination_file buffer) in
            Ok (read, Kernel.Bytes.to_string buffer))
      in
      match actual with
      | (read, contents) when read = Kernel.Bytes.length payload
      && contents = Kernel.Bytes.to_string payload -> Ok ()
      | _ -> Error "expected copy to preserve payloads larger than the internal copy chunk size")

let test_copy_preserves_source_permissions = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let source = Kernel.Path.(tempdir / "source-mode.sh") in
      let destination = Kernel.Path.(tempdir / "destination-mode.sh") in
      let* source_file = lift (Kernel.Fs.File.open_write source) in
      let* () =
        with_file
          source_file
          (fun () ->
            let payload = Kernel.Bytes.from_string "#!/bin/sh\nexit 0\n" in
            let* written = lift (Kernel.Fs.File.write source_file payload) in
            if written = Kernel.Bytes.length payload then
              Ok ()
            else
              Error "expected copy-permissions fixture write to write the whole payload")
      in
      let* () = lift (Kernel.Fs.File.set_permissions source ~perm:0o755) in
      let* () = lift (Kernel.Fs.File.copy ~src:source ~dst:destination) in
      let* source_metadata = lift (Kernel.Fs.File.metadata source) in
      let* destination_metadata = lift (Kernel.Fs.File.metadata destination) in
      if
        Kernel.Fs.File.Metadata.permissions source_metadata
        = Kernel.Fs.File.Metadata.permissions destination_metadata
      then
        Ok ()
      else
        Error "expected copy to preserve source permissions")

let test_fstat_continues_to_describe_the_open_file_after_rename = fun _ctx ->
  with_tempdir
    "kernel_new_file"
    (fun tempdir ->
      let original = Kernel.Path.(tempdir / "original.txt") in
      let moved = Kernel.Path.(tempdir / "moved.txt") in
      let* file = lift (Kernel.Fs.File.open_write original) in
      let* actual =
        with_file
          file
          (fun () ->
            let* written = lift (Kernel.Fs.File.write file (Kernel.Bytes.from_string "rename")) in
            if written != 6 then
              Error "expected rename fixture write to make progress"
            else
              let* before = lift (Kernel.Fs.File.fstat file) in
              let* () = lift (Kernel.Fs.File.rename ~src:original ~dst:moved) in
              let* after = lift (Kernel.Fs.File.fstat file) in
              let* by_path = lift (Kernel.Fs.File.metadata moved) in
              Ok (before, after, by_path))
      in
      match actual with
      | (before, after, by_path) ->
          if
            Kernel.Fs.File.Metadata.dev before = Kernel.Fs.File.Metadata.dev after
            && Kernel.Fs.File.Metadata.dev after = Kernel.Fs.File.Metadata.dev by_path
            && Kernel.Fs.File.Metadata.ino before = Kernel.Fs.File.Metadata.ino after
            && Kernel.Fs.File.Metadata.ino after = Kernel.Fs.File.Metadata.ino by_path
          then
            Ok ()
          else
            Error "expected fstat to keep describing the same open file across a path rename")

let tests = [
  Test.case "Fs.File scalar write roundtrips" test_file_scalar_write_roundtrips;
  Test.case "Fs.File vectored write roundtrips" test_file_vectored_write_roundtrips;
  Test.case
    "Fs.File read and write respect pos and len"
    test_file_read_and_write_respect_pos_and_len;
  Test.case "Fs.File create_dir and read_dir_names" test_create_dir_and_read_dir_names;
  Test.case "Fs.File symlink metadata and canonicalize" test_symlink_metadata_and_canonicalize;
  Test.case
    "Fs.File metadata and lstat stay explicit for symlinked directories"
    test_metadata_and_lstat_are_explicit_for_symlinked_directory;
  Test.case
    "Fs.File dangling symlink still reports symlink metadata"
    test_dangling_symlink_still_has_symlink_metadata;
  Test.case
    "Fs.File metadata reports missing target for dangling symlink"
    test_metadata_reports_missing_target_for_dangling_symlink;
  Test.case "Fs.File lstat matches symlink_metadata" test_lstat_matches_symlink_metadata;
  Test.case
    "Fs.File metadata follows symlink but remove_file only unlinks the symlink"
    test_metadata_follows_symlink_but_remove_only_unlinks_symlink;
  Test.case
    "Fs.File renamed symlink targets leave dangling links behind"
    test_renamed_target_turns_symlink_into_dangling_path;
  Test.case
    "Fs.File renaming broken symlinks preserves the link itself"
    test_renaming_broken_symlink_preserves_the_link_itself;
  Test.case "Fs.File copy and rename roundtrips" test_copy_and_rename_roundtrip;
  Test.case
    "Fs.File clone copies payloads and overwrites destinations"
    test_clone_copies_payload_and_overwrites_destination;
  Test.case
    "Fs.File clone copies payloads to fresh destinations"
    test_clone_copies_payload_to_new_destination;
  Test.case "Fs.File fstat matches path metadata" test_fstat_matches_path_metadata;
  Test.case
    "Fs.File hard_link and remove ops update filesystem state"
    test_hard_link_updates_link_count_and_remove_ops;
  Test.case
    "Fs.File removing originals preserves hard-link aliases and decrements link count"
    test_hard_link_remove_original_preserves_alias_and_decrements_link_count;
  Test.case
    "Fs.File remove non-empty dir reports an error"
    test_remove_nonempty_dir_reports_resource_busy;
  Test.case
    "Fs.File exists and is_directory report expected kinds"
    test_exists_and_is_directory_report_expected_kinds;
  Test.case "Fs.File read_vectored roundtrips" test_read_vectored_roundtrips;
  Test.case "Fs.File is_tty is false for files and pipes" test_is_tty_is_false_for_files_and_pipes;
  Test.case "Fs.File missing read maps kernel error" test_open_read_missing_file_maps_error;
  Test.case
    "Fs.File remove missing paths reports no-such-file"
    test_remove_missing_paths_report_no_such_file;
  Test.case
    "Fs.File read_dir_names skips dot entries and is order agnostic"
    test_read_dir_names_skips_dot_entries_and_is_order_agnostic;
  Test.case
    "Fs.File read_dir_names returns fresh snapshots across repeated calls"
    test_read_dir_names_returns_fresh_snapshots;
  Test.case
    "Fs.File nested symlink chains canonicalize cleanly"
    test_nested_symlink_chain_canonicalizes_cleanly;
  Test.case
    "Fs.File hard-link rename preserves remaining link counts"
    test_hard_link_rename_preserves_remaining_link_count;
  Test.case "Fs.File vectored write subslices roundtrip" test_vectored_write_subslice_roundtrips;
  Test.case
    "Fs.File scalar partial io slice matrix roundtrips"
    test_scalar_partial_io_slice_matrix_roundtrips;
  Test.case
    "Fs.File vectored partial io slice matrix roundtrips"
    test_vectored_partial_io_slice_matrix_roundtrips;
  Test.case "Fs.File canonicalize rejects symlink loops" test_canonicalize_rejects_symlink_loops;
  Test.case
    "Fs.File read_dir_names handles larger snapshots with renames and removes"
    test_read_dir_names_handles_larger_snapshots_with_renames_and_removes;
  Test.case
    "Fs.File close on the same handle twice reports bad_file_descriptor"
    test_close_twice_reports_bad_file_descriptor;
  Test.case
    "Fs.File open_write without create rejects missing paths"
    test_open_write_without_create_rejects_missing_paths;
  Test.case
    "Fs.File append mode preserves existing bytes"
    test_open_write_append_preserves_existing_bytes;
  Test.case "Fs.File read len=0 is a no-op" test_read_len_zero_is_a_no_op;
  Test.case "Fs.File write len=0 is a no-op" test_write_len_zero_is_a_no_op;
  Test.case "Fs.File read rejects a negative pos" test_read_rejects_negative_pos;
  Test.case "Fs.File write rejects a negative len" test_write_rejects_negative_len;
  Test.case
    "Fs.File read rejects slices past the buffer end"
    test_read_rejects_slices_past_the_buffer_end;
  Test.case
    "Fs.File write rejects slices past the buffer end"
    test_write_rejects_slices_past_the_buffer_end;
  Test.case
    "Fs.File read_vectored ignores zero-length segments"
    test_read_vectored_ignores_zero_length_segments;
  Test.case
    "Fs.File write_vectored zero total length is a no-op"
    test_write_vectored_zero_total_length_is_a_no_op;
  Test.case
    "Fs.File create_dir on an existing directory reports already_exists"
    test_create_dir_on_existing_directory_reports_already_exists;
  Test.case
    "Fs.File remove_dir on a regular file reports not_directory"
    test_remove_dir_on_regular_file_reports_not_directory;
  Test.case
    "Fs.File remove_file on a directory reports a directory-related error"
    test_remove_file_on_directory_reports_a_directory_error;
  Test.case
    "Fs.File read_link on a non-symlink reports invalid_argument"
    test_read_link_on_non_symlink_reports_invalid_argument;
  Test.case
    "Fs.File is_directory reports false for dangling symlinks"
    test_is_directory_reports_false_for_dangling_symlinks;
  Test.case
    "Fs.File copy overwrites existing destination bytes"
    test_copy_overwrites_existing_destination_bytes;
  Test.case
    "Fs.File copy preserves payloads larger than the internal chunk size"
    test_copy_preserves_large_payloads_beyond_the_internal_chunk_size;
  Test.case "Fs.File copy preserves source permissions" test_copy_preserves_source_permissions;
  Test.case
    "Fs.File fstat continues to describe the open file after rename"
    test_fstat_continues_to_describe_the_open_file_after_rename;
  Test.case
    "Fs.File repeated pipe open and close stays healthy"
    test_repeated_pipe_open_and_close_stays_healthy;
]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"kernel_new_file_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
