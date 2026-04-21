open Std
module Test = Std.Test
module Kernel = Kernel

let ( let* ) value fn = Result.and_then value ~fn

let lift result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.from_fs_read_dir error))

let lift_file result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Error.to_string (Kernel.Error.from_fs_file error))

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

let collect_names = fun dir ->
  let rec loop acc =
    let* next = lift (Kernel.Fs.ReadDir.read_name dir) in
    match next with
    | None -> Ok (List.reverse acc)
    | Some name -> loop (name :: acc)
  in
  loop []

let test_open_dir_reads_snapshotted_names = fun _ctx ->
  with_tempdir "kernel_new_read_dir"
    (fun root ->
      let child_dir = Kernel.Path.(root / "nested") in
      let child_file = Kernel.Path.(root / "alpha.txt") in
      let* () = lift_file (Kernel.Fs.File.create_dir child_dir ~perm:0o755) in
      let* file = lift_file (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift_file (Kernel.Fs.File.write file (Kernel.Bytes.from_string "alpha")) in
            Ok ())
      in
      let* dir = lift (Kernel.Fs.ReadDir.open_dir root) in
      let later_file = Kernel.Path.(root / "later.txt") in
      let* later = lift_file (Kernel.Fs.File.open_write later_file) in
      let* () =
        with_file later
          (fun () ->
            let* _ = lift_file (Kernel.Fs.File.write later (Kernel.Bytes.from_string "later")) in
            Ok ())
      in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.ReadDir.close dir in
          ())
        (fun () ->
          let* names = collect_names dir in
          let names = Kernel.Array.from_list names in
          if
            array_contains names "nested"
            && array_contains names "alpha.txt"
            && not (array_contains names "later.txt")
            && not (array_contains names ".")
            && not (array_contains names "..")
          then
            Ok ()
          else
            Error "expected directory iteration to return only the original snapshotted entry names"))

let test_read_entry_reports_entry_kinds = fun _ctx ->
  with_tempdir "kernel_new_read_dir"
    (fun root ->
      let child_dir = Kernel.Path.(root / "nested") in
      let child_file = Kernel.Path.(root / "alpha.txt") in
      let child_link = Kernel.Path.(root / "alpha.link") in
      let* () = lift_file (Kernel.Fs.File.create_dir child_dir ~perm:0o755) in
      let* file = lift_file (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift_file (Kernel.Fs.File.write file (Kernel.Bytes.from_string "alpha")) in
            Ok ())
      in
      let* () = lift_file (Kernel.Fs.File.symlink ~src:child_file ~dst:child_link) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.File.remove_file child_link in
          ())
        (fun () ->
          let* dir = lift (Kernel.Fs.ReadDir.open_dir root) in
          let rec loop saw_dir saw_file saw_link =
            let* next = lift (Kernel.Fs.ReadDir.read_entry dir) in
            match next with
            | None ->
                if saw_dir && saw_file && saw_link then
                  Ok ()
                else
                  Error "expected read_entry to classify file, directory, and symlink entries"
            | Some entry ->
                let saw_dir =
                  saw_dir
                  || (Kernel.String.equal (Kernel.Path.to_string entry.path) "nested"
                  && entry.kind = Kernel.Fs.ReadDir.Directory) in
                let saw_file =
                  saw_file
                  || (Kernel.String.equal (Kernel.Path.to_string entry.path) "alpha.txt"
                  && entry.kind = Kernel.Fs.ReadDir.RegularFile) in
                let saw_link =
                  saw_link
                  || (Kernel.String.equal (Kernel.Path.to_string entry.path) "alpha.link"
                  && entry.kind = Kernel.Fs.ReadDir.SymbolicLink) in
                loop saw_dir saw_file saw_link
          in
          protect
            ~finally:(fun () ->
              let _ = Kernel.Fs.ReadDir.close dir in
              ())
            (fun () -> loop false false false)))

let test_read_entry_surfaces_removed_entries = fun _ctx ->
  with_tempdir "kernel_new_read_dir"
    (fun root ->
      let child_file = Kernel.Path.(root / "alpha.txt") in
      let* file = lift_file (Kernel.Fs.File.open_write child_file) in
      let* () =
        with_file file
          (fun () ->
            let* _ = lift_file (Kernel.Fs.File.write file (Kernel.Bytes.from_string "alpha")) in
            Ok ())
      in
      let* dir = lift (Kernel.Fs.ReadDir.open_dir root) in
      let* () = lift_file (Kernel.Fs.File.remove_file child_file) in
      protect
        ~finally:(fun () ->
          let _ = Kernel.Fs.ReadDir.close dir in
          ())
        (fun () ->
          match Kernel.Fs.ReadDir.read_entry dir with
          | Kernel.Result.Error (Kernel.Fs.ReadDir.File (Kernel.Fs.File.System Kernel.SystemError.NoSuchFileOrDirectory)) -> Ok ()
          | Kernel.Result.Error error -> Error (Kernel.Fs.ReadDir.error_to_string error)
          | Kernel.Result.Ok _ -> Error "expected read_entry to surface a removed snapshotted entry"))

let test_read_name_after_close_is_rejected = fun _ctx ->
  with_tempdir "kernel_new_read_dir"
    (fun root ->
      let* dir = lift (Kernel.Fs.ReadDir.open_dir root) in
      let* () = lift (Kernel.Fs.ReadDir.close dir) in
      match Kernel.Fs.ReadDir.read_name dir with
      | Kernel.Result.Error Kernel.Fs.ReadDir.Closed -> Ok ()
      | Kernel.Result.Error error -> Error (Kernel.Fs.ReadDir.error_to_string error)
      | Kernel.Result.Ok _ -> Error "expected closed iterators to reject later reads")

let tests = [
  Test.case "Fs.ReadDir snapshots names and excludes dot entries" test_open_dir_reads_snapshotted_names;
  Test.case "Fs.ReadDir read_entry reports current entry kinds" test_read_entry_reports_entry_kinds;
  Test.case "Fs.ReadDir read_entry surfaces removed snapshotted entries" test_read_entry_surfaces_removed_entries;
  Test.case "Fs.ReadDir read_name rejects reads after close" test_read_name_after_close_is_rejected;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_read_dir_tests" ~tests ~args ()

let () = Actors.run ~main ~args:Env.args ()
