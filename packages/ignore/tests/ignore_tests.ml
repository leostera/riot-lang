open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ Kernel.IO.error_message err)

let write = fun path text -> Fs.write text path |> Result.map_err Kernel.IO.error_message

let collect_paths = fun walker ->
  Ignore.Walker.to_list walker
  |> Result.map (List.map Fs.Walker.FileItem.path_string)
  |> Result.map_err
    (
      function
      | Ignore.Walker.File_system { cause; _ } -> Kernel.IO.error_message cause
      | Ignore.Walker.Invalid_glob { path; line; message; _ } -> Path.to_string path
      ^ ":"
      ^ string_of_int line
      ^ ": "
      ^ message
    )

let contains_path = fun paths suffix ->
  List.exists (fun path -> String.ends_with ~suffix path) paths

let tests = [ Test.case "ignore walker skips hidden directories by default"
    (fun _ctx ->
      with_tempdir "ignore_hidden"
        (fun root ->
          let hidden_dir = Path.(root / Path.v ".hidden") in
          let visible = Path.(root / Path.v "visible.txt") in
          let hidden_file = Path.(hidden_dir / Path.v "secret.txt") in
          let* () = Fs.create_dir_all hidden_dir |> Result.map_err Kernel.IO.error_message in
          let* () = write visible "visible" in
          let* () = write hidden_file "secret" in
          let* walker = Ignore.Walker.create ~roots:[ root ] ()
          |> Result.map_err (fun _ -> "walker create failed") in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "visible.txt");
          Test.assert_false (contains_path paths ".hidden");
          Test.assert_false (contains_path paths "secret.txt");
          Ok ())); Test.case "ignore walker respects root gitignore files"
    (fun _ctx ->
      with_tempdir "ignore_gitignore"
        (fun root ->
          let gitignore = Path.(root / Path.v ".gitignore") in
          let vendor_dir = Path.(root / Path.v "vendor") in
          let vendor_file = Path.(vendor_dir / Path.v "pkg.ml") in
          let src_dir = Path.(root / Path.v "src") in
          let src_file = Path.(src_dir / Path.v "main.ml") in
          let* () = Fs.create_dir_all vendor_dir |> Result.map_err Kernel.IO.error_message in
          let* () = Fs.create_dir_all src_dir |> Result.map_err Kernel.IO.error_message in
          let* () = write gitignore "vendor/\n" in
          let* () = write vendor_file "let vendor = 1\n" in
          let* () = write src_file "let main = 1\n" in
          let* walker = Ignore.Walker.create ~roots:[ root ] ~hidden:false ()
          |> Result.map_err (fun _ -> "walker create failed") in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "src/main.ml");
          Test.assert_false (contains_path paths "vendor");
          Test.assert_false (contains_path paths "vendor/pkg.ml");
          Ok ())); Test.case "custom ignore files override gitignore files"
    (fun _ctx ->
      with_tempdir "ignore_custom"
        (fun root ->
          let gitignore = Path.(root / Path.v ".gitignore") in
          let dockerignore = Path.(root / Path.v ".dockerignore") in
          let file = Path.(root / Path.v "keep.txt") in
          let* () = write gitignore "keep.txt\n" in
          let* () = write dockerignore "!keep.txt\n" in
          let* () = write file "keep\n" in
          let* walker = Ignore.Walker.create
            ~roots:[ root ]
            ~hidden:false
            ~custom_ignore_filenames:[ ".dockerignore" ]
            ()
          |> Result.map_err (fun _ -> "walker create failed") in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "keep.txt");
          Ok ())); Test.case "override globs keep unmatched directories but filter files"
    (fun _ctx ->
      with_tempdir "ignore_overrides"
        (fun root ->
          let dir = Path.(root / Path.v "src") in
          let file_ml = Path.(dir / Path.v "main.ml") in
          let file_txt = Path.(dir / Path.v "notes.txt") in
          let* () = Fs.create_dir_all dir |> Result.map_err Kernel.IO.error_message in
          let* () = write file_ml "let main = 1\n" in
          let* () = write file_txt "notes\n" in
          let* walker = Ignore.Walker.create ~roots:[ root ] ~hidden:false ~overrides:[ "*.ml" ] ()
          |> Result.map_err (fun _ -> "walker create failed") in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "src");
          Test.assert_true (contains_path paths "main.ml");
          Test.assert_false (contains_path paths "notes.txt");
          Ok ())) ]

let main = fun ~args -> Test.Cli.main ~name:"ignore_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
