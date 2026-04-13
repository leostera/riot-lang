open Std
module Test = Std.Test

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let find_index = fun items ~f ->
  let rec loop idx = function
    | [] -> None
    | item :: rest ->
        if f item then
          Some idx
        else
          loop (idx + 1) rest
  in
  loop 0 items

let tests = [ Test.case "fs walker rejects invalid depth ranges"
    (fun _ctx ->
      match Fs.Walker.create ~roots:[ Path.v "." ] ~min_depth:3 ~max_depth:1 () with
      | Error (Fs.Walker.MinDepthCannotBeMoreThanMaxDepth { min_depth; max_depth }) ->
          Test.assert_equal ~expected:3 ~actual:min_depth;
          Test.assert_equal ~expected:1 ~actual:max_depth;
          Ok ()
      | Ok _ -> Error "expected invalid depth range"); Test.case "fs walker can prune subtrees"
    (fun _ctx ->
      with_tempdir "std_fs_walker"
        (fun root ->
          let keep_dir = Path.(root / Path.v "keep") in
          let skip_dir = Path.(root / Path.v "skip") in
          Fs.create_dir_all keep_dir |> Result.expect ~msg:"mkdir keep";
          Fs.create_dir_all skip_dir |> Result.expect ~msg:"mkdir skip";
          Fs.write "let keep = ()\n" Path.(keep_dir / Path.v "a.ml") |> Result.expect ~msg:"write keep";
          Fs.write "let skip = ()\n" Path.(skip_dir / Path.v "b.ml") |> Result.expect ~msg:"write skip";
          let seen = ref [] in
          Fs.Walker.walk ~roots:[ root ] ~sort:true
            ~f:(fun entry ->
              let path = Fs.Walker.FileItem.path entry in
              seen := Path.to_string path :: !seen;
              if String.equal (Path.basename path) "skip" then
                Fs.Walker.Skip_subtree
              else
                Fs.Walker.Continue)
            () |> Result.expect ~msg:"walk";
          let seen = List.reverse !seen in
          Test.assert_true
            (List.any
              seen
              ~fn:(String.equal (Path.to_string Path.(keep_dir / Path.v "a.ml"))));
          Test.assert_false
            (List.any
              seen
              ~fn:(String.equal (Path.to_string Path.(skip_dir / Path.v "b.ml"))));
          Ok ())); Test.case "fs walker filter_entry skips directories lazily"
    (fun _ctx ->
      with_tempdir "std_fs_walker_filter"
        (fun root ->
          let keep_dir = Path.(root / Path.v "keep") in
          let skip_dir = Path.(root / Path.v "skip") in
          Fs.create_dir_all keep_dir |> Result.expect ~msg:"mkdir keep";
          Fs.create_dir_all skip_dir |> Result.expect ~msg:"mkdir skip";
          Fs.write "let keep = ()\n" Path.(keep_dir / Path.v "a.ml") |> Result.expect ~msg:"write keep";
          Fs.write "let skip = ()\n" Path.(skip_dir / Path.v "b.ml") |> Result.expect ~msg:"write skip";
          let iter =
            Fs.Walker.create ~roots:[ root ] ~sort:true ()
            |> Result.expect ~msg:"create walker"
            |> Fs.Walker.filter_entry
              ~f:(fun entry ->
                let path = Fs.Walker.FileItem.path entry in
                not (String.equal (Path.basename path) "skip"))
            |> Fs.Walker.into_iter
          in
          let rec collect iter acc =
            match Iter.Iterator.next iter with
            | None, _ -> List.reverse acc
            | Some (Ok (entry: Fs.Walker.FileItem.t)), iter' -> collect
              iter'
              (Fs.Walker.FileItem.path_string entry :: acc)
            | Some (Error _err), iter' -> collect iter' acc
          in
          let seen = collect iter [] in
          Test.assert_true
            (List.any
              seen
              ~fn:(String.equal (Path.to_string Path.(keep_dir / Path.v "a.ml"))));
          Test.assert_false
            (List.any
              seen
              ~fn:(String.equal (Path.to_string Path.(skip_dir / Path.v "b.ml"))));
          Ok ())); Test.case "fs walker contents_first emits directories after descendants"
    (fun _ctx ->
      with_tempdir "std_fs_walker_contents_first"
        (fun root ->
          let nested_dir = Path.(root / Path.v "nested") in
          let nested_file = Path.(nested_dir / Path.v "a.ml") in
          Fs.create_dir_all nested_dir |> Result.expect ~msg:"mkdir nested";
          Fs.write "let x = 1\n" nested_file |> Result.expect ~msg:"write nested";
          let entries =
            Fs.Walker.create ~roots:[ root ] ~sort:true ~contents_first:true ()
            |> Result.expect ~msg:"create walker"
            |> Fs.Walker.into_iter
            |> Iter.Iterator.to_list
            |> List.filter_map ~fn:(function
                | Ok (entry: Fs.Walker.FileItem.t) -> Some (Fs.Walker.FileItem.path_string entry)
                | Error _ -> None
              )
          in
          let file_index = find_index entries ~f:(String.equal (Path.to_string nested_file))
          |> Option.expect ~msg:"expected nested file in walker output" in
          let dir_index = find_index entries ~f:(String.equal (Path.to_string nested_dir))
          |> Option.expect ~msg:"expected nested dir in walker output" in
          Test.assert_true (file_index < dir_index);
          Ok ())); Test.case "fs walker to_list can omit directories"
    (fun _ctx ->
      with_tempdir "std_fs_walker_files_only"
        (fun root ->
          let nested_dir = Path.(root / Path.v "nested") in
          let nested_file = Path.(nested_dir / Path.v "a.ml") in
          Fs.create_dir_all nested_dir |> Result.expect ~msg:"mkdir nested";
          Fs.write "let x = 1\n" nested_file |> Result.expect ~msg:"write nested";
          let entries = Fs.Walker.to_list ~roots:[ root ] ~sort:true ~include_directories:false ()
          |> Result.expect ~msg:"to_list"
          |> List.map ~fn:Fs.Walker.FileItem.path_string in
          Test.assert_equal ~expected:[ Path.to_string nested_file ] ~actual:entries;
          Ok ())); ]

let main = fun ~args -> Test.Cli.main ~name:"std_fs_walker_tests" ~tests ~args

let () = Runtime.run ~main ~args:Env.args ()
