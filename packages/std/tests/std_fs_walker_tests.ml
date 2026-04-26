open Std

module Test = Std.Test

let ( let* ) = fun value fn -> Result.and_then value ~fn

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

let sorted = fun items -> List.sort items ~compare:String.compare

let packages_root_skipped_directories = [ "_build"; ".git"; ".riot"; ".tmp"; ".worktrees"; "dist"; ]

let should_skip_packages_subtree = fun ~root path ->
  (not (Path.equal root path))
  && List.any packages_root_skipped_directories ~fn:(String.equal (Path.basename path))

let find_packages_root = fun () ->
  let rec loop dir =
    let agents_md = Path.(dir / Path.v "AGENTS.md") in
    let packages_dir = Path.(dir / Path.v "packages") in
    let* has_agents =
      Fs.exists agents_md
      |> Result.map_err ~fn:IO.error_message
    in
    let* has_packages_dir =
      Fs.is_dir packages_dir
      |> Result.map_err ~fn:IO.error_message
    in
    if has_agents && has_packages_dir then
      Ok packages_dir
    else
      match Path.parent dir with
      | None -> Error ("failed to find repository root from " ^ Path.to_string dir)
      | Some parent ->
          if Path.equal parent dir then
            Error ("failed to find repository root from " ^ Path.to_string dir)
          else
            loop parent
  in
  let* cwd =
    Env.current_dir ()
    |> Result.map_err ~fn:(fun _ -> "failed to read current dir")
  in
  loop cwd

let collect_file_paths = fun ~root ~sort ->
  let seen = ref [] in
  Fs.Walker.walk
    ~roots:[ root ]
    ~sort
    ~f:(fun entry ->
      let path = Fs.Walker.FileItem.path entry in
      match Fs.Walker.FileItem.kind entry with
      | Fs.Walker.Directory when should_skip_packages_subtree ~root path -> Fs.Walker.Skip_subtree
      | Fs.Walker.File ->
          seen := Fs.Walker.FileItem.path_string entry :: !seen;
          Fs.Walker.Continue
      | Fs.Walker.Directory
      | Fs.Walker.Symlink
      | Fs.Walker.Other -> Fs.Walker.Continue)
    ()
  |> Result.map_err ~fn:IO.error_message
  |> Result.map
    ~fn:(fun () ->
      !seen
      |> sorted)

let compare_labeled_sets = fun labeled_sets ->
  match labeled_sets with
  | [] -> Ok ()
  | (baseline_label, baseline) :: rest ->
      let rec loop = function
        | [] -> Ok ()
        | (label, paths) :: remaining ->
            if baseline = paths then
              loop remaining
            else
              let mismatch =
                match (baseline, paths) with
                | (expected :: _, actual :: _) when not (String.equal expected actual) ->
                    "first mismatch: expected " ^ expected ^ " but got " ^ actual
                | ([], actual :: _) -> "baseline was empty but " ^ label ^ " produced " ^ actual
                | (expected :: _, []) -> label ^ " was empty but baseline produced " ^ expected
                | _ ->
                    "same length="
                    ^ Int.to_string (List.length baseline)
                    ^ " but different ordering or contents"
              in
              Error (label
              ^ " did not match "
              ^ baseline_label
              ^ " ("
              ^ mismatch
              ^ "; baseline_count="
              ^ Int.to_string (List.length baseline)
              ^ "; actual_count="
              ^ Int.to_string (List.length paths)
              ^ ")")
      in
      loop rest

let tests = [
  Test.case
    "fs walker rejects invalid depth ranges"
    (fun _ctx ->
      match Fs.Walker.create ~roots:[ Path.v "." ] ~min_depth:3 ~max_depth:1 () with
      | Error (Fs.Walker.MinDepthCannotBeMoreThanMaxDepth { min_depth; max_depth }) ->
          Test.assert_equal ~expected:3 ~actual:min_depth;
          Test.assert_equal ~expected:1 ~actual:max_depth;
          Ok ()
      | Ok _ -> Error "expected invalid depth range");
  Test.case
    "fs walker can prune subtrees"
    (fun _ctx ->
      with_tempdir
        "std_fs_walker"
        (fun root ->
          let keep_dir = Path.(root / Path.v "keep") in
          let skip_dir = Path.(root / Path.v "skip") in
          Fs.create_dir_all keep_dir
          |> Result.expect ~msg:"mkdir keep";
          Fs.create_dir_all skip_dir
          |> Result.expect ~msg:"mkdir skip";
          Fs.write "let keep = ()\n" Path.(keep_dir / Path.v "a.ml")
          |> Result.expect ~msg:"write keep";
          Fs.write "let skip = ()\n" Path.(skip_dir / Path.v "b.ml")
          |> Result.expect ~msg:"write skip";
          let seen = ref [] in
          Fs.Walker.walk
            ~roots:[ root ]
            ~sort:true
            ~f:(fun entry ->
              let path = Fs.Walker.FileItem.path entry in
              seen := Path.to_string path :: !seen;
              if String.equal (Path.basename path) "skip" then
                Fs.Walker.Skip_subtree
              else
                Fs.Walker.Continue)
            ()
          |> Result.expect ~msg:"walk";
          let seen = List.reverse !seen in
          Test.assert_true
            (List.any seen ~fn:(String.equal (Path.to_string Path.(keep_dir / Path.v "a.ml"))));
          Test.assert_false
            (List.any seen ~fn:(String.equal (Path.to_string Path.(skip_dir / Path.v "b.ml"))));
          Ok ()));
  Test.case
    "fs walker filter_entry skips directories lazily"
    (fun _ctx ->
      with_tempdir
        "std_fs_walker_filter"
        (fun root ->
          let keep_dir = Path.(root / Path.v "keep") in
          let skip_dir = Path.(root / Path.v "skip") in
          Fs.create_dir_all keep_dir
          |> Result.expect ~msg:"mkdir keep";
          Fs.create_dir_all skip_dir
          |> Result.expect ~msg:"mkdir skip";
          Fs.write "let keep = ()\n" Path.(keep_dir / Path.v "a.ml")
          |> Result.expect ~msg:"write keep";
          Fs.write "let skip = ()\n" Path.(skip_dir / Path.v "b.ml")
          |> Result.expect ~msg:"write skip";
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
            | (None, _) -> List.reverse acc
            | (Some (Ok (entry: Fs.Walker.FileItem.t)), iter') ->
                collect iter' (Fs.Walker.FileItem.path_string entry :: acc)
            | (Some (Error _err), iter') -> collect iter' acc
          in
          let seen = collect iter [] in
          Test.assert_true
            (List.any seen ~fn:(String.equal (Path.to_string Path.(keep_dir / Path.v "a.ml"))));
          Test.assert_false
            (List.any seen ~fn:(String.equal (Path.to_string Path.(skip_dir / Path.v "b.ml"))));
          Ok ()));
  Test.case
    "fs walker contents_first emits directories after descendants"
    (fun _ctx ->
      with_tempdir
        "std_fs_walker_contents_first"
        (fun root ->
          let nested_dir = Path.(root / Path.v "nested") in
          let nested_file = Path.(nested_dir / Path.v "a.ml") in
          Fs.create_dir_all nested_dir
          |> Result.expect ~msg:"mkdir nested";
          Fs.write "let x = 1\n" nested_file
          |> Result.expect ~msg:"write nested";
          let entries =
            Fs.Walker.create ~roots:[ root ] ~sort:true ~contents_first:true ()
            |> Result.expect ~msg:"create walker"
            |> Fs.Walker.into_iter
            |> Iter.Iterator.to_list
            |> List.filter_map
              ~fn:(
                function
                | Ok (entry: Fs.Walker.FileItem.t) -> Some (Fs.Walker.FileItem.path_string entry)
                | Error _ -> None
              )
          in
          let file_index =
            find_index entries ~f:(String.equal (Path.to_string nested_file))
            |> Option.expect ~msg:"expected nested file in walker output"
          in
          let dir_index =
            find_index entries ~f:(String.equal (Path.to_string nested_dir))
            |> Option.expect ~msg:"expected nested dir in walker output"
          in
          Test.assert_true (file_index < dir_index);
          Ok ()));
  Test.case
    "fs walker to_list can omit directories"
    (fun _ctx ->
      with_tempdir
        "std_fs_walker_files_only"
        (fun root ->
          let nested_dir = Path.(root / Path.v "nested") in
          let nested_file = Path.(nested_dir / Path.v "a.ml") in
          Fs.create_dir_all nested_dir
          |> Result.expect ~msg:"mkdir nested";
          Fs.write "let x = 1\n" nested_file
          |> Result.expect ~msg:"write nested";
          let entries =
            Fs.Walker.to_list ~roots:[ root ] ~sort:true ~include_directories:false ()
            |> Result.expect ~msg:"to_list"
            |> List.map ~fn:Fs.Walker.FileItem.path_string
          in
          Test.assert_equal ~expected:[ Path.to_string nested_file ] ~actual:entries;
          Ok ()));
  Test.case
    "fs walker unsorted multiple roots keep complete file set"
    (fun _ctx ->
      with_tempdir
        "std_fs_walker_multiple_roots"
        (fun root ->
          let src_dir = Path.(root / Path.v "src") in
          let net_dir = Path.(src_dir / Path.v "net") in
          let native_dir = Path.(root / Path.v "native") in
          let expected =
            [
              Path.to_string Path.(net_dir / Path.v "udp_socket.mli");
              Path.to_string Path.(net_dir / Path.v "udp_socket.ml");
              Path.to_string Path.(net_dir / Path.v "udp_server.mli");
              Path.to_string Path.(net_dir / Path.v "udp_server.ml");
              Path.to_string Path.(native_dir / Path.v "shim.c");
            ]
            |> sorted
          in
          Fs.create_dir_all net_dir
          |> Result.expect ~msg:"mkdir net";
          Fs.create_dir_all native_dir
          |> Result.expect ~msg:"mkdir native";
          Fs.write "type t\n" Path.(net_dir / Path.v "udp_socket.mli")
          |> Result.expect ~msg:"write udp_socket.mli";
          Fs.write "type t = unit\n" Path.(net_dir / Path.v "udp_socket.ml")
          |> Result.expect ~msg:"write udp_socket.ml";
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\n"
            Path.(net_dir / Path.v "udp_server.mli")
          |> Result.expect ~msg:"write udp_server.mli";
          Fs.write
            "type handler = socket:Udp_socket.t -> bytes -> unit\n"
            Path.(net_dir / Path.v "udp_server.ml")
          |> Result.expect ~msg:"write udp_server.ml";
          Fs.write "int shim(void) { return 1; }\n" Path.(native_dir / Path.v "shim.c")
          |> Result.expect ~msg:"write shim.c";
          let rec run iteration =
            if iteration = 0 then
              Ok ()
            else
              let actual =
                Fs.Walker.to_list
                  ~roots:[ src_dir; native_dir ]
                  ~sort:false
                  ~include_directories:false
                  ()
                |> Result.expect ~msg:"to_list"
                |> List.map ~fn:Fs.Walker.FileItem.path_string
                |> sorted
              in
              if actual = expected then
                run (iteration - 1)
              else
                Error ("expected unsorted multiple-root walker output ["
                ^ String.concat ", " expected
                ^ "] but got ["
                ^ String.concat ", " actual
                ^ "]")
          in
          run 25));
  Test.case
    ~size:Large
    "fs walker packages file set is stable across sort modes"
    (fun _ctx ->
      let* packages_root = find_packages_root () in
      let configs = [ ("sort=true", true); ("sort=false", false); ] in
      let rec collect acc = function
        | [] -> Ok (List.reverse acc)
        | (label, sort) :: rest ->
            let* paths = collect_file_paths ~root:packages_root ~sort in
            collect ((label, paths) :: acc) rest
      in
      let* labeled_sets = collect [] configs in compare_labeled_sets labeled_sets);
]

let main ~args = Test.Cli.main ~name:"std_fs_walker_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
