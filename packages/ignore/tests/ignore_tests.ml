open Std

module Test = Std.Test

let ( let* ) = fun value fn -> Result.and_then value ~fn

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let write = fun path text ->
  Fs.write text path
  |> Result.map_err ~fn:IO.error_message

let sorted = fun items -> List.sort items ~compare:String.compare

let packages_root_skipped_directories = [ "_build"; ".git"; ".riot"; ".tmp"; ".worktrees"; "dist"; ]

let path_is_within_skipped_packages_subtree = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Error _ -> false
  | Ok rel_path ->
      Path.components rel_path
      |> List.map ~fn:Path.to_string
      |> List.any
        ~fn:(fun component -> List.contains packages_root_skipped_directories ~value:component)

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

let compare_labeled_sets = fun labeled_sets ->
  match labeled_sets with
  | [] -> Ok ()
  | (baseline_label, baseline) :: rest ->
      let rec loop = fun __tmp1 ->
        match __tmp1 with
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

let glob_error_message = fun __tmp1 ->
  match __tmp1 with
  | Glob.Empty -> "empty glob"
  | Glob.Invalid_glob { input; message; offset } ->
      let offset =
        match offset with
        | None -> ""
        | Some offset -> " at offset " ^ Int.to_string offset
      in
      "invalid glob " ^ input ^ ": " ^ message ^ offset
  | Glob.Invalid_regex { message; offset } ->
      let offset =
        match offset with
        | None -> ""
        | Some offset -> " at offset " ^ Int.to_string offset
      in
      "invalid regex: " ^ message ^ offset

let with_lock = fun lock f ->
  Sync.Mutex.lock lock;
  match f () with
  | result ->
      Sync.Mutex.unlock lock;
      result
  | exception exn ->
      Sync.Mutex.unlock lock;
      raise exn

let collect_paths = fun walker ->
  Ignore.Walker.to_list walker
  |> Result.map ~fn:(List.map ~fn:Fs.Walker.FileItem.path_string)
  |> Result.map_err
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Ignore.Walker.File_system { cause; _ } -> IO.error_message cause
      | Ignore.Walker.Invalid_glob { path; line; message; _ } ->
          Path.to_string path ^ ":" ^ Int.to_string line ^ ": " ^ message)

let collect_file_paths = fun ~root walker ->
  Ignore.Walker.to_list walker
  |> Result.map
    ~fn:(
      List.filter_map
        ~fn:(fun (entry: Fs.Walker.FileItem.t) ->
          let path = Fs.Walker.FileItem.path entry in
          if path_is_within_skipped_packages_subtree ~root path then
            None
          else
            match Fs.Walker.FileItem.kind entry with
            | Fs.Walker.File -> Some (Fs.Walker.FileItem.path_string entry)
            | Fs.Walker.Directory
            | Fs.Walker.Symlink
            | Fs.Walker.Other -> None)
    )
  |> Result.map ~fn:sorted
  |> Result.map_err
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Ignore.Walker.File_system { cause; _ } -> IO.error_message cause
      | Ignore.Walker.Invalid_glob { path; line; message; _ } ->
          Path.to_string path ^ ":" ^ Int.to_string line ^ ": " ^ message)

let collect_file_paths_parallel = fun walker ->
  let lock = Sync.Mutex.create () in
  let seen = ref [] in
  Ignore.Walker.walk
    walker
    ~f:(fun entry ->
      match Fs.Walker.FileItem.kind entry with
      | Fs.Walker.File ->
          with_lock lock (fun () -> seen := Fs.Walker.FileItem.path_string entry :: !seen);
          Fs.Walker.Continue
      | Fs.Walker.Directory
      | Fs.Walker.Symlink
      | Fs.Walker.Other -> Fs.Walker.Continue)
  |> Result.map
    ~fn:(fun () ->
      !seen
      |> sorted)
  |> Result.map_err
    ~fn:(fun __tmp1 ->
      match __tmp1 with
      | Ignore.Walker.File_system { cause; _ } -> IO.error_message cause
      | Ignore.Walker.Invalid_glob { path; line; message; _ } ->
          Path.to_string path ^ ":" ^ Int.to_string line ^ ": " ^ message)

let contains_path = fun paths suffix ->
  List.any
    paths
    ~fn:(fun path -> String.ends_with ~suffix path)

let tests = [
  Test.case
    "ignore walker skips hidden directories by default"
    (fun _ctx ->
      with_tempdir
        "ignore_hidden"
        (fun root ->
          let hidden_dir = Path.(root / Path.v ".hidden") in
          let visible = Path.(root / Path.v "visible.txt") in
          let hidden_file = Path.(hidden_dir / Path.v "secret.txt") in
          let* () =
            Fs.create_dir_all hidden_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () = write visible "visible" in
          let* () = write hidden_file "secret" in
          let* walker =
            Ignore.Walker.create ~roots:[ root ] ()
            |> Result.map_err ~fn:(fun _ -> "walker create failed")
          in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "visible.txt");
          Test.assert_false (contains_path paths ".hidden");
          Test.assert_false (contains_path paths "secret.txt");
          Ok ()));
  Test.case
    "ignore walker respects root gitignore files"
    (fun _ctx ->
      with_tempdir
        "ignore_gitignore"
        (fun root ->
          let gitignore = Path.(root / Path.v ".gitignore") in
          let vendor_dir = Path.(root / Path.v "vendor") in
          let vendor_file = Path.(vendor_dir / Path.v "pkg.ml") in
          let src_dir = Path.(root / Path.v "src") in
          let src_file = Path.(src_dir / Path.v "main.ml") in
          let* () =
            Fs.create_dir_all vendor_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () =
            Fs.create_dir_all src_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () = write gitignore "vendor/\n" in
          let* () = write vendor_file "let vendor = 1\n" in
          let* () = write src_file "let main = 1\n" in
          let* walker =
            Ignore.Walker.create ~roots:[ root ] ~hidden:false ()
            |> Result.map_err ~fn:(fun _ -> "walker create failed")
          in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "src/main.ml");
          Test.assert_false (contains_path paths "vendor");
          Test.assert_false (contains_path paths "vendor/pkg.ml");
          Ok ()));
  Test.case
    "custom ignore files override gitignore files"
    (fun _ctx ->
      with_tempdir
        "ignore_custom"
        (fun root ->
          let gitignore = Path.(root / Path.v ".gitignore") in
          let dockerignore = Path.(root / Path.v ".dockerignore") in
          let file = Path.(root / Path.v "keep.txt") in
          let* () = write gitignore "keep.txt\n" in
          let* () = write dockerignore "!keep.txt\n" in
          let* () = write file "keep\n" in
          let* walker =
            Ignore.Walker.create
              ~roots:[ root ]
              ~hidden:false
              ~custom_ignore_filenames:[ ".dockerignore" ]
              ()
            |> Result.map_err ~fn:(fun _ -> "walker create failed")
          in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "keep.txt");
          Ok ()));
  Test.case
    "override globs keep unmatched directories but filter files"
    (fun _ctx ->
      with_tempdir
        "ignore_overrides"
        (fun root ->
          let dir = Path.(root / Path.v "src") in
          let file_ml = Path.(dir / Path.v "main.ml") in
          let file_txt = Path.(dir / Path.v "notes.txt") in
          let* () =
            Fs.create_dir_all dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () = write file_ml "let main = 1\n" in
          let* () = write file_txt "notes\n" in
          let* walker =
            Ignore.Walker.create ~roots:[ root ] ~hidden:false ~overrides:[ "*.ml" ] ()
            |> Result.map_err ~fn:(fun _ -> "walker create failed")
          in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "src");
          Test.assert_true (contains_path paths "main.ml");
          Test.assert_false (contains_path paths "notes.txt");
          Ok ()));
  Test.case
    "extra ignore patterns prune matching directories"
    (fun _ctx ->
      with_tempdir
        "ignore_extra_patterns"
        (fun root ->
          let tests_dir = Path.(root / Path.v "tests") in
          let fixtures_dir = Path.(tests_dir / Path.v "fixtures") in
          let fixtures_generated_dir = Path.(tests_dir / Path.v "fixtures-generated") in
          let visible_file = Path.(tests_dir / Path.v "demo_tests.ml") in
          let ignored_file = Path.(fixtures_dir / Path.v "ignored.ml") in
          let kept_file = Path.(fixtures_generated_dir / Path.v "keep.ml") in
          let* () =
            Fs.create_dir_all fixtures_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () =
            Fs.create_dir_all fixtures_generated_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () = write visible_file "let () = ()\n" in
          let* () = write ignored_file "let ignored = true\n" in
          let* () = write kept_file "let keep = true\n" in
          let* walker =
            Ignore.Walker.create ~roots:[ root ] ~hidden:false ~ignore_patterns:[ "fixtures" ] ()
            |> Result.map_err ~fn:(fun _ -> "walker create failed")
          in
          let* paths = collect_paths walker in
          Test.assert_true (contains_path paths "demo_tests.ml");
          Test.assert_false (contains_path paths "ignored.ml");
          Test.assert_true (contains_path paths "fixtures-generated/keep.ml");
          Ok ()));
  Test.case
    "parallel ignore walker keeps complete multiple-root file set"
    (fun _ctx ->
      with_tempdir
        "ignore_parallel_multiple_roots"
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
          let concurrency = Int.max 2 Thread.available_parallelism in
          let* () =
            Fs.create_dir_all net_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () =
            Fs.create_dir_all native_dir
            |> Result.map_err ~fn:IO.error_message
          in
          let* () = write Path.(net_dir / Path.v "udp_socket.mli") "type t\n" in
          let* () = write Path.(net_dir / Path.v "udp_socket.ml") "type t = unit\n" in
          let* () =
            write
              Path.(net_dir / Path.v "udp_server.mli")
              "type handler = socket:Udp_socket.t -> bytes -> unit\n"
          in
          let* () =
            write
              Path.(net_dir / Path.v "udp_server.ml")
              "type handler = socket:Udp_socket.t -> bytes -> unit\n"
          in
          let* () = write Path.(native_dir / Path.v "shim.c") "int shim(void) { return 1; }\n" in
          let rec run iteration =
            if iteration = 0 then
              Ok ()
            else
              let* walker =
                Ignore.Walker.create
                  ~roots:[ src_dir; native_dir ]
                  ~concurrency
                  ~sort:false
                  ~hidden:false
                  ()
                |> Result.map_err ~fn:(fun _ -> "walker create failed")
              in
              let* actual = collect_file_paths_parallel walker in
              if actual = expected then
                run (iteration - 1)
              else
                Error ("expected parallel ignore walker file set ["
                ^ String.concat ", " expected
                ^ "] but got ["
                ^ String.concat ", " actual
                ^ "]")
          in
          run 25));
  Test.case
    ~size:Large
    "ignore walker to_list packages file set is stable across concurrency and sort modes"
    (fun _ctx ->
      let* packages_root = find_packages_root () in
      let configs = [
        ("concurrency=1 sort=true", 1, true);
        ("concurrency=1 sort=false", 1, false);
        ("concurrency=2 sort=true", 2, true);
        ("concurrency=2 sort=false", 2, false);
        ("concurrency=available sort=true", Thread.available_parallelism, true);
        ("concurrency=available sort=false", Thread.available_parallelism, false);
      ]
      in
      let rec collect acc = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok (List.reverse acc)
        | (label, concurrency, sort) :: rest ->
            let* walker =
              Ignore.Walker.create ~roots:[ packages_root ] ~concurrency ~sort ()
              |> Result.map_err ~fn:glob_error_message
            in
            let* paths = collect_file_paths ~root:packages_root walker in
            collect ((label, paths) :: acc) rest
      in
      let* labeled_sets = collect [] configs in
      compare_labeled_sets labeled_sets);
]

let main ~args = Test.Cli.main ~name:"ignore_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
