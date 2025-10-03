open Std
open Core
open Model

type format_status =
  | Success of { file : Path.t; changed : bool }
  | Failed of { file : Path.t; error : string }

let collect_ocaml_files workspace =
  let rec walk_dir dir =
    match Fs.read_dir dir with
    | Error _ -> []
    | Ok entries ->
        let entry_list = MutIterator.to_list entries in
        List.concat_map
          (fun entry ->
            let entry_path = Path.(dir / entry) in
            match Fs.is_dir entry_path with
            | Ok true ->
                if
                  String.starts_with ~prefix:"." (Path.to_string entry)
                  || String.equal (Path.to_string entry) "_build"
                  || String.equal (Path.to_string entry) "target"
                then []
                else walk_dir entry_path
            | Ok false | Error _ ->
                let path_str = Path.to_string entry_path in
                if
                  String.ends_with ~suffix:".ml" path_str
                  || String.ends_with ~suffix:".mli" path_str
                then [ entry_path ]
                else [])
          entry_list
  in
  List.concat_map
    (fun (pkg : Workspace.package) ->
      walk_dir Path.(workspace.Workspace.root / pkg.path))
    workspace.packages

let format_file toolchain check_only file_path =
  let result =
    match Ocaml.Ocamlformat.format_file ~toolchain ~file_path ~check_only with
    | Ocaml.Ocamlformat.Formatted { changed; _ } ->
        Success { file = file_path; changed }
    | Ocaml.Ocamlformat.Error err -> Failed { file = file_path; error = err }
  in
  let status_char =
    match result with
    | Success { changed = true; _ } -> "\027[1;32m✓\027[0m"
    | Success { changed = false; _ } -> "\027[1;90m-\027[0m"
    | Failed _ -> "\027[1;31m✗\027[0m"
  in
  println "%s %s" status_char (Path.to_string file_path);
  result

let run args =
  let check_only = List.mem "--check" args in

  let cwd =
    Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"
  in
  let workspace =
    Workspace_manager.scan cwd |> Result.expect ~msg:"Failed to scan workspace"
  in

  let toolchain = Toolchains.ready_toolchains workspace in

  println "🎨 Formatting OCaml files...";
  println "";

  let files = collect_ocaml_files workspace in
  let file_count = List.length files in

  if file_count = 0 then (
    println "No OCaml files found.";
    Ok ())
  else
    let concurrency = min (System.available_parallelism / 2) 8 in
    let concurrency = max concurrency 1 in

    println "Found %d files, using %d workers" file_count concurrency;
    println "";

    let results =
      WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files
        ~fn:(fun file -> format_file toolchain check_only file)
        ()
    in

    println "";

    let successes =
      List.filter_map
        (fun (_idx, result) ->
          match result with
          | Success { file; changed } -> Some (file, changed)
          | Failed _ -> None)
        results
    in
    let failures =
      List.filter_map
        (fun (_idx, result) ->
          match result with
          | Failed { file; error } -> Some (file, error)
          | Success _ -> None)
        results
    in

    let changed_count =
      List.filter (fun (_file, changed) -> changed) successes |> List.length
    in
    let unchanged_count = List.length successes - changed_count in
    let failed_count = List.length failures in

    if failed_count > 0 then (
      println "\027[1;31mFailed to format %d files:\027[0m" failed_count;
      println "";
      List.iter
        (fun (file, error) ->
          println "  \027[1;31m✗\027[0m %s" (Path.to_string file);
          println "    %s" error;
          println "")
        failures;
      println "");

    if check_only then (
      if changed_count > 0 then (
        println "\027[1;33m%d files need formatting:\027[0m" changed_count;
        List.iter
          (fun (file, changed) ->
            if changed then println "  • %s" (Path.to_string file))
          successes;
        println "";
        println "Run 'tusk fmt' to format these files.";
        Error (Failure "Files need formatting"))
      else (
        println "\027[1;32m✓ All files are formatted correctly\027[0m";
        println "  %d files checked" file_count;
        Ok ()))
    else (
      println "\027[1;32m✓ Formatting complete\027[0m";
      if changed_count > 0 then println "  %d files formatted" changed_count;
      if unchanged_count > 0 then
        println "  %d files already formatted" unchanged_count;
      if failed_count > 0 then
        Error (Failure (format "%d files failed to format" failed_count))
      else Ok ())
