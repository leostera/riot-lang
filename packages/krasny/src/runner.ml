open Std
open Std.Iter

type file_result = {
  file : Path.t;
  needs_formatting : bool;
  error : string option;
}

type summary = {
  total_files : int;
  already_formatted : int;
  needs_formatting : int;
  failed_files : int;
  duration : Time.Duration.t;
}

type run_result = { files : file_result list; summary : summary }

let is_ocaml_source path =
  let path = Path.to_string path in
  String.ends_with ~suffix:".ml" path || String.ends_with ~suffix:".mli" path

let should_skip_directory path =
  let basename = Path.basename path in
  String.starts_with ~prefix:"." basename
  || String.equal basename "_build"
  || String.equal basename "target"

let rec walk_dir dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> MutIterator.to_list
      |> List.concat_map (fun entry ->
             let entry_path = Path.(dir / entry) in
             match Fs.is_dir entry_path with
             | Ok true ->
                 if should_skip_directory entry_path then
                   []
                 else
                   walk_dir entry_path
             | Ok false | Error _ ->
                 if is_ocaml_source entry_path then
                   [ entry_path ]
                 else
                   [])

let collect_ocaml_files ~roots =
  roots
  |> List.concat_map (fun root ->
         match Fs.is_dir root with
         | Ok true -> walk_dir root
         | Ok false | Error _ ->
             if is_ocaml_source root then
               [ root ]
             else
               [])
  |> List.sort_uniq (fun left right ->
         String.compare (Path.to_string left) (Path.to_string right))

let check_file file =
  match Fs.read file with
  | Error _ ->
      {
        file;
        needs_formatting = false;
        error = Some ("Failed to read " ^ Path.to_string file);
      }
  | Ok source ->
      let parsed = Syn.parse ~filename:file source in
      (match Format_core.format parsed with
      | Ok formatted ->
          { file; needs_formatting = not (String.equal source formatted); error = None }
      | Error err ->
          {
            file;
            needs_formatting = false;
            error = Some (Format_core.format_error_to_string err);
          })

let summarize ~duration files =
  List.fold_left
    (fun acc result ->
      match result.error, result.needs_formatting with
      | Some _, _ ->
          { acc with total_files = acc.total_files + 1; failed_files = acc.failed_files + 1 }
      | None, true ->
          {
            acc with
            total_files = acc.total_files + 1;
            needs_formatting = acc.needs_formatting + 1;
          }
      | None, false ->
          {
            acc with
            total_files = acc.total_files + 1;
            already_formatted = acc.already_formatted + 1;
          })
    {
      total_files = 0;
      already_formatted = 0;
      needs_formatting = 0;
      failed_files = 0;
      duration;
    }
    files

let run_checks ?(concurrency = System.available_parallelism) files =
  let concurrency = max 1 concurrency in
  let start = Time.Instant.now () in
  let files =
    List.sort (fun left right ->
        String.compare (Path.to_string left) (Path.to_string right))
      files
  in
  let results =
    WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files ~fn:check_file ()
    |> List.map snd
    |> List.sort (fun left right ->
           String.compare
             (Path.to_string left.file)
             (Path.to_string right.file))
  in
  let duration = Time.Instant.elapsed start in
  { files = results; summary = summarize ~duration results }
