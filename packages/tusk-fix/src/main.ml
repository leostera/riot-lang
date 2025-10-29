open Std
open Std.Iter

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

type lint_result = {
  file : Path.t;
  source : string;
  diagnostics : Diagnostic.t list;
}

let lint_file pipeline file_path =
  match Fs.read file_path with
  | Error _ -> { file = file_path; source = ""; diagnostics = [] }
  | Ok source -> (
      try
        let filename = Path.to_string file_path in
        let result = Pipeline.run pipeline ~filename source in
        { file = file_path; source; diagnostics = result.diagnostics }
      with exn ->
        let error_msg = format "Parser error: %s" (Printexc.to_string exn) in
        let diag =
          Diagnostic.make ~severity:Error ~message:error_msg
            ~span:(Syn.Ceibo.Span.make ~start:0 ~end_:0)
            ~rule_id:"parser-error" ()
        in
        { file = file_path; source; diagnostics = [ diag ] })

let main ~args:argv =
  let cmd =
    ArgParser.command "tusk_fix"
    |> ArgParser.about "OCaml linter and fixer"
    |> ArgParser.version "0.1.0"
    |> ArgParser.args
         [
           ArgParser.Arg.option "format"
           |> ArgParser.Arg.long "format"
           |> ArgParser.Arg.value_name "FORMAT"
           |> ArgParser.Arg.help "Output format (text or json)";
           ArgParser.Arg.positional "path"
           |> ArgParser.Arg.required false
           |> ArgParser.Arg.help
                "OCaml file or directory to lint (default: current directory)";
         ]
  in
  match ArgParser.get_matches cmd argv with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "Argument parsing failed")
  | Ok matches -> (
      let format_str =
        ArgParser.get_one matches "format" |> Option.unwrap_or ~default:"text"
      in
      let path =
        match ArgParser.get_path matches "path" with
        | Some p -> p
        | None ->
            Env.current_dir ()
            |> Result.expect ~msg:"Failed to get current directory"
      in
      let format =
        match format_str with
        | "json" -> Reporter.Json
        | "text" | _ -> Reporter.Text
      in
      let files =
        match Fs.is_dir path with
        | Ok true -> walk_dir path
        | Ok false | Error _ -> [ path ]
      in
      if List.length files = 0 then (
        println "No OCaml files found.";
        Ok ())
      else
        let pipeline = Pipeline.default () in
        let concurrency = min System.available_parallelism 50 in
        let concurrency = max concurrency 1 in
        let results =
          WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files
            ~fn:(fun file -> lint_file pipeline file)
            ()
        in
        let all_diagnostics =
          List.concat_map
            (fun (_idx, result) ->
              List.map
                (fun diag -> (result.file, result.source, diag))
                result.diagnostics)
            results
        in
        match format with
        | Reporter.Text ->
            let files_table = Collections.HashMap.create () in
            List.iter
              (fun (file, source, diag) ->
                let key = Path.to_string file in
                match Collections.HashMap.get files_table key with
                | Some (existing_source, existing_diags) ->
                    ignore
                      (Collections.HashMap.insert files_table key
                         (existing_source, existing_diags @ [ diag ]))
                | None ->
                    ignore
                      (Collections.HashMap.insert files_table key
                         (source, [ diag ])))
              all_diagnostics;
            Collections.HashMap.iter
              (fun file_str (source, diags) ->
                let file = Path.v file_str in
                let grouped = Diagnostic.group_diagnostics diags in
                List.iter
                  (fun grouped_diag ->
                    print "%s"
                      (Diagnostic.grouped_to_formatted_output ~file ~source
                         grouped_diag))
                  grouped)
              files_table;
            if List.length all_diagnostics > 0 then
              Error (Failure "Lint errors found")
            else Ok ()
        | Reporter.Json ->
            let open Data.Json in
            let json_diagnostics =
              List.map
                (fun (file, _source, diag) ->
                  Object
                    [
                      ("file", String (Path.to_string file));
                      ("diagnostic", Diagnostic.to_json diag);
                    ])
                all_diagnostics
            in
            let json =
              Object
                [
                  ("diagnostics", Array json_diagnostics);
                  ("count", Int (List.length all_diagnostics));
                ]
            in
            println "%s" (to_string json);
            if List.length all_diagnostics > 0 then
              Error (Failure "Lint errors found")
            else Ok ())

let () = Miniriot.run ~main ~args:Env.args ()
