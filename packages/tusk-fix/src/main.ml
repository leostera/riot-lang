open Std

(* Import library modules *)
module Diagnostic = Tusk_fix.Diagnostic
module Pipeline = Tusk_fix.Pipeline
module Reporter = Tusk_fix.Reporter
module File_scanner = Tusk_fix.File_scanner
module Coordinator = Tusk_fix.Coordinator
module Worker = Tusk_fix.Worker
module Messages = Tusk_fix.Messages

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
                "OCaml file or directory to lint (default: workspace packages)";
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
            (* Default to scanning workspace packages *)
            let cwd =
              Env.current_dir ()
              |> Result.expect ~msg:"Failed to get current directory"
            in
            (* Look for packages directory from workspace root *)
            let packages_dir = Path.(cwd / v "packages") in
            if Fs.is_dir packages_dir |> Result.unwrap_or ~default:false then
              packages_dir
            else cwd
      in
      let format =
        match format_str with
        | "json" -> Reporter.Json
        | "text" | _ -> Reporter.Text
      in

      (* Scan for files *)
      let files =
        match Fs.is_dir path with
        | Ok true ->
            let scanner = File_scanner.create ~root:path () in
            File_scanner.scan scanner
        | Ok false | Error _ -> [ path ]
      in

      if List.length files = 0 then (
        println "No OCaml files found.";
        Ok ())
      else
        let concurrency = min System.available_parallelism 50 in
        let concurrency = max concurrency 1 in
        let owner = self () in

        (* Spawn coordinator *)
        let _coordinator =
          Coordinator.start { files; concurrency; format; owner }
        in

        (* Wait for completion *)
        let selector = function
          | Messages.AllComplete result -> `select result
          | _ -> `skip
        in

        match receive ~selector () with
        | result ->
            if result.total_diagnostics > 0 then
              Error (Failure "Lint errors found")
            else Ok ())

let () = Miniriot.run ~main ~args:Env.args ()
