open Std

let () =
  let cmd =
    let open ArgParser in
    let open Arg in
    command "tusk-lint"
    |> version "0.1.0"
    |> about "Lint OCaml source files for common anti-patterns"
    |> args
         [
           positional "FILE" |> help "OCaml source file to lint" |> required true;
           flag "fix"
           |> long "fix"
           |> short 'f'
           |> help "Auto-fix issues when possible";
         ]
  in

  match ArgParser.get_matches cmd Env.args with
  | Error err ->
      ArgParser.print_error err;
      exit 1
  | Ok matches -> (
      match ArgParser.get_one matches "FILE" with
      | None ->
          println "Error: FILE argument is required";
          exit 1
      | Some file ->
          let _fix_mode = ArgParser.get_flag matches "fix" in

          let path = Path.v file in
          let content =
            Fs.read_to_string path
            |> Result.expect ~msg:(format "Failed to read %s" file)
          in

          let tokens = Syn.tokenize content in
          let trees = Syn.TokenTree.of_tokens tokens in

          let issues = Linter.run_rules Linter.all_rules trees in

          if List.length issues = 0 then println "No issues found!"
          else (
            List.iter
              (fun issue -> println "%s" (Lint_rule.format_issue issue))
              issues;
            exit 1))
