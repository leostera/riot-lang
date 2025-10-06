open Std

let () =
  let cmd =
    let open ArgParser in
    let open Arg in
    command "tusk-fmt" |> version "0.1.0"
    |> about "Format OCaml source files"
    |> args
         [
           positional "FILE"
           |> help "OCaml source file to format"
           |> required true;
           flag "check" |> long "check" |> short 'c'
           |> help "Check if file needs formatting (exit 1 if not formatted)";
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
          println "";
          exit 1
      | Some file ->
          let check_mode = ArgParser.get_flag matches "check" in

          let path = Path.v file in
          let content =
            Fs.read_to_string path
            |> Result.expect ~msg:(format "Failed to read %s" file)
          in

          let tokens = Syn.tokenize content in
          let trees = Syn.TokenTree.of_tokens tokens in
          (* Debug: print tree structure *)
          if false then (
            Printf.eprintf "Trees count: %d\n" (List.length trees);
            List.iteri (fun i tree ->
              Printf.eprintf "Tree %d: %s\n" i (match tree with
                | Syn.TokenTree.Tree (Syn.Token.BeginEnd, _) -> "BeginEnd"
                | Syn.TokenTree.Tree _ -> "Tree"
                | Syn.TokenTree.Token _ -> "Token")
            ) trees);
          let formatted = Formatter.format trees in

          if check_mode then if content = formatted then exit 0 else exit 1
          else println "%s" formatted)
