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
          if Sys.getenv_opt "DEBUG_TREES" <> None then (
            let rec print_tree indent = function
              | Syn.TokenTree.Token tok ->
                  Printf.eprintf "%s- Token: %s\n" (String.make (indent*2) ' ')
                    (match tok with
                     | Syn.Token.Keyword kw ->
                         Printf.sprintf "Keyword(%s)" 
                           (match kw with
                            | Syn.Token.Let -> "let"
                            | Syn.Token.Type -> "type"
                            | Syn.Token.Module -> "module"
                            | Syn.Token.Open -> "open"
                            | Syn.Token.Struct -> "struct"
                            | Syn.Token.End -> "end"
                            | _ -> "...")
                     | Syn.Token.Ident s -> Printf.sprintf "Ident(%s)" s
                     | Syn.Token.Literal (Syn.Token.Int i) -> Printf.sprintf "Int(%d)" i
                     | Syn.Token.Comment { value; _ } -> Printf.sprintf "Comment(%s...)" (String.sub value 0 (min 20 (String.length value)))
                     | Syn.Token.Docstring { value; _ } -> Printf.sprintf "Docstring(%s...)" (String.sub value 0 (min 20 (String.length value)))
                     | Syn.Token.Eq -> "="
                     | Syn.Token.Semi -> ";"
                     | Syn.Token.Whitespace -> "Whitespace"
                     | _ -> "...")
              | Syn.TokenTree.Tree (delim, children) ->
                  Printf.eprintf "%sTree(%s) [\n" (String.make (indent*2) ' ')
                    (match delim with
                     | Syn.Token.BeginEnd -> "BeginEnd"
                     | Syn.Token.Paren -> "Paren"
                     | Syn.Token.Bracket -> "Bracket"
                     | Syn.Token.Brace -> "Brace"
                     | _ -> "...");
                  List.iter (print_tree (indent + 1)) children;
                  Printf.eprintf "%s]\n" (String.make (indent*2) ' ')
            in
            Printf.eprintf "\n=== TOKEN TREES ===\n";
            Printf.eprintf "Total trees: %d\n" (List.length trees);
            List.iteri (fun i tree ->
              Printf.eprintf "\nTree #%d:\n" i;
              print_tree 0 tree
            ) trees;
            Printf.eprintf "=================\n\n"
          );
          let formatted = Formatter.format trees in

          if check_mode then if content = formatted then exit 0 else exit 1
          else println "%s" formatted)
