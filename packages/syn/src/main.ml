open Std

let handle_token_stream sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error "Error reading file %s" file;
      exit 1
  | Ok content ->
      let tokens = Lexer.tokenize content in
      if json then
        let json_tokens = List.map (fun tok -> 
          Data.Json.obj [
            ("kind", Data.Json.string (Token.show_kind tok.Token.kind));
            ("start", Data.Json.int tok.Token.span.start);
            ("end", Data.Json.int tok.Token.span.end_);
          ]
        ) tokens in
        println "%s" (Data.Json.to_string (Data.Json.array json_tokens))
      else
        List.iter
          (fun tok -> println "%s" (Token.show_kind tok.Token.kind))
          tokens

let handle_parse sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error "Error reading file %s" file;
      exit 1
  | Ok source ->
      let result =
        Parser.parse ~source ~filename:file (Lexer.tokenize source)
      in

      if json then
        let kind_to_json kind = Data.Json.String (Syntax_kind.to_string kind) in
        let text_to_json text = Data.Json.String text in

        let tree_json =
          Ceibo.Green.to_json ~kind_to_json ~text_to_json
            (Ceibo.Green.Node result.tree)
        in

        let output =
          Data.Json.Object
            [
              ("tree", tree_json);
              ( "diagnostics",
                Data.Json.Array (List.map Diagnostic.to_json result.diagnostics)
              );
            ]
        in

        println "%s" (Data.Json.to_string output)
      else if result.diagnostics <> [] then (
        Log.error "Parse errors:";
        List.iter
          (fun diag -> Log.error "  %s" (Diagnostic.to_string diag))
          result.diagnostics)
      else (
        Log.info "Parsed successfully";
        let width = Ceibo.Green.width (Ceibo.Green.Node result.tree) in
        Log.info "Tree width: %d bytes" width)

let () =
  (* Parse command line arguments *)
  let cmd =
    let open ArgParser in
    let open Arg in
    command "syn" |> version "0.1.0"
    |> about "OCaml syntax analysis tool"
    |> subcommands
         [
           (* token-stream subcommand *)
           command "token-stream"
           |> about "Print token stream for a file"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to tokenize"
                  |> required true;
                  flag "json" |> long "json" |> help "Output in JSON format";
                ];
           (* parse subcommand *)
           command "parse"
           |> about "Parse file into Ceibo syntax tree"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to parse"
                  |> required true;
                  flag "json" |> long "json"
                  |> help "Output syntax tree as JSON";
                ];
         ]
  in

  match ArgParser.get_matches cmd Env.args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      exit 1
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | None ->
          ArgParser.print_help cmd;
          exit 1
      | Some ("token-stream", sub_matches) -> handle_token_stream sub_matches
      | Some ("parse", sub_matches) -> handle_parse sub_matches
      | Some (cmd, _) ->
          Log.error "Unknown subcommand: %s" cmd;
          exit 1)
