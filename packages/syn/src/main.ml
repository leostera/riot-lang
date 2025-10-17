open Syn
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
        let json_tokens =
          List.map
            (fun tok ->
              Data.Json.obj
                [
                  ("kind", Data.Json.string (Token.show_kind tok.Token.kind));
                  ("start", Data.Json.int tok.Token.span.start);
                  ("end", Data.Json.int tok.Token.span.end_);
                ])
            tokens
        in
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
  let red_tree = ArgParser.get_flag sub_matches "red-tree" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error "Error reading file %s" file;
      exit 1
  | Ok source ->
      let tokens = Lexer.tokenize source in
      let result =
        if String.ends_with ~suffix:".mli" file then
          Parser.parse_interface ~source tokens
        else Parser.parse_implementation ~source tokens
      in

      if json then
        let kind_to_json kind = Data.Json.String (SyntaxKind.to_string kind) in
        let text_to_json text = Data.Json.String text in

        if red_tree then
          let red_root = Ceibo.Red.new_root result.tree in
          let tree_json =
            Ceibo.Red.to_json ~kind_to_json ~text_to_json
              (Ceibo.Red.Node red_root)
          in
          println "%s" (Data.Json.to_string tree_json)
        else
          let tree_json =
            Ceibo.Green.to_json ~kind_to_json ~text_to_json
              (Ceibo.Green.Node result.tree)
          in

          let output =
            Data.Json.Object
              [
                ("tree", tree_json);
                ( "diagnostics",
                  Data.Json.Array
                    (List.map Diagnostic.to_json result.diagnostics) );
              ]
          in

          println "%s" (Data.Json.to_string output)
      else if result.diagnostics <> [] then
        DiagnosticReporter.print ~file ~source result.diagnostics
      else (
        Log.info "Parsed successfully";
        let width = Ceibo.Green.width (Ceibo.Green.Node result.tree) in
        Log.info "Tree width: %d bytes" width)

let handle_explain sub_matches =
  let error_code =
    ArgParser.get_one sub_matches "ERROR_CODE"
    |> Option.expect ~msg:"ERROR_CODE required"
  in
  match Error.id_of_string error_code with
  | Some id -> println "%s\n" (Error.explain id)
  | None ->
      Log.error "Unknown error code: %s" error_code;
      exit 1

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
           command "tokenize"
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
                  flag "red-tree" |> long "red-tree"
                  |> help "Output red tree (with spans) instead of green tree";
                ];
           (* explain subcommand *)
           command "explain"
           |> about "Explain an error code"
           |> args
                [
                  positional "ERROR_CODE"
                  |> help "Error code to explain (e.g., E0001)"
                  |> required true;
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
      | Some ("tokenize", sub_matches) -> handle_token_stream sub_matches
      | Some ("parse", sub_matches) -> handle_parse sub_matches
      | Some ("explain", sub_matches) -> handle_explain sub_matches
      | _ ->
          ArgParser.print_help cmd;
          exit 1)
