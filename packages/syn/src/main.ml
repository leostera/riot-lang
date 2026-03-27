open Syn
open Std

let parse_file ~file ~source =
  let tokens = Lexer.tokenize source in
  if String.ends_with ~suffix:".mli" file then
    Parser.parse_interface ~source tokens
  else Parser.parse_implementation ~source tokens

let parse_result_to_ceibo_json result =
  let kind_to_json kind = Data.Json.String (SyntaxKind.to_string kind) in
  let text_to_json text = Data.Json.String text in
  let tree_json =
    Ceibo.Green.to_json ~kind_to_json ~text_to_json
      (Ceibo.Green.Node result.Parser.tree)
  in
  Data.Json.Object
    [
      ("tree", tree_json);
      ("diagnostics", Data.Json.Array (List.map Diagnostic.to_json result.diagnostics));
    ]

let handle_token_stream sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
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
        println (Data.Json.to_string (Data.Json.array json_tokens))
      else
        List.iter
          (fun tok -> println (Token.show_kind tok.Token.kind))
          tokens

let handle_parse sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  let red_tree = ArgParser.get_flag sub_matches "red-tree" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source ->
      let result = parse_file ~file ~source in

      if json then
        let kind_to_json kind = Data.Json.String (SyntaxKind.to_string kind) in
        let text_to_json text = Data.Json.String text in

        if red_tree then
          let red_root = Ceibo.Red.new_root result.tree in
          let tree_json =
            Ceibo.Red.to_json ~kind_to_json ~text_to_json
              (Ceibo.Red.Node red_root)
          in
          println (Data.Json.to_string tree_json)
        else
          println (Data.Json.to_string (parse_result_to_ceibo_json result))
      else if result.diagnostics != [] then
        DiagnosticReporter.print ~file ~source result.diagnostics
      else (
        Log.info "Parsed successfully";
        let width = Ceibo.Green.width (Ceibo.Green.Node result.tree) in
        Log.info ("Tree width: " ^ Int.to_string width ^ " bytes"))

let handle_print_ceibo sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source ->
      parse_file ~file ~source
      |> parse_result_to_ceibo_json
      |> Data.Json.to_string
      |> println

let handle_print_cst sub_matches =
  let file =
    ArgParser.get_one sub_matches "FILE" |> Option.expect ~msg:"FILE required"
  in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      exit 1
  | Ok source ->
      let result = parse_file ~file ~source in
      let json =
        if result.diagnostics != [] then
          Data.Json.Object
            [
              ("status", Data.Json.String "parse_error");
              ( "diagnostics",
                Data.Json.Array (List.map Diagnostic.to_json result.diagnostics) );
            ]
        else
          CstBuilder.create_from_ceibo
            ~kind:
              (if String.ends_with ~suffix:".mli" file then
                 `Interface
               else `Implementation)
            result.tree
          |> CstJson.of_result
      in
      println (Data.Json.to_string json)

let handle_explain sub_matches =
  let error_code =
    ArgParser.get_one sub_matches "ERROR_CODE"
    |> Option.expect ~msg:"ERROR_CODE required"
  in
  match Error.id_of_string error_code with
  | Some id -> println (Error.explain id ^ "\n")
  | None ->
      Log.error ("Unknown error code: " ^ error_code);
      exit 1

let main ~args =
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
           command "print-ceibo"
           |> about "Print lossless Ceibo parse result as JSON"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to parse"
                  |> required true;
                ];
           command "print-cst"
           |> about "Print typed CST lift result as JSON"
           |> args
                [
                  positional "FILE"
                  |> help "OCaml source file to lift"
                  |> required true;
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

  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "invalid CLI arguments")
  | Ok matches -> (
      match ArgParser.get_subcommand matches with
      | Some ("tokenize", sub_matches) ->
          handle_token_stream sub_matches;
          Ok ()
      | Some ("parse", sub_matches) ->
          handle_parse sub_matches;
          Ok ()
      | Some ("print-ceibo", sub_matches) ->
          handle_print_ceibo sub_matches;
          Ok ()
      | Some ("print-cst", sub_matches) ->
          handle_print_cst sub_matches;
          Ok ()
      | Some ("explain", sub_matches) ->
          handle_explain sub_matches;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          Error (Failure "missing subcommand"))

let () = Miniriot.run ~main ~args:Env.args ()
