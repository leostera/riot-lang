open Std
open Std.Collections
open Syn

module Iterator = Iter.Iterator

let slice_of_file_contents = fun contents ->
  IO.IoVec.IoSlice.from_string contents
  |> Result.expect ~msg:"failed to create syn source slice"

let span_to_json = fun span ->
  Data.Json.Object [
    ("start", Data.Json.int span.Span.start);
    ("end", Data.Json.int span.Span.end_);
  ]

let span_text = fun source span ->
  let width = Syn.Span.width span in
  if width <= 0 then
    ""
  else
    String.sub source ~offset:span.Span.start ~len:width

let trivia_kind_to_json = fun __tmp1 ->
  match __tmp1 with
  | Syn.Token.CommentTrivia { terminated; _ } ->
      Data.Json.Object [
        ("kind", Data.Json.string "comment");
        ("terminated", Data.Json.bool terminated);
      ]
  | Syn.Token.DocstringTrivia { terminated; _ } ->
      Data.Json.Object [
        ("kind", Data.Json.string "docstring");
        ("terminated", Data.Json.bool terminated);
      ]
  | Syn.Token.WhitespaceTrivia -> Data.Json.Object [ ("kind", Data.Json.string "whitespace"); ]

let trivia_to_json = fun ~source (trivia: Syn.Token.trivia) ->
  match trivia_kind_to_json trivia.Syn.Token.kind with
  | Data.Json.Object fields ->
      Data.Json.Object ([
        ("span", span_to_json trivia.Syn.Token.span);
        ("text", Data.Json.string (span_text source trivia.Syn.Token.span));
      ]
      @ fields)
  | json -> json

let token_to_json = fun ~source (token: Syn.Token.t) ->
  Data.Json.Object [
    ("kind", Data.Json.string (Syn.Token.show_kind token.Syn.Token.kind));
    ("span", span_to_json token.Syn.Token.span);
    ("text", Data.Json.string (span_text source token.Syn.Token.span));
    (
      "leading_trivia",
      Data.Json.Array (List.map token.Syn.Token.leading_trivia ~fn:(trivia_to_json ~source))
    );
  ]

let vector_to_json = fun vector ~fn ->
  Data.Json.Array (
    Vector.iter vector
    |> Iterator.map ~fn
    |> Iterator.to_list
  )

let parse_result_to_json = fun result ->
  Data.Json.Object [
    ("kind", Data.Json.String (
      match result.Syn.Parser.kind with
      | `Implementation -> "implementation"
      | `Interface -> "interface"
    ));
    ("diagnostics", vector_to_json result.Syn.Parser.diagnostics ~fn:Syn.Diagnostic.to_json);
    ("tree", Syn.SyntaxTree.to_json result.Syn.Parser.tree);
  ]

let diagnostics_to_list = fun diagnostics ->
  Vector.iter diagnostics
  |> Iterator.to_list

let handle_token_stream = fun sub_matches ->
  let file =
    ArgParser.get_one sub_matches "FILE"
    |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      System.exit 1
  | Ok content ->
      let tokens = Syn.Lexer.tokenize (slice_of_file_contents content) in
      if json then
        let json_tokens = List.map tokens ~fn:(token_to_json ~source:content) in
        println (Data.Json.to_string (Data.Json.array json_tokens))
      else
        List.for_each tokens ~fn:(fun tok -> println (Syn.Token.show_kind tok.Syn.Token.kind))

let handle_parse = fun sub_matches ->
  let file =
    ArgParser.get_one sub_matches "FILE"
    |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      System.exit 1
  | Ok source ->
      let result = Syn.Parser.parse ~filename:(Path.v file) (slice_of_file_contents source) in
      if json then
        println (Data.Json.to_string (parse_result_to_json result))
      else if Vector.length result.Syn.Parser.diagnostics != 0 then
        Syn.DiagnosticReporter.print
          ~file
          ~source
          (diagnostics_to_list result.Syn.Parser.diagnostics)
      else
        let root = Syn.SyntaxTree.root result.Syn.Parser.tree in
        Log.info ("Parsed successfully: " ^ Int.to_string root.Syn.SyntaxTree.full_width ^ " bytes")

let parse_error_to_json = fun (Syn.Deps.Parse_diagnostics diagnostics) ->
  Data.Json.Object [
    ("error", Data.Json.string "parse_diagnostics");
    ("diagnostics", Data.Json.Array (List.map diagnostics ~fn:Syn.Diagnostic.to_json));
  ]

let handle_deps = fun sub_matches ->
  let file =
    ArgParser.get_one sub_matches "FILE"
    |> Option.expect ~msg:"FILE required"
  in
  let json = ArgParser.get_flag sub_matches "json" in
  match Fs.read (Path.v file) with
  | Error _err ->
      Log.error ("Error reading file " ^ file);
      System.exit 1
  | Ok source ->
      let result = Syn.Parser.parse ~filename:(Path.v file) (slice_of_file_contents source) in
      match Syn.Deps.from_parse_result result with
      | Ok deps ->
          if json then
            println (Data.Json.to_string (Syn.Deps.to_json deps))
          else
            List.for_each (Syn.Deps.modules deps) ~fn:println
      | Error error ->
          if json then
            println (Data.Json.to_string (parse_error_to_json error))
          else
            (
              match error with
              | Syn.Deps.Parse_diagnostics diagnostics ->
                  Syn.DiagnosticReporter.print ~file ~source diagnostics
            );
          System.exit 1

let handle_explain = fun sub_matches ->
  let error_code =
    ArgParser.get_one sub_matches "ERROR_CODE"
    |> Option.expect ~msg:"ERROR_CODE required"
  in
  match Syn.Error.id_of_string error_code with
  | Some id -> println (Syn.Error.explain id ^ "\n")
  | None ->
      Log.error ("Unknown error code: " ^ error_code);
      System.exit 1

let main ~args =
  let cmd =
    let open ArgParser in
    let open ArgParser.Arg in
    command "syn"
    |> version "0.1.0"
    |> about "OCaml syntax analysis tool"
    |> subcommands
      [
        command "tokenize"
        |> about "Print token stream for a file"
        |> args
          [
            positional "FILE"
            |> help "OCaml source file to tokenize"
            |> required true;
            flag "json"
            |> long "json"
            |> help "Output in JSON format";
          ];
        command "parse"
        |> about "Parse file with the streaming parser"
        |> args
          [
            positional "FILE"
            |> help "OCaml source file to parse"
            |> required true;
            flag "json"
            |> long "json"
            |> help "Output syntax tree as JSON";
          ];
        command "deps"
        |> about "Print syntactic module dependencies for a file"
        |> args
          [
            positional "FILE"
            |> help "OCaml source file to inspect"
            |> required true;
            flag "json"
            |> long "json"
            |> help "Output dependencies as JSON";
          ];
        command "explain"
        |> about "Explain an error code"
        |> args
          [
            positional "ERROR_CODE"
            |> help "Error code to explain (e.g. E0001)"
            |> required true;
          ];
      ]
  in
  match ArgParser.get_matches cmd args with
  | Error err ->
      ArgParser.print_error err;
      ArgParser.print_help cmd;
      Error (Failure "invalid CLI arguments")
  | Ok matches ->
      match ArgParser.get_subcommand matches with
      | Some ("tokenize", sub_matches) ->
          handle_token_stream sub_matches;
          Ok ()
      | Some ("parse", sub_matches) ->
          handle_parse sub_matches;
          Ok ()
      | Some ("deps", sub_matches) ->
          handle_deps sub_matches;
          Ok ()
      | Some ("explain", sub_matches) ->
          handle_explain sub_matches;
          Ok ()
      | _ ->
          ArgParser.print_help cmd;
          Error (Failure "missing subcommand")

let () = Runtime.run ~main ~args:Env.args ()
