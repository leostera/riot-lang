open Std
open Std.Data
open Std.Collections
open Datalog

let read_file path =
  match Fs.read (Path.v path) with
  | Ok content -> content
  | Error _ -> panic ("Failed to read file: " ^ path)

let ast_to_json node =
  let rec get_token_text = function
    | Ceibo.Green.Token tok -> tok.text
    | Ceibo.Green.Node _ -> ""
  in

  let rec skip_trivia elements =
    Array.to_list elements
    |> List.filter (function
      | Ceibo.Green.Token tok ->
          tok.kind != Parser.Syntax_kind.WHITESPACE
          && tok.kind != Parser.Syntax_kind.COMMENT
      | _ -> true)
  in

  let rec convert_term = function
    | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.STRING_LITERAL
      ->
        let text =
          node.children |> Array.to_list |> List.map get_token_text
          |> String.concat ""
        in
        let unquoted =
          if
            String.length text >= 2
            && String.get text 0 = '"'
            && String.get text (String.length text - 1) = '"'
          then String.sub text 1 (String.length text - 2)
          else text
        in
        Json.Object
          [ ("type", Json.String "string"); ("value", Json.String unquoted) ]
    | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.INT_LITERAL -> (
        let text =
          node.children |> Array.to_list |> List.map get_token_text
          |> String.concat ""
        in
        match int_of_string_opt text with
        | Some i ->
            Json.Object
              [ ("type", Json.String "integer"); ("value", Json.Int i) ]
        | None ->
            Json.Object
              [ ("type", Json.String "integer"); ("value", Json.String text) ])
    | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.VARIABLE ->
        let name =
          node.children |> Array.to_list |> List.map get_token_text
          |> String.concat ""
        in
        Json.Object
          [ ("type", Json.String "variable"); ("name", Json.String name) ]
    | Ceibo.Green.Node node when node.kind = Parser.Syntax_kind.WILDCARD ->
        Json.Object [ ("type", Json.String "wildcard") ]
    | other ->
        Json.Object
          [
            ("type", Json.String "unknown");
            ("debug", Json.String (get_token_text other));
          ]
  in

  let rec convert_atom node =
    let children = skip_trivia node.Ceibo.Green.children in
    match children with
    | Ceibo.Green.Token pred_tok :: Ceibo.Green.Token _ :: args_and_paren ->
        let predicate = pred_tok.Ceibo.Green.text in
        let args =
          List.filter_map
            (function
              | Ceibo.Green.Node n
                when n.Ceibo.Green.kind = Parser.Syntax_kind.STRING_LITERAL
                     || n.kind = Parser.Syntax_kind.INT_LITERAL
                     || n.kind = Parser.Syntax_kind.VARIABLE
                     || n.kind = Parser.Syntax_kind.WILDCARD ->
                  Some (convert_term (Ceibo.Green.Node n))
              | _ -> None)
            args_and_paren
        in
        (predicate, args)
    | _ -> ("", [])
  in

  let convert_atom_to_json atom_node =
    let pred, args = convert_atom atom_node in
    Json.Object
      [
        ("type", Json.String "atom");
        ("predicate", Json.String pred);
        ("args", Json.Array args);
      ]
  in

  let convert_negated_atom_to_json node =
    let inner_children = skip_trivia node.Ceibo.Green.children in
    let atom_opt =
      List.find_map
        (function
          | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n) ->
              Some n
          | _ -> None)
        inner_children
    in
    match atom_opt with
    | Some atom ->
        let pred, args = convert_atom atom in
        Json.Object
          [
            ("type", Json.String "negated_atom");
            ("predicate", Json.String pred);
            ("args", Json.Array args);
          ]
    | None -> Json.Object [ ("type", Json.String "negated_atom") ]
  in

  let rec convert_builtin_to_json node =
    let inner_children = skip_trivia node.Ceibo.Green.children in

    let inner_builtin =
      List.find_map
        (function
          | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as n) ->
              Some n
          | _ -> None)
        inner_children
    in

    match inner_builtin with
    | Some inner -> convert_builtin_to_json inner
    | None -> (
        let op_opt =
          List.find_map
            (function
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.GT; _ } ->
                  Some ">"
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.LT; _ } ->
                  Some "<"
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.GTEQ; _ } ->
                  Some ">="
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.LTEQ; _ } ->
                  Some "<="
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.EQ; _ } ->
                  Some "="
              | Ceibo.Green.Token { kind = Parser.Syntax_kind.NOTEQ; _ } ->
                  Some "!="
              | _ -> None)
            inner_children
        in
        let args =
          List.filter_map
            (function
              | Ceibo.Green.Node n
                when n.Ceibo.Green.kind = Parser.Syntax_kind.STRING_LITERAL
                     || n.kind = Parser.Syntax_kind.INT_LITERAL
                     || n.kind = Parser.Syntax_kind.VARIABLE
                     || n.kind = Parser.Syntax_kind.WILDCARD ->
                  Some (convert_term (Ceibo.Green.Node n))
              | _ -> None)
            inner_children
        in
        match op_opt with
        | Some op ->
            Json.Object
              [
                ("type", Json.String "builtin");
                ("op", Json.String op);
                ("args", Json.Array args);
              ]
        | None -> Json.Object [ ("type", Json.String "builtin") ])
  in

  let convert_comment tok =
    Json.Object
      [
        ("type", Json.String "comment");
        ("text", Json.String tok.Ceibo.Green.text);
      ]
  in

  let convert_item = function
    | Ceibo.Green.Token ({ kind = Parser.Syntax_kind.COMMENT; _ } as tok) ->
        Some (convert_comment tok)
    | Ceibo.Green.Node
        ({ kind = Parser.Syntax_kind.FACT; children; _ } as _node) -> (
        let children = skip_trivia children in
        match children with
        | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as atom) :: _
          ->
            let predicate, args = convert_atom atom in
            Some
              (Json.Object
                 [
                   ("type", Json.String "fact");
                   ("predicate", Json.String predicate);
                   ("args", Json.Array args);
                 ])
        | _ -> None)
    | Ceibo.Green.Node { kind = Parser.Syntax_kind.RULE; children; _ } -> (
        let all_children = Array.to_list children in
        let children_no_trivia = skip_trivia children in

        let rec split_at_colon_dash acc = function
          | [] -> (List.rev acc, [])
          | Ceibo.Green.Token { kind = Parser.Syntax_kind.COLON_DASH; _ }
            :: rest ->
              (List.rev acc, rest)
          | x :: xs -> split_at_colon_dash (x :: acc) xs
        in

        let before_dash, after_dash = split_at_colon_dash [] all_children in
        let head_children = skip_trivia (Array.of_list before_dash) in
        let body_children = skip_trivia (Array.of_list after_dash) in

        let head_opt =
          List.find_map
            (function
              | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n) ->
                  Some n
              | _ -> None)
            head_children
        in

        let rec find_inner_atom node =
          let inner_children = skip_trivia node.Ceibo.Green.children in
          match
            List.find_map
              (function
                | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n)
                  ->
                    Some n
                | _ -> None)
              inner_children
          with
          | Some inner -> find_inner_atom inner
          | None -> node
        in

        let body_items =
          List.filter_map
            (function
              | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.ATOM; _ } as n) ->
                  let actual_atom = find_inner_atom n in
                  Some (convert_atom_to_json actual_atom)
              | Ceibo.Green.Node
                  ({ kind = Parser.Syntax_kind.NEGATED_ATOM; _ } as n) ->
                  Some (convert_negated_atom_to_json n)
              | Ceibo.Green.Node ({ kind = Parser.Syntax_kind.BUILTIN; _ } as n)
                ->
                  Some (convert_builtin_to_json n)
              | _ -> None)
            body_children
        in

        match head_opt with
        | Some head ->
            let head_pred, head_args = convert_atom head in
            Some
              (Json.Object
                 [
                   ("type", Json.String "rule");
                   ( "head",
                     Json.Object
                       [
                         ("predicate", Json.String head_pred);
                         ("args", Json.Array head_args);
                       ] );
                   ("body", Json.Array body_items);
                 ])
        | None -> Some (Json.Object [ ("type", Json.String "rule") ]))
    | _ -> None
  in

  let items =
    Array.to_list node.Ceibo.Green.children
    |> List.filter (function
      | Ceibo.Green.Token { kind = Parser.Syntax_kind.WHITESPACE; _ } -> false
      | _ -> true)
    |> List.filter_map convert_item
  in

  Json.Object [ ("type", Json.String "program"); ("items", Json.Array items) ]

let normalize_json json =
  let rec normalize = function
    | Json.Object fields ->
        Json.Object
          (List.sort
             (fun (k1, _) (k2, _) -> String.compare k1 k2)
             (List.map (fun (k, v) -> (k, normalize v)) fields))
    | Json.Array items -> Json.Array (List.map normalize items)
    | other -> other
  in
  normalize json

let test_valid_parse (name, datalog_file, expected_file) =
  Test.case ("Valid: " ^ name) (fun () ->
      let input = read_file datalog_file in
      let expected_json = read_file expected_file in

      match Parser.parse input with
      | Error diagnostics ->
          let error_msgs =
            List.map (fun d -> d.Parser.Diagnostic.message) diagnostics
          in
          Error
            ("Parse failed with errors: " ^
               String.concat "; " error_msgs)
      | Ok tree -> (
          let actual_json = ast_to_json tree in
          let actual_str = Json.to_string actual_json in

          match Json.of_string expected_json with
          | Error _ -> Error "Failed to parse expected JSON"
          | Ok expected ->
              let normalized_actual = normalize_json actual_json in
              let normalized_expected = normalize_json expected in

              if normalized_actual = normalized_expected then Ok ()
              else
                Error
                  ("AST mismatch:\nExpected: " ^
                     Json.to_string normalized_expected ^
                     "\nActual: " ^
                     Json.to_string normalized_actual)))

let diagnostic_to_json d =
  Json.Object
    [
      ("type", Json.String "syntax_error");
      ("message", Json.String d.Parser.Diagnostic.message);
      ( "span",
        Json.Object
          [ ("start", Json.Int d.span.start); ("end", Json.Int d.span.end_) ] );
      ("severity", Json.String "error");
    ]

let test_invalid_parse (name, datalog_file, error_file) =
  Test.case ("Invalid: " ^ name) (fun () ->
      try
        let input = read_file datalog_file in
        let expected_json = read_file error_file in

        match Parser.parse input with
        | Ok _tree -> Error "Expected parse to fail, but it succeeded"
        | Error diagnostics -> (
            let actual_json =
              Json.Object
                [
                  ( "errors",
                    Json.Array (List.map diagnostic_to_json diagnostics) );
                ]
            in

            match Json.of_string expected_json with
            | Error _ -> Error "Failed to parse expected JSON"
            | Ok expected ->
                let normalized_actual = normalize_json actual_json in
                let normalized_expected = normalize_json expected in

                if normalized_actual = normalized_expected then Ok ()
                else
                  Error
                    ("Error mismatch:\nExpected: " ^
                       Json.to_string normalized_expected ^
                       "\nActual: " ^
                       Json.to_string normalized_actual))
      with exn -> Error "Exception occurred during parsing")

let load_fixtures base_path input_suffix expected_suffix =
  let fixtures_path = Path.v base_path in
  match Fs.read_dir fixtures_path with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      let fixtures =
        List.filter_map
          (fun path ->
            let name = Path.basename path in
            if String.ends_with ~suffix:input_suffix name then
              let base =
                String.sub name 0
                  (String.length name - String.length input_suffix)
              in
              let input_file = base_path ^ "/" ^ name in
              let expected_file =
                base_path ^ "/" ^ base ^ expected_suffix
              in
              match Fs.exists (Path.v expected_file) with
              | Ok true -> Some (base, input_file, expected_file)
              | _ -> None
            else None)
          entries
      in
      List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) fixtures

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let valid_fixtures =
        load_fixtures "packages/datalog/tests/parser/fixtures/valid" ".datalog"
          ".ast.json"
      in
      let invalid_fixtures =
        load_fixtures "packages/datalog/tests/parser/fixtures/invalid"
          ".datalog" ".error.json"
      in

      let valid_tests = List.map test_valid_parse valid_fixtures in
      let invalid_tests = List.map test_invalid_parse invalid_fixtures in

      let all_tests = valid_tests @ invalid_tests in
      let all_tests = [] in

      Test.Cli.main ~name:"datalog-parser" ~tests:all_tests ~args)
    ~args:Env.args ()
