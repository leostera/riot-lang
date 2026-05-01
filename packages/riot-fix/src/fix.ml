open Std

type target = Fixme.Fix.target =
  | Node of Syn.Ast.Node.t
  | Token of Syn.Ast.Token.t

type replacement = Fixme.Fix.replacement =
  | SourceOfNode of Syn.Ast.Node.t
  | SourceOfToken of Syn.Ast.Token.t
  | Text of string

type operation = Fixme.Fix.operation =
  | Delete of {
      target: target;
    }
  | Replace of {
      target: target;
      replacement: replacement;
    }
  | InsertBefore of {
      anchor: target;
      content: replacement;
    }
  | InsertAfter of {
      anchor: target;
      content: replacement;
    }
  | Swap of {
      left: target;
      right: target;
    }

type fix = Fixme.Fix.fix = {
  title: string;
  operations: operation list;
}

type text_edit = Fixme.Fix.text_edit = {
  span: Syn.Span.t;
  new_text: string;
}

let source_of_node = Fixme.Fix.source_of_node

let source_of_token = Fixme.Fix.source_of_token

let text = Fixme.Fix.text

let delete = Fixme.Fix.delete

let delete_node = Fixme.Fix.delete_node

let replace = Fixme.Fix.replace

let replace_node = Fixme.Fix.replace_node

let replace_node_with_text = Fixme.Fix.replace_node_with_text

let replace_token_with_text = Fixme.Fix.replace_token_with_text

let insert_before = Fixme.Fix.insert_before

let insert_after = Fixme.Fix.insert_after

let swap = Fixme.Fix.swap

let make = Fixme.Fix.make

let title = Fixme.Fix.title

let operations = Fixme.Fix.operations

let apply_operation = Fixme.Fix.apply_operation

let lower_fix = Fixme.Fix.lower_fix

let lower_fixes = Fixme.Fix.lower_fixes

let apply_fix = Fixme.Fix.apply_fix

let apply_fixes = Fixme.Fix.apply_fixes

let validate_fix = Fixme.Fix.validate_fix

let span_of_node = fun node ->
  Syn.Span.make
    ~start:(Syn.Ast.Node.span_start node)
    ~end_:(Syn.Ast.Node.span_end node)

let span_of_token = fun token ->
  Syn.Span.make
    ~start:(Syn.Ast.Token.span_start token)
    ~end_:(Syn.Ast.Token.span_end token)

let target_to_json target =
  match target with
  | Node node ->
      let span = span_of_node node in
      Data.Json.Object [
        ("kind", Data.Json.String "node");
        (
          "span",
          Data.Json.Object [
            ("start", Data.Json.Int span.start);
            ("end", Data.Json.Int span.end_);
          ]
        );
      ]
  | Token token ->
      let span = span_of_token token in
      Data.Json.Object [
        ("kind", Data.Json.String "token");
        (
          "span",
          Data.Json.Object [
            ("start", Data.Json.Int span.start);
            ("end", Data.Json.Int span.end_);
          ]
        );
      ]

let replacement_to_json replacement =
  match replacement with
  | SourceOfNode node ->
      let span = span_of_node node in
      Data.Json.Object [
        ("kind", Data.Json.String "source_of_node");
        (
          "span",
          Data.Json.Object [
            ("start", Data.Json.Int span.start);
            ("end", Data.Json.Int span.end_);
          ]
        );
      ]
  | SourceOfToken token ->
      let span = span_of_token token in
      Data.Json.Object [
        ("kind", Data.Json.String "source_of_token");
        (
          "span",
          Data.Json.Object [
            ("start", Data.Json.Int span.start);
            ("end", Data.Json.Int span.end_);
          ]
        );
      ]
  | Text value ->
      Data.Json.Object [ ("kind", Data.Json.String "text"); ("text", Data.Json.String value); ]

let operation_to_json operation =
  match operation with
  | Delete { target } ->
      Data.Json.Object [ ("kind", Data.Json.String "delete"); ("target", target_to_json target); ]
  | Replace { target; replacement } ->
      Data.Json.Object [
        ("kind", Data.Json.String "replace");
        ("target", target_to_json target);
        ("replacement", replacement_to_json replacement);
      ]
  | InsertBefore { anchor; content } ->
      Data.Json.Object [
        ("kind", Data.Json.String "insert_before");
        ("anchor", target_to_json anchor);
        ("content", replacement_to_json content);
      ]
  | InsertAfter { anchor; content } ->
      Data.Json.Object [
        ("kind", Data.Json.String "insert_after");
        ("anchor", target_to_json anchor);
        ("content", replacement_to_json content);
      ]
  | Swap { left; right } ->
      Data.Json.Object [
        ("kind", Data.Json.String "swap");
        ("left", target_to_json left);
        ("right", target_to_json right);
      ]

let to_json = fun fix ->
  Data.Json.Object [
    ("title", Data.Json.String fix.title);
    ("operations", Data.Json.Array (List.map fix.operations ~fn:operation_to_json));
  ]
