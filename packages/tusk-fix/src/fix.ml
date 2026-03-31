open Std

type target = Fixme.Fix.target =
  | Node of Syn.Cst.syntax_node
  | Token of Syn.Cst.syntax_token

type replacement = Fixme.Fix.replacement =
  | Source_of_node of Syn.Cst.syntax_node
  | Source_of_token of Syn.Cst.syntax_token
  | Text of string

type operation = Fixme.Fix.operation =
  | Delete of {
      target: target;
    }
  | Replace of {
      target: target;
      replacement: replacement;
    }
  | Insert_before of {
      anchor: target;
      content: replacement;
    }
  | Insert_after of {
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

let apply_fix = Fixme.Fix.apply_fix

let apply_fixes = Fixme.Fix.apply_fixes

let validate_fix = Fixme.Fix.validate_fix

let target_to_json = function
  | Node node ->
      let span = Syn.Ceibo.Red.SyntaxNode.span node in
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
      let span = Syn.Ceibo.Red.SyntaxToken.span token in
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

let replacement_to_json = function
  | Source_of_node node ->
      let span = Syn.Ceibo.Red.SyntaxNode.span node in
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
  | Source_of_token token ->
      let span = Syn.Ceibo.Red.SyntaxToken.span token in
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
      Data.Json.Object [ ("kind", Data.Json.String "text"); ("text", Data.Json.String value);  ]

let operation_to_json = function
  | Delete { target } -> Data.Json.Object [
    ("kind", Data.Json.String "delete");
    ("target", target_to_json target);

  ]
  | Replace { target; replacement } -> Data.Json.Object [
    ("kind", Data.Json.String "replace");
    ("target", target_to_json target);
    ("replacement", replacement_to_json replacement);

  ]
  | Insert_before { anchor; content } -> Data.Json.Object [
    ("kind", Data.Json.String "insert_before");
    ("anchor", target_to_json anchor);
    ("content", replacement_to_json content);

  ]
  | Insert_after { anchor; content } -> Data.Json.Object [
    ("kind", Data.Json.String "insert_after");
    ("anchor", target_to_json anchor);
    ("content", replacement_to_json content);

  ]
  | Swap { left; right } -> Data.Json.Object [
    ("kind", Data.Json.String "swap");
    ("left", target_to_json left);
    ("right", target_to_json right);

  ]

let to_json = fun fix ->
    Data.Json.Object [
      ("title", Data.Json.String fix.title);
      ("operations", Data.Json.Array (List.map operation_to_json fix.operations));

    ]
