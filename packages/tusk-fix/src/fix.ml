open Std

type target = Tusk_fix_api.Fix.target =
  | Node of Syn.Cst.syntax_node
  | Token of Syn.Cst.syntax_token

type replacement = Tusk_fix_api.Fix.replacement =
  | Source_of_node of Syn.Cst.syntax_node
  | Source_of_token of Syn.Cst.syntax_token
  | Text of string

type operation = Tusk_fix_api.Fix.operation =
  | Delete of {
      target : target;
    }
  | Replace of {
      target : target;
      replacement : replacement;
    }
  | Insert_before of {
      anchor : target;
      content : replacement;
    }
  | Insert_after of {
      anchor : target;
      content : replacement;
    }
  | Swap of {
      left : target;
      right : target;
    }

type fix = Tusk_fix_api.Fix.fix = {
  title : string;
  operations : operation list;
}

let source_of_node = Tusk_fix_api.Fix.source_of_node
let source_of_token = Tusk_fix_api.Fix.source_of_token
let text = Tusk_fix_api.Fix.text
let delete = Tusk_fix_api.Fix.delete
let delete_node = Tusk_fix_api.Fix.delete_node
let replace = Tusk_fix_api.Fix.replace
let replace_node = Tusk_fix_api.Fix.replace_node
let replace_node_with_text = Tusk_fix_api.Fix.replace_node_with_text
let replace_token_with_text = Tusk_fix_api.Fix.replace_token_with_text
let insert_before = Tusk_fix_api.Fix.insert_before
let insert_after = Tusk_fix_api.Fix.insert_after
let swap = Tusk_fix_api.Fix.swap
let make = Tusk_fix_api.Fix.make
let title = Tusk_fix_api.Fix.title
let operations = Tusk_fix_api.Fix.operations
let apply_operation = Tusk_fix_api.Fix.apply_operation
let apply_fix = Tusk_fix_api.Fix.apply_fix
let apply_fixes = Tusk_fix_api.Fix.apply_fixes
let validate_fix = Tusk_fix_api.Fix.validate_fix

let target_to_json = function
  | Node node ->
      let span = Syn.Ceibo.Red.SyntaxNode.span node in
      Data.Json.Object
        [
          ("kind", Data.Json.String "node");
          ( "span",
            Data.Json.Object
              [
                ("start", Data.Json.Int span.start);
                ("end", Data.Json.Int span.end_);
              ] );
        ]
  | Token token ->
      let span = Syn.Ceibo.Red.SyntaxToken.span token in
      Data.Json.Object
        [
          ("kind", Data.Json.String "token");
          ( "span",
            Data.Json.Object
              [
                ("start", Data.Json.Int span.start);
                ("end", Data.Json.Int span.end_);
              ] );
        ]

let replacement_to_json = function
  | Source_of_node node ->
      let span = Syn.Ceibo.Red.SyntaxNode.span node in
      Data.Json.Object
        [
          ("kind", Data.Json.String "source_of_node");
          ( "span",
            Data.Json.Object
              [
                ("start", Data.Json.Int span.start);
                ("end", Data.Json.Int span.end_);
              ] );
        ]
  | Source_of_token token ->
      let span = Syn.Ceibo.Red.SyntaxToken.span token in
      Data.Json.Object
        [
          ("kind", Data.Json.String "source_of_token");
          ( "span",
            Data.Json.Object
              [
                ("start", Data.Json.Int span.start);
                ("end", Data.Json.Int span.end_);
              ] );
        ]
  | Text value ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "text");
          ("text", Data.Json.String value);
        ]

let operation_to_json = function
  | Delete { target } ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "delete");
          ("target", target_to_json target);
        ]
  | Replace { target; replacement } ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "replace");
          ("target", target_to_json target);
          ("replacement", replacement_to_json replacement);
        ]
  | Insert_before { anchor; content } ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "insert_before");
          ("anchor", target_to_json anchor);
          ("content", replacement_to_json content);
        ]
  | Insert_after { anchor; content } ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "insert_after");
          ("anchor", target_to_json anchor);
          ("content", replacement_to_json content);
        ]
  | Swap { left; right } ->
      Data.Json.Object
        [
          ("kind", Data.Json.String "swap");
          ("left", target_to_json left);
          ("right", target_to_json right);
        ]

let to_json fix =
  Data.Json.Object
    [
      ("title", Data.Json.String fix.title);
      ("operations", Data.Json.Array (List.map operation_to_json fix.operations));
    ]
