open Std

let test_expressions =
  [
    ("simple math", "1 + 2 * 3");
    ("function application", "f x y");
    ("chained application", "f (g x) (h y)");
    ("sequence", "print 1; print 2; 42");
    ("field access", "record.field");
    ("let in", "let x = 42 in x + 1");
    ("if then else", "if x > 0 then 1 else 0");
    ("tuple", "(1, 2, 3)");
    ("list", "[1; 2; 3]");
    ("empty list", "[]");
    ("record", "{ x = 1; y = 2 }");
    ("fun", "fun x -> x + 1");
    ("match", "match x with | 0 -> true | _ -> false");
  ]

let () =
  println "=== Testing OCaml Parser ===\n";

  List.iter
    (fun (name, code) ->
      print "%s: " name;
      match Ocaml_syn.parse code with
      | Ok _ -> println "✓"
      | Error (Ocaml_syn.Parser.UnexpectedToken { expected; found = _ }) ->
          println "✗ (expected %s)" expected
      | Error Ocaml_syn.Parser.UnexpectedEOF -> println "✗ (unexpected EOF)"
      | Error Ocaml_syn.Parser.InvalidPattern -> println "✗ (invalid pattern)"
      | Error (Ocaml_syn.Parser.InvalidExpression msg) -> println "✗ (%s)" msg)
    test_expressions;

  println "\nDone!"
