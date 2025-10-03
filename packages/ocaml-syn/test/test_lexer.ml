open Std

let test_code =
  {|
let x = 42

let add a b = a + b

type t = int

module M = struct
  let y = true
end
|}

let () =
  println "=== Testing Lexer ===\n";

  let tokens = Ocaml_syn.tokenize test_code in
  println "Tokens:";
  List.iter
    (fun tok ->
      match tok with
      | Ocaml_syn.Token.Keyword k -> println "  Keyword"
      | Ocaml_syn.Token.Ident s -> println "  Ident: %s" s
      | Ocaml_syn.Token.Literal (Int i) -> println "  Int: %d" i
      | Ocaml_syn.Token.Eq -> println "  Eq"
      | Ocaml_syn.Token.Whitespace -> ()
      | Ocaml_syn.Token.EOF -> println "  EOF"
      | _ -> println "  Other")
    tokens;

  println "\n=== Testing TokenTrees ===\n";

  let trees = Ocaml_syn.parse_token_trees test_code in
  println "%s" (Ocaml_syn.TokenTree.list_to_string trees);

  println "\nDone!"
