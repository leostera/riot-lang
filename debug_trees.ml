open Std

let rec indent_string n =
  String.make (n * 2) ' '

let rec print_tree indent = function
  | Syn.TokenTree.Token tok ->
      Printf.printf "%sToken: %s\n" (indent_string indent) 
        (match tok with
         | Syn.Token.Keyword kw ->
             Printf.sprintf "Keyword(%s)" 
               (match kw with
                | Syn.Token.Let -> "let"
                | Syn.Token.Type -> "type"
                | Syn.Token.Module -> "module"
                | Syn.Token.Struct -> "struct"
                | Syn.Token.End -> "end"
                | _ -> "...")
         | Syn.Token.Ident s -> Printf.sprintf "Ident(%s)" s
         | Syn.Token.Literal (Syn.Token.Int i) -> Printf.sprintf "Int(%d)" i
         | Syn.Token.Comment { value; _ } -> Printf.sprintf "Comment(%s)" value
         | Syn.Token.Eq -> "Eq(=)"
         | Syn.Token.Whitespace -> "Whitespace"
         | Syn.Token.Semi -> "Semi(;)"
         | Syn.Token.OpenDelim d -> 
             Printf.sprintf "OpenDelim(%s)" 
               (match d with
                | Syn.Token.Paren -> "("
                | Syn.Token.Bracket -> "["
                | Syn.Token.Brace -> "{"
                | _ -> "?")
         | Syn.Token.CloseDelim d ->
             Printf.sprintf "CloseDelim(%s)"
               (match d with  
                | Syn.Token.Paren -> ")"
                | Syn.Token.Bracket -> "]"
                | Syn.Token.Brace -> "}"
                | _ -> "?")
         | _ -> "...")
  | Syn.TokenTree.Tree (delim, children) ->
      Printf.printf "%sTree(%s) [\n" (indent_string indent)
        (match delim with
         | Syn.Token.BeginEnd -> "BeginEnd"
         | Syn.Token.Paren -> "Paren"
         | Syn.Token.Bracket -> "Bracket"
         | Syn.Token.Brace -> "Brace"
         | Syn.Token.StructEnd -> "StructEnd"
         | Syn.Token.SigEnd -> "SigEnd"
         | Syn.Token.ObjectEnd -> "ObjectEnd");
      List.iter (print_tree (indent + 1)) children;
      Printf.printf "%s]\n" (indent_string indent)

let () =
  let content = Fs.read (Path.v "test_debug.ml") |> Result.unwrap in
  let tokens = Syn.tokenize content in
  let trees = Syn.TokenTree.of_tokens tokens in
  
  Printf.printf "=== TOKEN TREES ===\n";
  Printf.printf "Total trees: %d\n\n" (List.length trees);
  
  List.iteri (fun i tree ->
    Printf.printf "Tree #%d:\n" i;
    print_tree 0 tree;
    Printf.printf "\n"
  ) trees