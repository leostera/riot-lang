open Std
open Std.Collections

let trim_trailing_newlines text =
  let rec loop index =
    if index <= 0 then
      ""
    else
      match text.[index - 1] with
      | '\n' | '\r' ->
          loop (index - 1)
      | _ ->
          String.sub text 0 index
  in
  loop (String.length text)

let source_of_syntax_node (node : Syn.Cst.syntax_node) =
  let buffer = IO.Buffer.create 1024 in
  let append_token ?(include_leading_trivia = true) token =
    if include_leading_trivia then
      Syn.Ceibo.Red.SyntaxToken.leading_trivia token
      |> List.iter (fun trivia ->
             IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxTrivia.text trivia));
    IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxToken.text token)
  in
  match Syn.Ceibo.Red.SyntaxNode.tokens node with
  | [] ->
      ""
  | first :: rest ->
      append_token ~include_leading_trivia:false first;
      List.iter append_token rest;
      IO.Buffer.contents buffer |> trim_trailing_newlines
