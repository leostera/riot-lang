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

let syntax_node_has_comment_like_trivia (node : Syn.Cst.syntax_node) =
  let found = ref false in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token ->
        if
          Syn.Ceibo.Red.SyntaxToken.leading_trivia token
          |> List.exists (fun trivia ->
                 match Syn.Ceibo.Red.SyntaxTrivia.kind trivia with
                 | Syn.SyntaxKind.COMMENT
                 | Syn.SyntaxKind.DOCSTRING ->
                     true
                 | _ ->
                     false)
        then
          found := true
    | Syn.Ceibo.Red.Node _ ->
        ());
  !found

let source_of_span source (span : Syn.Ceibo.Span.t) =
  let source_length = String.length source in
  let start =
    if span.start < 0 then
      0
    else if span.start > source_length then
      source_length
    else
      span.start
  in
  let end_ =
    if span.end_ < start then
      start
    else if span.end_ > source_length then
      source_length
    else
      span.end_
  in
  if end_ <= start then
    ""
  else
    String.sub source start (end_ - start)

let source_between source ~start ~end_ =
  source_of_span source (Syn.Ceibo.Span.make ~start ~end_)
