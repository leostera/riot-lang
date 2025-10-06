open Std

type t = Token of Token.t | Tree of Token.delimiter * t list

let rec of_tokens tokens =
  (* Group tokens into top-level definitions *)
  let rec parse_top_level acc tokens =
    match tokens with
    | [] -> List.rev acc
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> parse_top_level acc rest
        | Token.Whitespace ->
            (* Skip top-level whitespace *)
            parse_top_level acc rest
        | Token.Comment _ | Token.Docstring _ ->
            (* Comments start accumulating for the next definition *)
            collect_definition acc [Token tok] rest
        | Token.Keyword Token.Let ->
            parse_let acc [Token tok] rest
        | Token.Keyword Token.Type ->
            parse_type acc [Token tok] rest
        | Token.Keyword Token.Module ->
            parse_module acc [Token tok] rest
        | Token.Keyword Token.Open ->
            parse_simple acc [Token tok] rest
        | Token.OpenDelim delim ->
            let tree, remaining = parse_delimited delim rest in
            parse_top_level ((Tree (Token.BeginEnd, [tree])) :: acc) remaining
        | _ ->
            (* Other top-level tokens - shouldn't normally happen *)
            parse_top_level ((Tree (Token.BeginEnd, [Token tok])) :: acc) rest)

  and collect_definition acc current tokens =
    (* Collect comments/whitespace until we hit a definition *)
    match tokens with
    | [] -> 
        if current = [] then List.rev acc
        else List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> 
            collect_definition acc current rest
        | Token.Whitespace ->
            collect_definition acc (Token tok :: current) rest
        | Token.Comment _ | Token.Docstring _ ->
            collect_definition acc (Token tok :: current) rest
        | Token.Keyword Token.Let ->
            (* Start let with accumulated comments *)
            parse_let acc (Token tok :: current) rest
        | Token.Keyword Token.Type ->
            parse_type acc (Token tok :: current) rest
        | Token.Keyword Token.Module ->
            parse_module acc (Token tok :: current) rest
        | Token.Keyword Token.Open ->
            parse_simple acc (Token tok :: current) rest
        | _ ->
            (* Non-keyword after comments - treat as simple definition *)
            parse_simple acc (Token tok :: current) rest)
        
  and parse_let acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> parse_let acc current rest
        | Token.Whitespace ->
            parse_let acc (Token tok :: current) rest
        | Token.Comment _ | Token.Docstring _ ->
            (* Comment after let - finish this let and start collecting for next *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            collect_definition acc' [Token tok] rest
        | Token.Keyword Token.Let ->
            (* New let starts - finish this one *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_let acc' [Token tok] rest
        | Token.Keyword Token.Type ->
            (* New type starts - finish this let *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_type acc' [Token tok] rest
        | Token.Keyword Token.Module ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_module acc' [Token tok] rest
        | Token.Keyword Token.Open ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_simple acc' [Token tok] rest
        | Token.OpenDelim delim ->
            let tree, remaining = parse_delimited delim rest in
            parse_let acc (tree :: current) remaining
        | _ ->
            parse_let acc (Token tok :: current) rest)
        
  and parse_type acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> parse_type acc current rest
        | Token.Whitespace ->
            parse_type acc (Token tok :: current) rest
        | Token.Comment _ | Token.Docstring _ ->
            (* Comment after type - finish this type and start collecting for next *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            collect_definition acc' [Token tok] rest
        | Token.Keyword Token.Let ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_let acc' [Token tok] rest
        | Token.Keyword Token.Type -> (
            (* Check if we have 'and' in current - continue type, else start new *)
            let has_and = List.exists (function 
              | Token t -> (match t.Token.kind with Token.Keyword Token.And -> true | _ -> false)
              | _ -> false) current in
            if has_and then
              (* Continue with 'and' type *)
              parse_type acc (Token tok :: current) rest
            else
              (* New type starts *)
              let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
              parse_type acc' [Token tok] rest)
        | Token.Keyword Token.Module ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_module acc' [Token tok] rest
        | Token.Keyword Token.Open ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_simple acc' [Token tok] rest
        | Token.OpenDelim delim ->
            let tree, remaining = parse_delimited delim rest in
            parse_type acc (tree :: current) remaining
        | _ ->
            parse_type acc (Token tok :: current) rest)
        
  and parse_module acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> parse_module acc current rest
        | Token.Whitespace ->
            parse_module acc (Token tok :: current) rest
        | Token.Comment _ | Token.Docstring _ ->
            (* Comment after module - finish this module and start collecting for next *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            collect_definition acc' [Token tok] rest
        | Token.Keyword Token.Let ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_let acc' [Token tok] rest
        | Token.Keyword Token.Type ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_type acc' [Token tok] rest
        | Token.Keyword Token.Module ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_module acc' [Token tok] rest
        | Token.Keyword Token.Open ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_simple acc' [Token tok] rest
        | Token.OpenDelim delim ->
            let tree, remaining = parse_delimited delim rest in
            parse_module acc (tree :: current) remaining
        | _ ->
            parse_module acc (Token tok :: current) rest)
        
  and parse_simple acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | tok :: rest -> (
        match tok.Token.kind with
        | Token.EOF -> parse_simple acc current rest
        | Token.Whitespace ->
            parse_simple acc (Token tok :: current) rest
        | Token.Comment _ | Token.Docstring _ ->
            (* Comment after simple - finish this and start collecting for next *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            collect_definition acc' [Token tok] rest
        | Token.Keyword Token.Let ->
            (* Start of new top-level item *)
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_let acc' [Token tok] rest
        | Token.Keyword Token.Type ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_type acc' [Token tok] rest
        | Token.Keyword Token.Module ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_module acc' [Token tok] rest
        | Token.Keyword Token.Open ->
            let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
            parse_simple acc' [Token tok] rest
        | _ ->
            parse_simple acc (Token tok :: current) rest)
        
  and parse_delimited delim tokens =
    let rec parse_stream acc tokens =
      match tokens with
      | [] -> (List.rev acc, [])
      | tok :: rest -> (
          match tok.Token.kind with
          | Token.EOF -> (List.rev acc, rest)
          | Token.CloseDelim d when d = delim -> (List.rev acc, rest)
          | Token.OpenDelim inner_delim ->
              let tree, remaining = parse_delimited inner_delim rest in
              parse_stream (tree :: acc) remaining
          | _ -> parse_stream (Token tok :: acc) rest)
    in
    let contents, remaining = parse_stream [] tokens in
    (Tree (delim, contents), remaining)
  in
  parse_top_level [] tokens

let delimiter_to_string = function
  | Token.Paren -> "Paren"
  | Token.Brace -> "Brace"
  | Token.Bracket -> "Bracket"
  | Token.BeginEnd -> "BeginEnd"
  | Token.StructEnd -> "StructEnd"
  | Token.SigEnd -> "SigEnd"
  | Token.ObjectEnd -> "ObjectEnd"

let rec to_string = function
  | Token _tok -> "Token(...)"  (* Simple placeholder since Token doesn't have to_string *)
  | Tree (delim, children) ->
      let delim_str = delimiter_to_string delim in
      let children_str =
        children |> List.map to_string |> String.concat ", "
      in
      Printf.sprintf "Tree(%s, [%s])" delim_str children_str

let list_to_string trees =
  trees |> List.map to_string |> String.concat "\n"