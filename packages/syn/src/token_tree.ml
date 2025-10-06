open Std

type t = Token of Token.t | Tree of Token.delimiter * t list

let rec of_tokens tokens =
  (* Group tokens into top-level definitions *)
  let rec parse_top_level acc tokens =
    match tokens with
    | [] -> List.rev acc
    | Token.EOF :: rest -> parse_top_level acc rest
    | Token.Whitespace :: rest ->
        (* Skip top-level whitespace *)
        parse_top_level acc rest
    | (Token.Comment _ | Token.Docstring _) as comment :: rest ->
        (* Comments start accumulating for the next definition *)
        collect_definition acc [Token comment] rest
    | (Token.Keyword Token.Let) :: rest ->
        parse_let acc [Token (Token.Keyword Token.Let)] rest
    | (Token.Keyword Token.Type) :: rest ->
        parse_type acc [Token (Token.Keyword Token.Type)] rest
    | (Token.Keyword Token.Module) :: rest ->
        parse_module acc [Token (Token.Keyword Token.Module)] rest
    | (Token.Keyword Token.Open) :: rest ->
        parse_simple acc [Token (Token.Keyword Token.Open)] rest
    | (Token.OpenDelim delim) :: rest ->
        let tree, remaining = parse_delimited delim rest in
        parse_top_level ((Tree (Token.BeginEnd, [tree])) :: acc) remaining
    | tok :: rest ->
        (* Other top-level tokens - shouldn't normally happen *)
        parse_top_level ((Tree (Token.BeginEnd, [Token tok])) :: acc) rest

  and collect_definition acc current tokens =
    (* Collect comments/whitespace until we hit a definition *)
    match tokens with
    | [] -> 
        if current = [] then List.rev acc
        else List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | Token.EOF :: rest -> 
        collect_definition acc current rest
    | Token.Whitespace :: rest ->
        collect_definition acc (Token Token.Whitespace :: current) rest
    | ((Token.Comment _ | Token.Docstring _) as tok) :: rest ->
        collect_definition acc (Token tok :: current) rest
    | (Token.Keyword Token.Let) :: rest ->
        (* Start let with accumulated comments *)
        parse_let acc ((Token (Token.Keyword Token.Let)) :: current) rest
    | (Token.Keyword Token.Type) :: rest ->
        parse_type acc ((Token (Token.Keyword Token.Type)) :: current) rest
    | (Token.Keyword Token.Module) :: rest ->
        parse_module acc ((Token (Token.Keyword Token.Module)) :: current) rest
    | (Token.Keyword Token.Open) :: rest ->
        parse_simple acc ((Token (Token.Keyword Token.Open)) :: current) rest
    | tok :: rest ->
        (* Non-keyword after comments - treat as simple definition *)
        parse_simple acc (Token tok :: current) rest
        
  and parse_let acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | Token.EOF :: rest -> parse_let acc current rest
    | Token.Whitespace :: rest ->
        parse_let acc (Token Token.Whitespace :: current) rest
    | ((Token.Comment _ | Token.Docstring _) as comment) :: rest ->
        (* Comment after let - finish this let and start collecting for next *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        collect_definition acc' [Token comment] rest
    | (Token.Keyword Token.Let) :: rest ->
        (* New let starts - finish this one *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_let acc' [Token (Token.Keyword Token.Let)] rest
    | (Token.Keyword Token.Type) :: rest ->
        (* New type starts - finish this let *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_type acc' [Token (Token.Keyword Token.Type)] rest
    | (Token.Keyword Token.Module) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_module acc' [Token (Token.Keyword Token.Module)] rest
    | (Token.Keyword Token.Open) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_simple acc' [Token (Token.Keyword Token.Open)] rest
    | (Token.OpenDelim delim) :: rest ->
        let tree, remaining = parse_delimited delim rest in
        parse_let acc (tree :: current) remaining
    | tok :: rest ->
        parse_let acc (Token tok :: current) rest
        
  and parse_type acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | Token.EOF :: rest -> parse_type acc current rest
    | Token.Whitespace :: rest ->
        parse_type acc (Token Token.Whitespace :: current) rest
    | ((Token.Comment _ | Token.Docstring _) as comment) :: rest ->
        (* Comment after type - finish this type and start collecting for next *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        collect_definition acc' [Token comment] rest
    | (Token.Keyword Token.Let) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_let acc' [Token (Token.Keyword Token.Let)] rest
    | (Token.Keyword Token.Type) :: rest when 
        List.exists (function Token (Token.Keyword Token.And) -> true | _ -> false) current ->
        (* Continue with 'and' type *)
        parse_type acc (Token (Token.Keyword Token.Type) :: current) rest
    | (Token.Keyword Token.Type) :: rest ->
        (* New type starts *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_type acc' [Token (Token.Keyword Token.Type)] rest
    | (Token.Keyword Token.Module) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_module acc' [Token (Token.Keyword Token.Module)] rest
    | (Token.Keyword Token.Open) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_simple acc' [Token (Token.Keyword Token.Open)] rest
    | (Token.OpenDelim delim) :: rest ->
        let tree, remaining = parse_delimited delim rest in
        parse_type acc (tree :: current) remaining
    | tok :: rest ->
        parse_type acc (Token tok :: current) rest
        
  and parse_module acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | Token.EOF :: rest -> parse_module acc current rest
    | Token.Whitespace :: rest ->
        parse_module acc (Token Token.Whitespace :: current) rest
    | ((Token.Comment _ | Token.Docstring _) as comment) :: rest ->
        (* Comment after module - finish this module and start collecting for next *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        collect_definition acc' [Token comment] rest
    | (Token.Keyword Token.Let) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_let acc' [Token (Token.Keyword Token.Let)] rest
    | (Token.Keyword Token.Type) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_type acc' [Token (Token.Keyword Token.Type)] rest
    | (Token.Keyword Token.Module) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_module acc' [Token (Token.Keyword Token.Module)] rest
    | (Token.Keyword Token.Open) :: rest ->
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        parse_simple acc' [Token (Token.Keyword Token.Open)] rest
    | (Token.OpenDelim delim) :: rest ->
        let tree, remaining = parse_delimited delim rest in
        parse_module acc (tree :: current) remaining
    | tok :: rest ->
        parse_module acc (Token tok :: current) rest
        
  and parse_simple acc current tokens =
    match tokens with
    | [] -> List.rev ((Tree (Token.BeginEnd, List.rev current)) :: acc)
    | Token.EOF :: rest -> parse_simple acc current rest
    | Token.Whitespace :: rest ->
        parse_simple acc (Token Token.Whitespace :: current) rest
    | ((Token.Comment _ | Token.Docstring _) as comment) :: rest ->
        (* Comment after simple - finish this and start collecting for next *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        collect_definition acc' [Token comment] rest
    | ((Token.Keyword (Token.Let | Token.Type | Token.Module | Token.Open)) as kw) :: rest ->
        (* Start of new top-level item *)
        let acc' = (Tree (Token.BeginEnd, List.rev current)) :: acc in
        (match kw with
         | Token.Keyword Token.Let -> parse_let acc' [Token kw] rest
         | Token.Keyword Token.Type -> parse_type acc' [Token kw] rest
         | Token.Keyword Token.Module -> parse_module acc' [Token kw] rest
         | Token.Keyword Token.Open -> parse_simple acc' [Token kw] rest
         | _ -> parse_simple acc' [Token kw] rest)
    | tok :: rest ->
        parse_simple acc (Token tok :: current) rest
        
  and parse_delimited delim tokens =
    let rec parse_stream acc tokens =
      match tokens with
      | [] -> (List.rev acc, [])
      | Token.EOF :: rest -> (List.rev acc, rest)
      | (Token.CloseDelim d) :: rest when d = delim -> (List.rev acc, rest)
      | (Token.OpenDelim inner_delim) :: rest ->
          let tree, remaining = parse_delimited inner_delim rest in
          parse_stream (tree :: acc) remaining
      | token :: rest -> parse_stream (Token token :: acc) rest
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