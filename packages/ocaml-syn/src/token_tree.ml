open Std

type t = Token of Token.t | Tree of Token.delimiter * t list

let rec of_tokens tokens =
  let rec parse_stream acc = function
    | [] -> (List.rev acc, [])
    | Token.EOF :: rest -> (List.rev acc, rest)
    | Token.OpenDelim delim :: rest ->
        let tree, remaining = parse_delimited delim rest in
        parse_stream (tree :: acc) remaining
    | Token.CloseDelim _ :: rest -> (List.rev acc, rest)
    | token :: rest -> parse_stream (Token token :: acc) rest
  and parse_delimited delim tokens =
    let contents, remaining = parse_stream [] tokens in
    (Tree (delim, contents), remaining)
  in
  let trees, _ = parse_stream [] tokens in
  trees

let delimiter_to_string = function
  | Token.Paren -> "Paren"
  | Token.Brace -> "Brace"
  | Token.Bracket -> "Bracket"
  | Token.BeginEnd -> "BeginEnd"
  | Token.StructEnd -> "StructEnd"
  | Token.SigEnd -> "SigEnd"
  | Token.ObjectEnd -> "ObjectEnd"

let token_to_string = function
  | Token.Keyword _ -> "Keyword(...)"
  | Token.Ident s -> format "Ident(%s)" s
  | Token.Literal _ -> "Literal(...)"
  | Token.OpenDelim d -> format "OpenDelim(%s)" (delimiter_to_string d)
  | Token.CloseDelim d -> format "CloseDelim(%s)" (delimiter_to_string d)
  | Token.Comment _ -> "Comment(...)"
  | Token.Docstring _ -> "Docstring(...)"
  | Token.Plus -> "Plus"
  | Token.Minus -> "Minus"
  | Token.Star -> "Star"
  | Token.Slash -> "Slash"
  | Token.Percent -> "Percent"
  | Token.Caret -> "Caret"
  | Token.Eq -> "Eq"
  | Token.Lt -> "Lt"
  | Token.Gt -> "Gt"
  | Token.Bang -> "Bang"
  | Token.And -> "And"
  | Token.Or -> "Or"
  | Token.Colon -> "Colon"
  | Token.Semi -> "Semi"
  | Token.Comma -> "Comma"
  | Token.Dot -> "Dot"
  | Token.Arrow -> "Arrow"
  | Token.FatArrow -> "FatArrow"
  | Token.ColonColon -> "ColonColon"
  | Token.ColonEq -> "ColonEq"
  | Token.Question -> "Question"
  | Token.At -> "At"
  | Token.Hash -> "Hash"
  | Token.Tilde -> "Tilde"
  | Token.Dollar -> "Dollar"
  | Token.Pipe -> "Pipe"
  | Token.Ampersand -> "Ampersand"
  | Token.Whitespace -> "Whitespace"
  | Token.EOF -> "EOF"
  | Token.Unknown c -> format "Unknown(%c)" c

let rec to_string_indent indent tree =
  let indent_str = String.make (indent * 2) ' ' in
  match tree with
  | Token tok -> format "%sToken(%s)\n" indent_str (token_to_string tok)
  | Tree (delim, trees) ->
      let contents =
        List.map (to_string_indent (indent + 1)) trees |> String.concat ""
      in
      format "%sTree(%s) {\n%s%s}\n" indent_str (delimiter_to_string delim)
        contents indent_str

let to_string tree = to_string_indent 0 tree
let list_to_string trees = List.map to_string trees |> String.concat "\n"
