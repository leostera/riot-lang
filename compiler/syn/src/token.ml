open Std

type keyword = Keyword.t

type literal =
  | String of { value: string; terminated: bool }
  | Int of int
  | Float of float
  | Char of char

type delimiter =
  | Paren
  | Brace
  | Bracket
  | Array
  | BeginEnd
  | StructEnd
  | SigEnd

type trivia_kind =
  | CommentTrivia of { value: string; terminated: bool }
  | DocstringTrivia of { value: string; terminated: bool }
  | WhitespaceTrivia

type token_kind =
  | Keyword of keyword
  | Ident of string
  | Literal of literal
  | OpenDelim of delimiter
  | CloseDelim of delimiter
  | Comment of { value: string; terminated: bool }
  | Docstring of { value: string; terminated: bool }
  | Whitespace
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Eq
  | Lt
  | Gt
  | LtEq
  | GtEq
  | Ne
  | Bang
  | And
  | Or
  | Colon
  | Semi
  | Comma
  | Dot
  | DotDot
  | Arrow
  | LeftArrow
  | FatArrow
  | ColonColon
  | ColonEq
  | Question
  | At
  | Hash
  | Tilde
  | Dollar
  | Pipe
  | Ampersand
  | Underscore
  | Backtick
  | Quote
  | StarStar
  | EqEq
  | BangEq
  | AtAt
  | PipeGt
  | PercentGt
  | LtPercent
  | PlusDot
  (* +. *)
  | MinusDot
  (* -. *)
  | StarDot
  (* *. *)
  | SlashDot
  (* /. *)
  | EOF
  | Unknown of char

type t = {
  kind: token_kind;
  span: Span.t;
  leading_trivia: trivia list;
}

and trivia = {
  kind: trivia_kind;
  span: Span.t;
}

let delimiter_of_keyword: keyword -> delimiter option = fun __tmp1 ->
  match __tmp1 with
  | Begin -> Some BeginEnd
  | Struct -> Some StructEnd
  | Sig -> Some SigEnd
  | _ -> None

let token_kind_of_trivia_kind = fun __tmp1 ->
  match __tmp1 with
  | CommentTrivia { value; terminated } -> Comment { value; terminated }
  | DocstringTrivia { value; terminated } -> Docstring { value; terminated }
  | WhitespaceTrivia -> Whitespace

let trivia_kind_of_token_kind = fun __tmp1 ->
  match __tmp1 with
  | Comment { value; terminated } -> Some (CommentTrivia { value; terminated })
  | Docstring { value; terminated } -> Some (DocstringTrivia { value; terminated })
  | Whitespace -> Some WhitespaceTrivia
  | _ -> None

let trivia_of_token = fun token ->
  Option.map
    (trivia_kind_of_token_kind token.kind)
    ~fn:(fun kind -> { kind; span = token.span })

let trivia_to_token = fun (trivia: trivia) -> {
  kind = token_kind_of_trivia_kind trivia.kind;
  span = trivia.span;
  leading_trivia = [];
}

let with_leading_trivia = fun token leading_trivia -> { token with leading_trivia }

let show_kind = fun __tmp1 ->
  match __tmp1 with
  | Keyword _ -> "keyword"
  | Ident _ -> "identifier"
  | Literal (Int _) -> "integer"
  | Literal (Float _) -> "float"
  | Literal (String _) -> "string"
  | Literal (Char _) -> "char"
  | OpenDelim Paren -> "("
  | CloseDelim Paren -> ")"
  | OpenDelim Brace -> "{"
  | CloseDelim Brace -> "}"
  | OpenDelim Bracket -> "["
  | CloseDelim Bracket -> "]"
  | OpenDelim Array -> "[|"
  | CloseDelim Array -> "|]"
  | OpenDelim BeginEnd -> "begin"
  | CloseDelim BeginEnd -> "end"
  | OpenDelim StructEnd -> "struct"
  | CloseDelim StructEnd -> "end"
  | OpenDelim SigEnd -> "sig"
  | CloseDelim SigEnd -> "end"
  | Comment _ -> "comment"
  | Docstring _ -> "docstring"
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"
  | Percent -> "%"
  | Caret -> "^"
  | Eq -> "="
  | Lt -> "<"
  | Gt -> ">"
  | LtEq -> "<="
  | GtEq -> ">="
  | Ne -> "<>"
  | Bang -> "!"
  | And -> "&&"
  | Or -> "||"
  | Colon -> ":"
  | Semi -> ";"
  | Comma -> ","
  | Dot -> "."
  | DotDot -> ".."
  | Arrow -> "->"
  | LeftArrow -> "<-"
  | FatArrow -> "=>"
  | ColonColon -> "::"
  | ColonEq -> ":="
  | Question -> "?"
  | At -> "@"
  | Hash -> "#"
  | Tilde -> "~"
  | Dollar -> "$"
  | Pipe -> "|"
  | Ampersand -> "&"
  | Underscore -> "_"
  | Backtick -> "`"
  | Quote -> "'"
  | StarStar -> "**"
  | EqEq -> "=="
  | BangEq -> "!="
  | AtAt -> "@@"
  | PipeGt -> "|>"
  | PercentGt -> "%>"
  | LtPercent -> "<%"
  | PlusDot -> "+."
  | MinusDot -> "-."
  | StarDot -> "*."
  | SlashDot -> "/."
  | Whitespace -> "whitespace"
  | EOF -> "end of file"
  | Unknown c -> "unknown character '" ^ String.make ~len:1 ~char:c ^ "'"

let to_string = fun token -> show_kind token.kind
