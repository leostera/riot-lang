open Std

let keyword_to_string = function
  | Token.Let -> "let" | Token.In -> "in" | Token.If -> "if"
  | Token.Then -> "then" | Token.Else -> "else" | Token.Match -> "match"
  | Token.With -> "with" | Token.Function -> "function" | Token.Fun -> "fun"
  | Token.Try -> "try" | Token.Type -> "type" | Token.Module -> "module"
  | Token.Struct -> "struct" | Token.Sig -> "sig" | Token.End -> "end"
  | Token.Open -> "open" | Token.Include -> "include" | Token.And -> "and"
  | Token.Or -> "or" | Token.Rec -> "rec" | Token.True -> "true"
  | Token.False -> "false" | Token.When -> "when" | Token.As -> "as"
  | Token.Of -> "of" | Token.Exception -> "exception" | Token.For -> "for"
  | Token.To -> "to" | Token.Downto -> "downto" | Token.While -> "while"
  | Token.Do -> "do" | Token.Done -> "done" | Token.Begin -> "begin"
  | Token.Val -> "val" | Token.External -> "external" | Token.Method -> "method"
  | Token.Object -> "object" | Token.Class -> "class" | Token.New -> "new"
  | Token.Inherit -> "inherit" | Token.Constraint -> "constraint"
  | Token.Initializer -> "initializer" | Token.Lazy -> "lazy"
  | Token.Mutable -> "mutable" | Token.Private -> "private"
  | Token.Virtual -> "virtual" | Token.Nonrec -> "nonrec"
  | Token.Assert -> "assert" | Token.Asr -> "asr" | Token.Land -> "land"
  | Token.Lor -> "lor" | Token.Lsl -> "lsl" | Token.Lsr -> "lsr"
  | Token.Lxor -> "lxor" | Token.Mod -> "mod" | Token.Functor -> "functor"

let to_string = function
  | Token.Keyword kw -> keyword_to_string kw
  | Token.Ident s -> Printf.sprintf "identifier '%s'" s
  | Token.Literal (Token.Int i) -> Printf.sprintf "integer %d" i
  | Token.Literal (Token.Float f) -> Printf.sprintf "float %f" f
  | Token.Literal (Token.String { value; _ }) -> Printf.sprintf "string \"%s\"" value
  | Token.Literal (Token.Char c) -> Printf.sprintf "char '%c'" c
  | Token.Plus -> "+" | Token.Minus -> "-" | Token.Star -> "*"
  | Token.Slash -> "/" | Token.Percent -> "%" | Token.Caret -> "^"
  | Token.Eq -> "=" | Token.Lt -> "<" | Token.Gt -> ">"
  | Token.LtEq -> "<=" | Token.GtEq -> ">=" | Token.Ne -> "<>"
  | Token.Arrow -> "->" | Token.FatArrow -> "=>" | Token.ColonColon -> "::"
  | Token.Semi -> ";" | Token.Comma -> "," | Token.Dot -> "."
  | Token.Colon -> ":" | Token.ColonEq -> ":=" | Token.Pipe -> "|"
  | Token.OpenDelim Token.Paren -> "("
  | Token.CloseDelim Token.Paren -> ")"
  | Token.OpenDelim Token.Bracket -> "["
  | Token.CloseDelim Token.Bracket -> "]"
  | Token.OpenDelim Token.Brace -> "{"
  | Token.CloseDelim Token.Brace -> "}"
  | Token.Bang -> "!" | Token.And -> "&&" | Token.Or -> "||"
  | Token.Question -> "?" | Token.At -> "@" | Token.Hash -> "#"
  | Token.Tilde -> "~" | Token.Dollar -> "$" | Token.Ampersand -> "&"
  | Token.Underscore -> "_" | Token.Whitespace -> "whitespace"
  | Token.Comment _ -> "comment" | Token.Docstring _ -> "docstring"
  | Token.EOF -> "end of file"
  | Token.Unknown c -> Printf.sprintf "unknown character '%c'" c
  | _ -> "token"