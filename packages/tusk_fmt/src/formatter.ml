open Std

let keyword_to_string : Syn.Token.keyword -> string = function
  | Syn.Token.And -> "and"
  | Syn.Token.As -> "as"
  | Syn.Token.Asr -> "asr"
  | Syn.Token.Assert -> "assert"
  | Syn.Token.Begin -> "begin"
  | Syn.Token.Class -> "class"
  | Syn.Token.Constraint -> "constraint"
  | Syn.Token.Do -> "do"
  | Syn.Token.Done -> "done"
  | Syn.Token.Downto -> "downto"
  | Syn.Token.Else -> "else"
  | Syn.Token.End -> "end"
  | Syn.Token.Exception -> "exception"
  | Syn.Token.External -> "external"
  | Syn.Token.False -> "false"
  | Syn.Token.For -> "for"
  | Syn.Token.Fun -> "fun"
  | Syn.Token.Function -> "function"
  | Syn.Token.Functor -> "functor"
  | Syn.Token.If -> "if"
  | Syn.Token.In -> "in"
  | Syn.Token.Include -> "include"
  | Syn.Token.Inherit -> "inherit"
  | Syn.Token.Initializer -> "initializer"
  | Syn.Token.Land -> "land"
  | Syn.Token.Lazy -> "lazy"
  | Syn.Token.Let -> "let"
  | Syn.Token.Lor -> "lor"
  | Syn.Token.Lsl -> "lsl"
  | Syn.Token.Lsr -> "lsr"
  | Syn.Token.Lxor -> "lxor"
  | Syn.Token.Match -> "match"
  | Syn.Token.Method -> "method"
  | Syn.Token.Mod -> "mod"
  | Syn.Token.Module -> "module"
  | Syn.Token.Mutable -> "mutable"
  | Syn.Token.New -> "new"
  | Syn.Token.Nonrec -> "nonrec"
  | Syn.Token.Object -> "object"
  | Syn.Token.Of -> "of"
  | Syn.Token.Open -> "open"
  | Syn.Token.Or -> "or"
  | Syn.Token.Private -> "private"
  | Syn.Token.Rec -> "rec"
  | Syn.Token.Sig -> "sig"
  | Syn.Token.Struct -> "struct"
  | Syn.Token.Then -> "then"
  | Syn.Token.To -> "to"
  | Syn.Token.True -> "true"
  | Syn.Token.Try -> "try"
  | Syn.Token.Type -> "type"
  | Syn.Token.Val -> "val"
  | Syn.Token.Virtual -> "virtual"
  | Syn.Token.When -> "when"
  | Syn.Token.While -> "while"
  | Syn.Token.With -> "with"

let literal_to_string : Syn.Token.literal -> string = function
  | Syn.Token.String { value; _ } -> format "\"%s\"" value
  | Syn.Token.Int i -> string_of_int i
  | Syn.Token.Float f -> string_of_float f
  | Syn.Token.Char c -> format "'%c'" c

let delimiter_strings : Syn.Token.delimiter -> string * string = function
  | Syn.Token.Paren -> ("(", ")")
  | Syn.Token.Brace -> ("{", "}")
  | Syn.Token.Bracket -> ("[", "]")
  | Syn.Token.BeginEnd -> ("begin", "end")
  | Syn.Token.StructEnd -> ("struct", "end")
  | Syn.Token.SigEnd -> ("sig", "end")
  | Syn.Token.ObjectEnd -> ("object", "end")

let format_token = function
  | Syn.Token.Comment { value; _ } -> format "(*%s*)" value
  | Syn.Token.Docstring { value; _ } -> format "(**%s*)" value
  | Syn.Token.Keyword kw -> keyword_to_string kw
  | Syn.Token.Ident s -> s
  | Syn.Token.Literal lit -> literal_to_string lit
  | Syn.Token.Plus -> "+"
  | Syn.Token.Minus -> "-"
  | Syn.Token.Star -> "*"
  | Syn.Token.Slash -> "/"
  | Syn.Token.Percent -> "%"
  | Syn.Token.Caret -> "^"
  | Syn.Token.Eq -> "="
  | Syn.Token.Lt -> "<"
  | Syn.Token.Gt -> ">"
  | Syn.Token.Bang -> "!"
  | Syn.Token.And -> "&&"
  | Syn.Token.Or -> "||"
  | Syn.Token.Colon -> ":"
  | Syn.Token.Semi -> ";"
  | Syn.Token.Comma -> ","
  | Syn.Token.Dot -> "."
  | Syn.Token.Arrow -> "->"
  | Syn.Token.FatArrow -> "=>"
  | Syn.Token.ColonColon -> "::"
  | Syn.Token.ColonEq -> ":="
  | Syn.Token.Question -> "?"
  | Syn.Token.At -> "@"
  | Syn.Token.Hash -> "#"
  | Syn.Token.Tilde -> "~"
  | Syn.Token.Dollar -> "$"
  | Syn.Token.Pipe -> "|"
  | Syn.Token.Ampersand -> "&"
  | Syn.Token.Underscore -> "_"
  | Syn.Token.Whitespace -> " "
  | Syn.Token.EOF -> ""
  | Syn.Token.Unknown c -> format "%c" c
  | Syn.Token.OpenDelim _ -> ""
  | Syn.Token.CloseDelim _ -> ""

let rec format_tree = function
  | Syn.TokenTree.Token tok -> format_token tok
  | Syn.TokenTree.Tree (delim, contents) ->
      let open_str, close_str = delimiter_strings delim in
      let inner = format_trees contents in
      format "%s%s%s" open_str inner close_str

and format_trees trees = String.concat "" (List.map format_tree trees)

let format trees = Simple_formatter.format trees
