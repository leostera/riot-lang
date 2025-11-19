type t =
  | ERROR
  | WHITESPACE
  | COMMENT
  | PROGRAM
  | FACT
  | RULE
  | ATOM
  | NEGATED_ATOM
  | BUILTIN
  | VARIABLE
  | CONSTANT
  | WILDCARD
  | STRING_LITERAL
  | INT_LITERAL
  | IDENT
  | DOT
  | COMMA
  | LPAREN
  | RPAREN
  | BANG
  | COLON_DASH
  | GT
  | LT
  | GTEQ
  | LTEQ
  | EQ
  | NOTEQ

val to_string : t -> string
