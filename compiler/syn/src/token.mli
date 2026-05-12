open Std

(**
   Lexical elements produced by the lexer.

   Tokens represent atomic lexical elements from the source: keywords,
   identifiers, literals, operators, delimiters, and trivia
   (whitespace/comments).

   # Token Structure

   Each token has:
   - **kind**: What type of token it is
   - **span**: Where it appears in the source (using `Span.t`)
   - **leading_trivia**: Trivia immediately preceding the token

   The text of the token is not stored directly - use the span to extract it
   from the original source when needed.
*)

(** Keyword type from the `Keyword` module. *)
type keyword = Keyword.t
type literal =
  | String of { value: string; terminated: bool }
  (**
     String literal. `terminated` is false if the closing quote is missing.
  *)
  | Int of int
  (** Integer literal. *)
  | Float of float
  (** Floating-point literal. *)
  | Char of char
(** Character literal. *)
type delimiter =
  | Paren
  (** `(` or `)` *)
  | Brace
  (** `{` or `}` *)
  | Bracket
  (** `[` or `]` *)
  | Array
  (** `[|` or `|]` *)
  | BeginEnd
  (** `begin` / `end` pair *)
  | StructEnd
  (** `struct` / `end` pair *)
  | SigEnd
(** A token with its kind and source location. *)
type trivia_kind =
  | CommentTrivia of { value: string; terminated: bool }
  | DocstringTrivia of { value: string; terminated: bool }
  | WhitespaceTrivia
type trivia = {
  kind: trivia_kind;
  span: Span.t;
}
type token_kind =
  (* Keywords *)
  | Keyword of keyword
  (** Reserved OCaml keyword like `let`, `if`, `match`, etc. *)
  (* Identifiers *)
  | Ident of string
  (** Identifier: variable name, function name, module name, etc. *)
  (* Literals *)
  | Literal of literal
  (** Literal value: int, float, string, or char. *)
  (* Delimiters *)
  | OpenDelim of delimiter
  (** Opening delimiter: `(`, `{`, `[`, `begin`, `struct`, `sig` *)
  | CloseDelim of delimiter
  (** Closing delimiter: `)`, `}`, `]`, `end` *)
  (* Trivia *)
  | Comment of { value: string; terminated: bool }
  (** Block comment `(* ... *)`. `terminated` is false if unclosed. *)
  | Docstring of { value: string; terminated: bool }
  (**
     Documentation comment `(** ... *)`. `terminated` is false if unclosed.
  *)
  | Whitespace
  (** Whitespace: spaces, tabs, newlines. *)
  (* Operators *)
  | Plus
  (** `+` *)
  | Minus
  (** `-` *)
  | Star
  (** `*` *)
  | Slash
  (** `/` *)
  | Percent
  (** `%` *)
  | Caret
  (** `^` *)
  | Eq
  (** `=` *)
  | Lt
  (** `<` *)
  | Gt
  (** `>` *)
  | LtEq
  (** `<=` *)
  | GtEq
  (** `>=` *)
  | Ne
  (** `<>` *)
  | Bang
  (** `!` *)
  | And
  (** `&&` *)
  | Or
  (** `||` *)
  (* Punctuation *)
  | Colon
  (** `:` *)
  | Semi
  (** `;` *)
  | Comma
  (** `,` *)
  | Dot
  (** `.` *)
  | DotDot
  (** `..` for range patterns *)
  | Arrow
  (** `->` *)
  | LeftArrow
  (** `<-` *)
  | FatArrow
  (** `=>` *)
  | ColonColon
  (** `::` *)
  | ColonEq
  (** `:=` *)
  | Question
  (** `?` *)
  | At
  (** `@` *)
  | Hash
  (** `#` *)
  | Tilde
  (** `~` *)
  | Dollar
  (** `$` *)
  | Pipe
  (** `|` *)
  | Ampersand
  (** `&` *)
  | Underscore
  (** `_` *)
  | Backtick
  (** `` ` `` - polymorphic variant tag *)
  | Quote
  (** `'` - type variable prefix *)
  | StarStar
  (** `**` - float power *)
  | EqEq
  (** `==` - physical equality *)
  | BangEq
  (** `!=` - physical inequality *)
  | AtAt
  (** `@@` - application operator *)
  | PipeGt
  (** `|>` - reverse application *)
  | PercentGt
  (** `%>` - compose right *)
  | LtPercent
  (** `<%` - compose left *)
  | PlusDot
  (** `+.` - float addition *)
  | MinusDot
  (** `-.` - float subtraction *)
  | StarDot
  (** `*.` - float multiplication *)
  | SlashDot
  (** `/.` - float division *)
  (* Special *)
  | EOF
  (** End of file marker. *)
  | Unknown of char
(** Unknown/invalid character. Used for error recovery. *)
type t = {
  kind: token_kind;
  span: Span.t;
  leading_trivia: trivia list;
}

(**
   `delimiter_of_keyword kw` returns the delimiter type for keywords that act
   as opening delimiters.

   Returns `Some delimiter` for: `begin`, `struct`, `sig`, `object` Returns
   `None` for all other keywords.

   Example: ```ocaml delimiter_of_keyword Keyword.Begin = Some BeginEnd
   delimiter_of_keyword Keyword.Struct = Some StructEnd delimiter_of_keyword
   Keyword.Let = None ```
*)
val delimiter_of_keyword: keyword -> delimiter option

val token_kind_of_trivia_kind: trivia_kind -> token_kind

val trivia_kind_of_token_kind: token_kind -> trivia_kind option

val trivia_of_token: t -> trivia option

val trivia_to_token: trivia -> t

val with_leading_trivia: t -> trivia list -> t

(**
   `show_kind kind` returns a human-readable name for a token kind.

   Useful for error messages and debugging.

   Example: ```ocaml show_kind (Ident "foo") = "identifier" show_kind (Literal
   (Int 42)) = "integer" show_kind Plus = "'+'" show_kind EOF = "end of file"
   ```
*)
val show_kind: token_kind -> string

(**
   `to_string token` returns a human-readable description of the token.

   This is a convenience wrapper around `show_kind` that takes a full token.

   Example: ```ocaml
   to_string { kind = Ident "foo"; span = ... } = "identifier"
   to_string { kind = EOF; span = ... } = "end of file"
   ```
*)
val to_string: t -> string
