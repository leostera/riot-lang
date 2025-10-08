open Std

(** Tokens - Lexical Elements

    This module defines all token types produced by the lexer.

    Tokens represent atomic lexical elements from the source: keywords,
    identifiers, literals, operators, delimiters, and trivia
    (whitespace/comments).

    # Token Structure

    Each token has:
    - **kind**: What type of token it is
    - **span**: Where it appears in the source (using `Ceibo.Span.t`)

    The text of the token is not stored directly - use the span to extract it
    from the original source when needed. *)

(** # Types *)

type keyword = Keyword.t
(** Keyword type from the `Keyword` module. *)

type literal =
  | String of { value : string; terminated : bool }
      (** String literal. `terminated` is false if the closing quote is missing.
      *)
  | Int of int  (** Integer literal. *)
  | Float of float  (** Floating-point literal. *)
  | Char of char  (** Character literal. *)

type delimiter =
  | Paren  (** `(` or `)` *)
  | Brace  (** `{` or `}` *)
  | Bracket  (** `[` or `]` *)
  | BeginEnd  (** `begin` / `end` pair *)
  | StructEnd  (** `struct` / `end` pair *)
  | SigEnd  (** `sig` / `end` pair *)
  | ObjectEnd  (** `object` / `end` pair *)

type token_kind =
  (* Keywords *)
  | Keyword of keyword
      (** Reserved OCaml keyword like `let`, `if`, `match`, etc. *)
  (* Identifiers *)
  | Ident of string
      (** Identifier: variable name, function name, module name, etc. *)
  (* Literals *)
  | Literal of literal  (** Literal value: int, float, string, or char. *)
  (* Delimiters *)
  | OpenDelim of delimiter
      (** Opening delimiter: `(`, `{`, `[`, `begin`, `struct`, `sig`, `object` *)
  | CloseDelim of delimiter  (** Closing delimiter: `)`, `}`, `]`, `end` *)
  (* Trivia *)
  | Comment of { value : string; terminated : bool }
      (** Block comment `(* ... *)`. `terminated` is false if unclosed. *)
  | Docstring of { value : string; terminated : bool }
      (** Documentation comment `(** ... *)`. `terminated` is false if unclosed.
      *)
  | Whitespace  (** Whitespace: spaces, tabs, newlines. *)
  (* Operators *)
  | Plus  (** `+` *)
  | Minus  (** `-` *)
  | Star  (** `*` *)
  | Slash  (** `/` *)
  | Percent  (** `%` *)
  | Caret  (** `^` *)
  | Eq  (** `=` *)
  | Lt  (** `<` *)
  | Gt  (** `>` *)
  | LtEq  (** `<=` *)
  | GtEq  (** `>=` *)
  | Ne  (** `<>` *)
  | Bang  (** `!` *)
  | And  (** `&&` *)
  | Or  (** `||` *)
  (* Punctuation *)
  | Colon  (** `:` *)
  | Semi  (** `;` *)
  | Comma  (** `,` *)
  | Dot  (** `.` *)
  | Arrow  (** `->` *)
  | LeftArrow  (** `<-` *)
  | FatArrow  (** `=>` *)
  | ColonColon  (** `::` *)
  | ColonEq  (** `:=` *)
  | Question  (** `?` *)
  | At  (** `@` *)
  | Hash  (** `#` *)
  | Tilde  (** `~` *)
  | Dollar  (** `$` *)
  | Pipe  (** `|` *)
  | Ampersand  (** `&` *)
  | Underscore  (** `_` *)
  | Backtick  (** `` ` `` - polymorphic variant tag *)
  | Quote  (** `'` - type variable prefix *)
  | StarStar  (** `**` - float power *)
  | EqEq  (** `==` - physical equality *)
  | BangEq  (** `!=` - physical inequality *)
  | AtAt  (** `@@` - application operator *)
  | PipeGt  (** `|>` - reverse application *)
  | PercentGt  (** `%>` - compose right *)
  | LtPercent  (** `<%` - compose left *)
  (* Special *)
  | EOF  (** End of file marker. *)
  | Unknown of char  (** Unknown/invalid character. Used for error recovery. *)

type t = { kind : token_kind; span : Ceibo.Span.t }
(** A token with its kind and source location. *)

(** # Utilities *)

val delimiter_of_keyword : keyword -> delimiter option
(** `delimiter_of_keyword kw` returns the delimiter type for keywords that act
    as opening delimiters.

    Returns `Some delimiter` for: `begin`, `struct`, `sig`, `object` Returns
    `None` for all other keywords.

    Example: ```ocaml delimiter_of_keyword Keyword.Begin = Some BeginEnd
    delimiter_of_keyword Keyword.Struct = Some StructEnd delimiter_of_keyword
    Keyword.Let = None ``` *)

val show_kind : token_kind -> string
(** `show_kind kind` returns a human-readable name for a token kind.

    Useful for error messages and debugging.

    Example: ```ocaml show_kind (Ident "foo") = "identifier" show_kind (Literal
    (Int 42)) = "integer" show_kind Plus = "'+'" show_kind EOF = "end of file"
    ``` *)
