open Std

(** Normalize line endings to `\n`.

    Use this before lexing when the input may contain Windows or mixed newline
    conventions.
*)
val normalize_newlines: string -> string

(** Tokenize markdown source into low-level lexer tokens.

    Example:
    ```ocaml
    let tokens = Markdown_lexer.tokenize "# title\n"
    ```
*)
val tokenize: string -> Markdown_token.t list
