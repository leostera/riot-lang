open Std

(** Lexical Analyzer - Source to Tokens
    
    The lexer (tokenizer) converts source code text into a flat stream of tokens.
    
    # Lexer Characteristics
    
    - **Lossless**: Preserves all source information including whitespace
    - **Error Recovery**: Produces tokens even from malformed input
    - **Trivia Preservation**: Includes whitespace, comments, and docstrings
    - **Single Pass**: Processes source left-to-right in one scan
    
    # What Gets Tokenized
    
    The lexer recognizes:
    - Keywords (`let`, `if`, `match`, etc.)
    - Identifiers (variable/function/module names)
    - Literals (integers, floats, strings, chars, booleans)
    - Operators (`+`, `-`, `::`, `->`, etc.)
    - Delimiters (`(`, `)`, `[`, `]`, `{`, `}`, `begin`/`end`, etc.)
    - Trivia (whitespace, comments, docstrings)
    
    # Token Stream Properties
    
    The token stream has these properties:
    - Tokens are in source order
    - Token body spans are body-only; preceding trivia is stored on
      `leading_trivia`
    - All source bytes are covered by token bodies or token `leading_trivia`
    - Malformed tokens (unclosed strings) are still tokenized
    
    # Example Usage
    
    ## Basic Tokenization
    
    ```ocaml
    let source = "let x = 42" in
    let tokens = Lexer.tokenize source in
    
    List.iter (fun tok ->
      let text = String.sub source tok.span.start 
        (tok.span.end_ - tok.span.start) in
      Printf.printf "%s: %s\n" (Token.show_kind tok.kind) text
    ) tokens
    ```
    
    ## Handling Comments
    
    ```ocaml
    let source = "(* comment *) let x = 1" in
    let tokens = Lexer.tokenize source in
    
    (* Leading trivia is preserved on the next real token *)
    let has_comment = List.exists (fun tok ->
      List.exists (fun trivia ->
        match trivia.Token.kind with
        | Token.CommentTrivia _ -> true
        | _ -> false
      ) tok.leading_trivia
    ) tokens in
    Printf.printf "Has comment: %b\n" has_comment
    ```
    
    ## Low-Level Token Stream
    
    ```ocaml
    let cursor = Lexer.create source in
    
    (* Repeatedly get next token *)
    let rec loop () =
      let tok = Lexer.next cursor in
      match tok.kind with
      | Token.EOF -> []
      | _ -> tok :: loop ()
    in
    let tokens = loop ()
    ```
*)
(** # Types *)

(** Lexer state (cursor over source). *)
type t
(** # Construction *)
(** `create source` creates a new lexer for the given source code.

    Example: ```ocaml let lexer = Lexer.create "let x = 1" in let tok =
    Lexer.next lexer ``` *)
val create : string -> t

(** # Tokenization *)
(** `next lexer delim_stack` returns the next token from the source.

    Advances the lexer position past the returned token. Returns EOF token when
    end of source is reached. The delimiter stack is used to match 'end'
    keywords to their corresponding opening delimiters (struct, sig, begin,
    object).

    Example: ```ocaml let lexer = Lexer.create source in let tok1 = Lexer.next
    lexer [] in (* first token *) let tok2 = Lexer.next lexer [] in (* second
    token *) (* ... *) let last = Lexer.next lexer [] in (* eventually returns
    EOF *) ``` *)
val next : t -> Token.delimiter list -> Token.t

(** `tokenize source` lexes the entire source into a token list.

    This is a convenience function that creates a lexer, repeatedly calls `next`
    until EOF, and returns all tokens. Non-EOF trivia is attached to the next
    real token's `leading_trivia`, and trailing file trivia is preserved on the
    EOF token's `leading_trivia`.

    The returned list always ends with an EOF token.

    Example: ```ocaml let tokens = Lexer.tokenize "let x = 42" in List.length
    tokens (* typically 5: let, x, =, 42, EOF *) ``` *)
val tokenize : string -> Token.t list
