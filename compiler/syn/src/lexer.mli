open Std

(**
   Lexical analyzer from source slices to token records.

   The lexer is lossless and recovers from malformed input. Token body spans
   cover only the token text; whitespace, comments, and docstrings that appear
   before a token are stored on that token's `leading_trivia`. The EOF token
   carries trailing file trivia.

   The parser uses this module through `tokenize`, so keep the public lexer
   path slice-backed as well.
*)

(** Lexer state (cursor over a source slice). *)
type t

(**
   `create source` creates a new lexer for the given source slice.

   Example:
   ```ocaml
   let source =
     IO.IoVec.IoSlice.from_string "let x = 1"
     |> Result.expect ~msg:"source"
   in
   let lexer = Lexer.create source in
   let tok = Lexer.next lexer []
   ```
*)
val create: IO.IoVec.IoSlice.t -> t

(**
   `next lexer delim_stack` returns the next token from the source.

   Advances the lexer position past the returned token. Returns EOF token when
   end of source is reached. The delimiter stack is used to match 'end'
   keywords to their corresponding opening delimiters (struct, sig, begin,
   object).

   Example:
   ```ocaml
   let lexer = Lexer.create source in
   let tok1 = Lexer.next lexer [] in
   let tok2 = Lexer.next lexer [] in
   ignore (tok1, tok2)
   ```
*)
val next: t -> Token.delimiter list -> Token.t

(**
   `tokenize source` lexes the entire source slice into a token list.

   This is a convenience function that creates a lexer, repeatedly calls `next`
   until EOF, and returns all tokens. Non-EOF trivia is attached to the next
   real token's `leading_trivia`, and trailing file trivia is preserved on the
   EOF token's `leading_trivia`.

   The returned list always ends with an EOF token.

   Example:
   ```ocaml
   let tokens = Lexer.tokenize source in
   List.length tokens
   ```
*)
val tokenize: IO.IoVec.IoSlice.t -> Token.t list
