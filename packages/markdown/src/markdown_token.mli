open Std

(** Token kind emitted by the markdown lexer. *)
type token_kind =
  | Line_text
  | Newline
  | EOF

(** One markdown lexer token. *)
type t = {
  (** Token kind. *)
  kind: token_kind;
  (** Source span covered by the token. *)
  span: Ceibo.Span.t;
  (** Raw token text. *)
  text: string;
}

(**
   Render a token kind as a human-readable label.

   Example:
   ```ocaml
   Markdown_token.show_kind Markdown_token.Newline = "Newline"
   ```
*)
val show_kind: token_kind -> string
