open AndesCore
open AndesLexer
open AndesOCamlAst
open Diagnostics

type t = { cursor : Cursor.t } [@@unboxed]

let from_string source = { cursor = Cursor.from_string source }

(*************************************************************************************************)

let ident (t : t) (token : Lexer.token) =
  let raw_str = Lexer.string_from_token t.cursor token in
  let symbol = Symbol.intern raw_str in
  if Token.(is_open_delim_keyword symbol) then
    Token.(OpenDelim (Keyword symbol))
  else if Token.(is_close_delim_keyword symbol) then
    Token.(CloseDelim (Keyword symbol))
  else
    match Token.find_keyword symbol with
    | Some keyword -> Token.Keyword keyword
    | None -> Token.Ident symbol

let comment (t : t) (token : Lexer.token) terminated =
  if not terminated then report_unterminated_comment token;
  let full_str = Lexer.string_from_token t.cursor token in
  (* we gotta drop the open and close pairs *)
  let comment = StringLabels.sub full_str ~pos:2 ~len:(token.size - 4) in
  Token.Comment comment

let docstring (t : t) (token : Lexer.token) terminated =
  if not terminated then report_unterminated_docstring token;
  let full_str = Lexer.string_from_token t.cursor token in
  (* we gotta drop the openeniing ** and close pairs *)
  let docstring = StringLabels.sub full_str ~pos:3 ~len:(token.size - 5) in
  Token.Docstring docstring

let string (t : t) (token : Lexer.token) _terminated =
  let full_str = Lexer.string_from_token t.cursor token in
  (* we gotta drop the quotes *)
  let str = StringLabels.sub full_str ~pos:1 ~len:(token.size - 2) in
  Token.Literal (String str)

let rec next_token t =
  let token = Lexer.next_token t.cursor in
  match token.kind with
  | EOF -> Token.EOF
  | Whitespace -> next_token t
  | Ident -> ident t token
  | Comment { terminated } -> comment t token terminated
  | Docstring { terminated } -> docstring t token terminated
  | Semi -> Token.Semi
  | Comma -> Token.Comma
  | Dot -> Token.Dot
  | OpenParen -> Token.(OpenDelim Paren)
  | CloseParen -> Token.(CloseDelim Paren)
  | OpenBrace -> Token.(OpenDelim Brace)
  | CloseBrace -> Token.(CloseDelim Brace)
  | OpenBracket -> Token.(OpenDelim Bracket)
  | CloseBracket -> Token.(CloseDelim Bracket)
  | At -> Token.At
  | Hash -> Token.Hash
  | Tilde -> Token.Tilde
  | Question -> Token.Question
  | Colon -> Token.Colon
  | Dollar -> Token.Dollar
  | Eq -> Token.Eq
  | Bang -> Token.Bang
  | Lt -> Token.Lt
  | Gt -> Token.Gt
  | Minus -> Token.Minus
  | And -> Token.And
  | Or -> Token.Or
  | Plus -> Token.Plus
  | Star -> Token.Star
  | Slash -> Token.Slash
  | Caret -> Token.Caret
  | Percent -> Token.Percent
  | Literal (Lexer.String { terminated }) -> string t token terminated
  | Unknown -> Token.Unknown

(*************************************************************************************************)

module MutIter = struct
  type state = t
  type item = Token.t

  let next t =
    let item = next_token t in
    if item = Token.EOF then None else Some item

  let size t = Cursor.length_remaining t.cursor
  let clone t = { cursor = Cursor.clone t.cursor }
end

let into_mut_iter t = MutIterator.make (module MutIter) t

module Iter = struct
  type state = t
  type item = Token.t

  let next t =
    let item = next_token t in
    ((if item = Token.EOF then None else Some item), t)

  let size t = Cursor.length_remaining t.cursor
end

let into_iter t = Iterator.make (module Iter) t

(*************************************************************************************************)

module Tests = struct
  let%test "skips all whitespace" =
    let lexer = from_string "     hello world" in
    let token1 = next_token lexer in
    let token2 = next_token lexer in
    let token3 = next_token lexer in
    token1 = Token.Ident "hello"
    && token2 = Token.Ident "world"
    && token3 = Token.EOF

  let%test "captures docstrings" =
    let lexer = from_string "     (** hello world *)" in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Docstring " hello world "

  let%test "captures nested docstrings" =
    let lexer = from_string "     (** hello (* world *)*)" in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Docstring " hello (* world *)"

  let%test "captures multiline docstrings" =
    let lexer =
      from_string
        {docstring|     (** hello

    (* world
     *)

*)

  |docstring}
    in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Docstring " hello\n\n    (* world\n     *)\n\n"

  let%test "captures comments" =
    let lexer = from_string "     (* hello world *)" in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Comment " hello world "

  let%test "captures nested comments" =
    let lexer = from_string "     (* hello (* world *)*)" in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Comment " hello (* world *)"

  let%test "captures multiline comments" =
    let lexer =
      from_string {comment|     (* hello

    (* world
     *)

*)

  |comment}
    in
    let token1 = next_token lexer in
    (* Format.printf "%a\n%!" Token.pp_token token1; *)
    token1 = Token.Comment " hello\n\n    (* world\n     *)\n\n"

  let%expect_test "small program" =
    let lexer =
      {code|

let%test "captures comments" =
  let lexer = from_string "(* hello world *)" in
  let token1 = next_token lexer in
  (* Format.printf "%a\n%!" Token.pp_token token1; *)
  token1 = Token.Comment " hello world "
;;

  |code}
      |> from_string |> into_iter
    in
    let tokens = Iterator.to_list lexer in
    Format.printf "%a\n%!" Token.pp_tokens tokens;
    [%expect
      {|
      ((Keyword Let) Percent (Ident test) (Literal (String "captures comments")) Eq
        (Keyword Let) (Ident lexer) Eq (Ident from_string)
        (Literal (String "(* hello world *)")) (Keyword In) (Keyword Let)
        (Ident token1) Eq (Ident next_token) (Ident lexer) (Keyword In)
        (Comment " Format.printf \"%a\\n%!\" Token.pp_token token1; ")
        (Ident token1) Eq (Ident Token) Dot (Ident Comment)
        (Literal (String " hello world ")) Semi Semi)
      |}]

  let%expect_test "nested parens in comment" =
    let lexer =
      {code|

(* | effect x, k -> () *)

  |code} |> from_string |> into_iter
    in
    let tokens = Iterator.to_list lexer in
    Format.printf "%a\n%!" Token.pp_tokens tokens;
    [%expect {| ((Comment " | effect x, k -> () ")) |}]
end
