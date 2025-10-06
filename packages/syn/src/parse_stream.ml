open Std

type error = {
  message : string;
  position : int;
  expected : string list option;
}

type t = {
  tokens : Token.t array;
  position : int;
  errors : error list;
}

let create tokens = {
  tokens;
  position = 0;
  errors = [];
}

let position t = t.position

let is_empty t = 
  t.position >= Array.length t.tokens ||
  (match t.tokens.(t.position).Token.kind with Token.EOF -> true | _ -> false)

let peek t =
  if t.position < Array.length t.tokens then
    Some t.tokens.(t.position)
  else
    None

let peek_n t n =
  let pos = t.position + n in
  if pos < Array.length t.tokens then
    Some t.tokens.(pos)
  else
    None

let next t =
  match peek t with
  | None -> None
  | Some tok -> 
      Some (tok, { t with position = t.position + 1 })

let check t pred =
  match peek t with
  | None -> false
  | Some tok -> pred tok

let parse_token t expected_kind =
  match peek t with
  | Some tok when tok.Token.kind = expected_kind ->
      Ok (tok, { t with position = t.position + 1 })
  | Some tok ->
      Error {
        message = "Expected token";
        position = tok.Token.span.start;
        expected = None;
      }
  | None ->
      Error {
        message = "Unexpected end of input";
        position = t.position;
        expected = None;
      }

let parse_keyword t expected =
  match peek t with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Keyword kw when kw = expected ->
          Ok (kw, { t with position = t.position + 1 })
      | _ ->
          Error {
            message = "Expected keyword";
            position = tok.Token.span.start;
            expected = None;
          })
  | None ->
      Error {
        message = "Unexpected end of input";
        position = t.position;
        expected = None;
      }

let parse_ident t =
  match peek t with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Ident name ->
          Ok (name, { t with position = t.position + 1 })
      | _ ->
          Error {
            message = "Expected identifier";
            position = tok.Token.span.start;
            expected = None;
          })
  | None ->
      Error {
        message = "Unexpected end of input";
        position = t.position;
        expected = None;
      }

let checkpoint t = t
let fork t = t

let span start_pos t = 
  start_pos, t.position

let with_error t error =
  { t with errors = error :: t.errors }

let errors t = List.rev t.errors