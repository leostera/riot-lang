module EmailMessage = Message

open Std

let contains_substring haystack needle =
  let rec search pos =
    if pos > String.length haystack - String.length needle then false
    else if String.sub haystack pos (String.length needle) = needle then true
    else search (pos + 1)
  in
  if needle = "" then true else search 0

type t =
  | All
  | HasAttachment
  | From of string
  | To of string
  | Subject of string
  | Contains of string
  | And of t * t
  | Or of t * t
  | Maybe of t

let tokenize str =
  let len = String.length str in
  let rec aux pos acc current_token in_quotes =
    if pos >= len then
      let acc = if current_token != "" then current_token :: acc else acc in
      List.rev acc
    else
      let c = str.[pos] in
      match c with
      | '\'' when not in_quotes -> aux (pos + 1) acc current_token true
      | '\'' when in_quotes -> aux (pos + 1) acc current_token false
      | (' ' | '\t') when not in_quotes ->
          let acc = if current_token != "" then current_token :: acc else acc in
          aux (pos + 1) acc "" false
      | '(' when not in_quotes ->
          let acc = if current_token != "" then current_token :: acc else acc in
          aux (pos + 1) ("(" :: acc) "" false
      | ')' when not in_quotes ->
          let acc = if current_token != "" then current_token :: acc else acc in
          aux (pos + 1) (")" :: acc) "" false
      | _ -> aux (pos + 1) acc (current_token ^ String.make 1 c) in_quotes
  in
  aux 0 [] "" false

let rec parse_expr tokens =
  let left, rest = parse_term tokens in
  parse_expr_rest left rest

and parse_expr_rest left tokens =
  match tokens with
  | "OR" :: rest ->
      let right, rest = parse_term rest in
      parse_expr_rest (Or (left, right)) rest
  | _ -> (left, tokens)

and parse_term tokens =
  let left, rest = parse_factor tokens in
  parse_term_rest left rest

and parse_term_rest left tokens =
  match tokens with
  | "AND" :: rest ->
      let right, rest = parse_factor rest in
      parse_term_rest (And (left, right)) rest
  | _ -> (left, tokens)

and parse_factor tokens =
  match tokens with
  | "?" :: rest ->
      let inner, rest = parse_factor rest in
      (Maybe inner, rest)
  | "(" :: rest -> (
      let expr, rest = parse_expr rest in
      match rest with ")" :: rest -> (expr, rest) | _ -> (expr, rest))
  | token :: rest ->
      if token = "has:attachment" then (HasAttachment, rest)
      else if String.starts_with ~prefix:"from:" token then
        (From (String.sub token 5 (String.length token - 5)), rest)
      else if String.starts_with ~prefix:"to:" token then
        (To (String.sub token 3 (String.length token - 3)), rest)
      else if String.starts_with ~prefix:"subject:" token then
        (Subject (String.sub token 8 (String.length token - 8)), rest)
      else if String.starts_with ~prefix:"contains:" token then
        (Contains (String.sub token 9 (String.length token - 9)), rest)
      else if String.starts_with ~prefix:"?from:" token then
        (Maybe (From (String.sub token 6 (String.length token - 6))), rest)
      else if String.starts_with ~prefix:"?to:" token then
        (Maybe (To (String.sub token 4 (String.length token - 4))), rest)
      else if String.starts_with ~prefix:"?subject:" token then
        (Maybe (Subject (String.sub token 9 (String.length token - 9))), rest)
      else if String.starts_with ~prefix:"?contains:" token then
        (Maybe (Contains (String.sub token 10 (String.length token - 10))), rest)
      else (All, rest)
  | [] -> (All, [])

let parse str =
  let str = String.trim str in
  if str = "" then Ok All
  else
    try
      let tokens = tokenize str in
      let query, _rest = parse_expr tokens in
      Ok query
    with _ -> Error "Failed to parse query"

let rec matches query msg =
  match query with
  | All -> true
  | HasAttachment ->
      let content = EmailMessage.body msg in
      contains_substring content "Content-Disposition: attachment"
  | From addr ->
      let headers = EmailMessage.headers msg in
      List.exists
        (fun (name, value) ->
          String.lowercase_ascii name = "from" && contains_substring value addr)
        headers
  | To addr ->
      let headers = EmailMessage.headers msg in
      List.exists
        (fun (name, value) ->
          String.lowercase_ascii name = "to" && contains_substring value addr)
        headers
  | Subject subj ->
      let headers = EmailMessage.headers msg in
      List.exists
        (fun (name, value) ->
          String.lowercase_ascii name = "subject"
          && contains_substring value subj)
        headers
  | Contains text ->
      let body = EmailMessage.body msg in
      contains_substring body text
  | And (q1, q2) -> matches q1 msg && matches q2 msg
  | Or (q1, q2) -> matches q1 msg || matches q2 msg
  | Maybe q -> matches q msg

let matches_entry query entry = matches query entry.Mbox.message
