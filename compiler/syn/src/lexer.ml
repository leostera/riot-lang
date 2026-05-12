open Std
open Std.Collections
open Std.IO

type t = Cursor.t

let create = fun source -> Cursor.create source

let make_token = fun ~kind ~span -> { Token.kind; span; leading_trivia = [] }

let int_of_string_opt = Int.parse

let float_of_string_opt = Float.parse

let is_whitespace = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\t'
  | '\n'
  | '\r' -> true
  | _ -> false

let is_ident_start = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_' -> true
  | _ -> false

let is_ident_continue = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '\'' -> true
  | _ -> false

let is_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9' -> true
  | _ -> false

let is_digit_or_underscore = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9'
  | '_' -> true
  | _ -> false

let is_alpha = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let is_hex_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F' -> true
  | _ -> false

let is_hex_digit_or_underscore = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '9'
  | 'a' .. 'f'
  | 'A' .. 'F'
  | '_' -> true
  | _ -> false

let is_octal_digit = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '7' -> true
  | _ -> false

let is_octal_digit_or_underscore = fun __tmp1 ->
  match __tmp1 with
  | '0' .. '7'
  | '_' -> true
  | _ -> false

let is_binary_digit = fun __tmp1 ->
  match __tmp1 with
  | '0'
  | '1' -> true
  | _ -> false

let is_binary_digit_or_underscore = fun __tmp1 ->
  match __tmp1 with
  | '0'
  | '1'
  | '_' -> true
  | _ -> false

type quoted_string_info = { pipe_offset: int; delimiter: string; is_extension: bool }

let is_quoted_string_header_char = fun __tmp1 ->
  match __tmp1 with
  | '%' -> true
  | c -> is_ident_continue c

let trim_right = fun text ->
  let rec loop i =
    if i < 0 then
      ""
    else
      match String.get_unchecked text ~at:i with
      | ' '
      | '\t'
      | '\n'
      | '\r' -> loop (i - 1)
      | _ -> String.sub text ~offset:0 ~len:(i + 1)
  in
  loop (String.length text - 1)

let delimiter_of_quoted_string_header = fun header ~is_extension ->
  if not is_extension then
    header
  else
    let trimmed = trim_right header in
    let rec find_last_space i =
      if i < 0 then
        None
      else
        match String.get_unchecked trimmed ~at:i with
        | ' '
        | '\t'
        | '\n'
        | '\r' -> Some i
        | _ -> find_last_space (i - 1)
    in
    match find_last_space (String.length trimmed - 1) with
    | Some i when i + 1 < String.length trimmed ->
        String.sub trimmed ~offset:(i + 1) ~len:(String.length trimmed - i - 1)
    | _ -> ""

let rec find_quoted_string_pipe_offset = fun cursor offset ~is_extension ->
  match Cursor.peek_n cursor offset with
  | Some '|' -> Some offset
  | Some c when is_extension && (is_quoted_string_header_char c || is_whitespace c) ->
      find_quoted_string_pipe_offset cursor (offset + 1) ~is_extension
  | Some c when is_quoted_string_header_char c ->
      find_quoted_string_pipe_offset cursor (offset + 1) ~is_extension
  | Some _ -> None
  | None -> None

let delimiter_matches_after_pipe = fun cursor delimiter ->
  let rec loop index =
    if index = String.length delimiter then
      match Cursor.peek_n cursor (index + 1) with
      | Some '}' -> true
      | _ -> false
    else
      match Cursor.peek_n cursor (index + 1) with
      | Some c when c = String.get_unchecked delimiter ~at:index -> loop (index + 1)
      | _ -> false
  in
  loop 0

let quoted_string_info_at_cursor = fun cursor ->
  match Cursor.peek cursor with
  | Some '{' -> (
      let (header_start, is_extension) =
        match Cursor.peek_n cursor 1 with
        | Some '|' -> (1, false)
        | Some '%' -> (
            match Cursor.peek_n cursor 2 with
            | Some '%' -> (3, true)
            | _ -> (2, true)
          )
        | Some c when is_ident_continue c -> (1, false)
        | _ -> (0, false)
      in
      if header_start = 0 then
        None
      else
        match find_quoted_string_pipe_offset cursor header_start ~is_extension with
        | None -> None
        | Some pipe_offset ->
            let header =
              if pipe_offset = header_start then
                ""
              else
                Cursor.slice
                  cursor
                  (Cursor.position cursor + header_start)
                  (pipe_offset - header_start)
            in
            let delimiter = delimiter_of_quoted_string_header header ~is_extension in
            if is_extension && pipe_offset = header_start then
              None
            else
              Some { pipe_offset; delimiter; is_extension }
    )
  | _ -> None

let lex_whitespace = fun cursor start ->
  Cursor.skip_while cursor is_whitespace;
  let end_ = Cursor.position cursor in
  make_token ~kind:Token.Whitespace ~span:(Span.make ~start ~end_)

let try_skip_quoted_string = fun cursor ->
  match quoted_string_info_at_cursor cursor with
  | None -> false
  | Some { pipe_offset; delimiter; _ } ->
      for _ = 0 to pipe_offset do
        Cursor.advance cursor
      done;
      let rec skip_body () =
        match Cursor.peek cursor with
        | None -> ()
        | Some '|' when delimiter_matches_after_pipe cursor delimiter ->
            Cursor.advance cursor;
            for _ = 0 to String.length delimiter - 1 do
              Cursor.advance cursor
            done;
            Cursor.advance cursor
        | Some _ ->
            Cursor.advance cursor;
            skip_body ()
      in
      skip_body ();
      true

let rec lex_block_comment = fun cursor depth content_start token_start ->
  match Cursor.peek cursor with
  | None ->
      let value = Cursor.slice cursor content_start (Cursor.position cursor - content_start) in
      let end_ = Cursor.position cursor in
      make_token
        ~kind:(Token.Comment { value; terminated = false })
        ~span:{ start = token_start; end_ }
  | Some '(' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some '*' ->
          Cursor.advance cursor;
          lex_block_comment cursor (depth + 1) content_start token_start
      | _ -> lex_block_comment cursor depth content_start token_start
    )
  | Some '*' -> (
      Cursor.advance cursor;
      match Cursor.peek cursor with
      | Some ')' ->
          Cursor.advance cursor;
          if depth = 1 then
            let value =
              Cursor.slice cursor content_start (Cursor.position cursor - content_start - 2)
            in
            let end_ = Cursor.position cursor in
            make_token
              ~kind:(Token.Comment { value; terminated = true })
              ~span:{ start = token_start; end_ }
          else
            lex_block_comment cursor (depth - 1) content_start token_start
      | _ -> lex_block_comment cursor depth content_start token_start
    )
  | Some '{' ->
      if try_skip_quoted_string cursor then
        lex_block_comment cursor depth content_start token_start
      else (
        Cursor.advance cursor;
        lex_block_comment cursor depth content_start token_start
      )
  | Some _ ->
      Cursor.advance cursor;
      lex_block_comment cursor depth content_start token_start

let lex_comment = fun cursor token_start ->
  Cursor.advance cursor;
  (* skip '(' *)
  Cursor.advance cursor;
  (* skip '*' *)
  (* Check if it's a docstring *)
  let is_docstring =
    match Cursor.peek cursor with
    | Some '*' when match Cursor.peek_n cursor 1 with
    | Some ')'
    | Some '*' -> false
    | _ -> true ->
        Cursor.advance cursor;
        (* skip the second '*' for docstrings *)
        true
    | _ -> false
  in
  let content_start = Cursor.position cursor in
  let rec lex_content depth =
    match Cursor.peek cursor with
    | None ->
        let value = Cursor.slice cursor content_start (Cursor.position cursor - content_start) in
        let end_ = Cursor.position cursor in
        if is_docstring then
          make_token
            ~kind:(Token.Docstring { value; terminated = false })
            ~span:{ start = token_start; end_ }
        else
          make_token
            ~kind:(Token.Comment { value; terminated = false })
            ~span:{ start = token_start; end_ }
    | Some '(' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '*' ->
            Cursor.advance cursor;
            lex_content (depth + 1)
        | _ -> lex_content depth
      )
    | Some '*' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ')' ->
            Cursor.advance cursor;
            if depth = 1 then
              let value =
                Cursor.slice cursor content_start (Cursor.position cursor - content_start - 2)
              in
              let end_ = Cursor.position cursor in
              if is_docstring then
                make_token
                  ~kind:(Token.Docstring { value; terminated = true })
                  ~span:{ start = token_start; end_ }
              else
                make_token
                  ~kind:(Token.Comment { value; terminated = true })
                  ~span:{ start = token_start; end_ }
            else
              lex_content (depth - 1)
        | _ -> lex_content depth
      )
    | Some '{' ->
        if try_skip_quoted_string cursor then
          lex_content depth
        else (
          Cursor.advance cursor;
          lex_content depth
        )
    | Some _ ->
        Cursor.advance cursor;
        lex_content depth
  in
  lex_content 1

let lex_ident = fun cursor delim_stack token_start ->
  let start = Cursor.position cursor in
  Cursor.skip_while cursor is_ident_continue;
  let len = Cursor.position cursor - start in
  let ident = Cursor.slice cursor start len in
  let end_ = Cursor.position cursor in
  let kind =
    if ident = "_" then
      Token.Underscore
    else
      match Keyword.from_string ident with
      | Some kw -> (
          if Keyword.is_opening kw then
            let delim =
              Token.delimiter_of_keyword kw
              |> Option.unwrap
            in
            Token.OpenDelim delim
          else if Keyword.is_closing kw then
            match delim_stack with
            | d :: _ -> Token.CloseDelim d
            | [] -> Token.CloseDelim BeginEnd
          (* Default fallback *)
          else
            (* Operator keywords can be used as identifiers in bindings like: let lnot = ... *)
            (* Treat them as identifiers to simplify parsing *)
            match kw with
            | Lnot
            | Land
            | Lor
            | Lxor
            | Lsl
            | Lsr
            | Asr
            | Mod -> Token.Ident ident
            | _ -> Token.Keyword kw
        )
      | None -> Token.Ident ident
  in
  make_token ~kind ~span:{ start = token_start; end_ }

let lex_raw_ident = fun cursor token_start ->
  Cursor.advance cursor;
  Cursor.advance cursor;
  let start = Cursor.position cursor in
  Cursor.skip_while cursor is_ident_continue;
  let len = Cursor.position cursor - start in
  let ident = Cursor.slice cursor start len in
  let end_ = Cursor.position cursor in
  make_token ~kind:(Token.Ident ("\\#" ^ ident)) ~span:{ start = token_start; end_ }

let lex_number = fun cursor token_start ->
  (* Helper to remove underscores from a string *)
  let remove_underscores s =
    let buf = Buffer.create ~size:(String.length s) in
    String.for_each
      s
      ~fn:(fun c ->
        if c != '_' then
          Buffer.add_char buf c);
    Buffer.contents buf
  in
  let consume_numeric_suffix () =
    match Cursor.peek cursor with
    | Some c when is_alpha c ->
        Cursor.advance cursor;
        true
    | _ -> false
  in
  let exponent_follows () =
    match Cursor.peek cursor with
    | Some ('e' | 'E') -> (
        match Cursor.peek_n cursor 1 with
        | Some ('+' | '-') -> (
            match Cursor.peek_n cursor 2 with
            | Some c when is_digit c -> true
            | _ -> false
          )
        | Some c when is_digit c -> true
        | _ -> false
      )
    | _ -> false
  in
  let consume_float_exponent () =
    if exponent_follows () then
      let start = Cursor.position cursor in
      let _ = Cursor.advance cursor in
      let _ =
        match Cursor.peek cursor with
        | Some ('+' | '-') -> Cursor.advance cursor
        | _ -> ()
      in
      let _ = Cursor.take_while cursor is_digit_or_underscore in
      Some (
        Cursor.slice cursor start (Cursor.position cursor - start)
        |> remove_underscores
      )
    else
      None
  in
  (* Check if this is a hex/octal/binary literal: 0x, 0o, 0b *)
  (* At this point, cursor is AT the first digit (not consumed yet) *)
  match (Cursor.peek cursor, Cursor.peek_n cursor 1) with
  | (Some '0', Some ('x' | 'X')) ->
      Cursor.advance cursor;
      (* consume '0' *)
      Cursor.advance cursor;
      (* consume 'x' *)
      let hex_digits_raw = Cursor.take_while cursor is_hex_digit_or_underscore in
      let hex_digits = remove_underscores hex_digits_raw in
      let _ = consume_numeric_suffix () in
      let hex_str = "0x" ^ hex_digits in
      let kind =
        match Int.parse hex_str with
        | Some i -> Token.Literal (Int i)
        | None -> Token.Literal (Int 0)
      in
      make_token ~kind ~span:(Span.make ~start:token_start ~end_:(Cursor.position cursor))
  | (Some '0', Some ('o' | 'O')) ->
      Cursor.advance cursor;
      (* consume '0' *)
      Cursor.advance cursor;
      (* consume 'o' *)
      let octal_digits_raw = Cursor.take_while cursor is_octal_digit_or_underscore in
      let octal_digits = remove_underscores octal_digits_raw in
      let _ = consume_numeric_suffix () in
      let octal_str = "0o" ^ octal_digits in
      let kind =
        match Int.parse octal_str with
        | Some i -> Token.Literal (Int i)
        | None -> Token.Literal (Int 0)
      in
      make_token ~kind ~span:(Span.make ~start:token_start ~end_:(Cursor.position cursor))
  | (Some '0', Some ('b' | 'B')) ->
      Cursor.advance cursor;
      (* consume '0' *)
      Cursor.advance cursor;
      (* consume 'b' *)
      let binary_digits_raw = Cursor.take_while cursor is_binary_digit_or_underscore in
      let binary_digits = remove_underscores binary_digits_raw in
      let _ = consume_numeric_suffix () in
      let binary_str = "0b" ^ binary_digits in
      let kind =
        match Int.parse binary_str with
        | Some i -> Token.Literal (Int i)
        | None -> Token.Literal (Int 0)
      in
      make_token ~kind ~span:(Span.make ~start:token_start ~end_:(Cursor.position cursor))
  | _ ->
      let num_str_raw = Cursor.take_while cursor is_digit_or_underscore in
      (* Remove underscores for parsing *)
      let num_str = remove_underscores num_str_raw in
      let kind =
        match Cursor.peek cursor with
        | Some ('e' | 'E') when exponent_follows () -> (
            let exponent = Option.unwrap_or (consume_float_exponent ()) ~default:"" in
            let _ = consume_numeric_suffix () in
            let float_str = num_str ^ exponent in
            match float_of_string_opt float_str with
            | Some f -> Token.Literal (Float f)
            | None -> Token.Literal (Float 0.0)
          )
        | Some c when is_alpha c ->
            let _ = consume_numeric_suffix () in
            Token.Literal (Int 0)
        | Some '.' -> (
            match Cursor.peek_n cursor 1 with
            | Some c when is_digit c -> (
                Cursor.advance cursor;
                let frac_raw = Cursor.take_while cursor is_digit_or_underscore in
                let frac = remove_underscores frac_raw in
                let exponent = Option.unwrap_or (consume_float_exponent ()) ~default:"" in
                let _ = consume_numeric_suffix () in
                let float_str = num_str ^ "." ^ frac ^ exponent in
                match float_of_string_opt float_str with
                | Some f -> Token.Literal (Float f)
                | None -> Token.Literal (Float 0.0)
              )
            | Some '.' -> (
                match int_of_string_opt num_str with
                | Some i -> Token.Literal (Int i)
                | None -> Token.Literal (Int 0)
              )
            | Some c when is_alpha c -> (
                Cursor.advance cursor;
                let exponent =
                  if exponent_follows () then
                    Option.unwrap_or (consume_float_exponent ()) ~default:""
                  else
                    ""
                in
                let _ = consume_numeric_suffix () in
                let float_str = num_str ^ "." ^ exponent in
                match float_of_string_opt float_str with
                | Some f -> Token.Literal (Float f)
                | None -> Token.Literal (Float 0.0)
              )
            | _ -> (
                Cursor.advance cursor;
                let exponent = Option.unwrap_or (consume_float_exponent ()) ~default:"" in
                let _ = consume_numeric_suffix () in
                let float_str = num_str ^ "." ^ exponent in
                match float_of_string_opt float_str with
                | Some f -> Token.Literal (Float f)
                | None -> Token.Literal (Float 0.0)
              )
          )
        | _ -> (
            match int_of_string_opt num_str with
            | Some i -> Token.Literal (Int i)
            | None -> Token.Literal (Int 0)
          )
      in
      make_token ~kind ~span:(Span.make ~start:token_start ~end_:(Cursor.position cursor))

let lex_string = fun cursor token_start ->
  Cursor.advance cursor;
  let start = Cursor.position cursor in
  let rec loop () =
    match Cursor.peek cursor with
    | None -> (Cursor.slice cursor start (Cursor.position cursor - start), false)
    | Some '\\' ->
        Cursor.advance cursor;
        Cursor.advance cursor;
        loop ()
    | Some '"' ->
        let value = Cursor.slice cursor start (Cursor.position cursor - start) in
        Cursor.advance cursor;
        (value, true)
    | Some _ ->
        Cursor.advance cursor;
        loop ()
  in
  let (value, terminated) = loop () in
  let end_ = Cursor.position cursor in
  make_token
    ~kind:(Token.Literal (String { value; terminated }))
    ~span:{ start = token_start; end_ }

let lex_quoted_string = fun cursor token_start ->
  let rec find_pipe_offset offset =
    match Cursor.peek_n cursor offset with
    | Some '|' -> Some offset
    | Some c when is_ident_continue c -> find_pipe_offset (offset + 1)
    | _ -> None
  in
  match find_pipe_offset 1 with
  | Some pipe_offset ->
      let delimiter =
        if pipe_offset = 1 then
          ""
        else
          Cursor.slice cursor (Cursor.position cursor + 1) (pipe_offset - 1)
      in
      for _ = 0 to pipe_offset do
        Cursor.advance cursor
      done;
      let start = Cursor.position cursor in
      let rec loop () =
        match Cursor.peek cursor with
        | None -> (Cursor.slice cursor start (Cursor.position cursor - start), false)
        | Some '|' when delimiter_matches_after_pipe cursor delimiter ->
            let value = Cursor.slice cursor start (Cursor.position cursor - start) in
            Cursor.advance cursor;
            for _ = 0 to String.length delimiter - 1 do
              Cursor.advance cursor
            done;
            Cursor.advance cursor;
            (value, true)
        | Some _ ->
            Cursor.advance cursor;
            loop ()
      in
      let (value, terminated) = loop () in
      let end_ = Cursor.position cursor in
      make_token
        ~kind:(Token.Literal (String { value; terminated }))
        ~span:{ start = token_start; end_ }
  | None ->
      Cursor.advance cursor;
      let end_ = Cursor.position cursor in
      make_token ~kind:(Token.OpenDelim Brace) ~span:(Span.make ~start:token_start ~end_)

let lex_char = fun cursor token_start ->
  Cursor.advance cursor;
  let parse_escape_sequence () =
    Cursor.advance cursor;
    (* Skip the backslash *)
    match Cursor.peek cursor with
    | None -> None
    | Some 'n' ->
        Cursor.advance cursor;
        Some '\n'
    | Some 't' ->
        Cursor.advance cursor;
        Some '\t'
    | Some 'r' ->
        Cursor.advance cursor;
        Some '\r'
    | Some 'b' ->
        Cursor.advance cursor;
        Some '\b'
    | Some '\\' ->
        Cursor.advance cursor;
        Some '\\'
    | Some '\'' ->
        Cursor.advance cursor;
        Some '\''
    | Some '"' ->
        Cursor.advance cursor;
        Some '"'
    | Some ' ' ->
        Cursor.advance cursor;
        Some ' '
    | Some ('0' .. '9' as c) -> (
        let d1 = Char.code c - Char.code '0' in
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ('0' .. '9' as c2) -> (
            let d2 = Char.code c2 - Char.code '0' in
            Cursor.advance cursor;
            match Cursor.peek cursor with
            | Some ('0' .. '9' as c3) ->
                let d3 = Char.code c3 - Char.code '0' in
                Cursor.advance cursor;
                let code = (d1 * 64) + (d2 * 8) + d3 in
                if code <= 255 then
                  Some (Char.from_int_unchecked code)
                else
                  None
            | _ -> Some (Char.from_int_unchecked ((d1 * 8) + d2))
          )
        | _ -> Some (Char.from_int_unchecked d1)
      )
    | Some 'x' -> (
        Cursor.advance cursor;
        let hex_to_int c =
          match c with
          | '0' .. '9' -> Some (Char.code c - Char.code '0')
          | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
          | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
          | _ -> None
        in
        match Cursor.peek cursor with
        | Some c1 -> (
            match hex_to_int c1 with
            | Some h1 -> (
                Cursor.advance cursor;
                match Cursor.peek cursor with
                | Some c2 -> (
                    match hex_to_int c2 with
                    | Some h2 ->
                        Cursor.advance cursor;
                        Some (Char.from_int_unchecked ((h1 * 16) + h2))
                    | None -> None
                  )
                | None -> None
              )
            | None -> None
          )
        | None -> None
      )
    | Some c ->
        Cursor.advance cursor;
        Some c
  in
  let kind =
    match Cursor.peek cursor with
    | None -> Token.Unknown '\''
    | Some '\\' -> (
        match parse_escape_sequence () with
        | Some char_value -> (
            match Cursor.peek cursor with
            | Some '\'' ->
                Cursor.advance cursor;
                Token.Literal (Char char_value)
            | _ -> Token.Unknown '\''
          )
        | None -> Token.Unknown '\''
      )
    | Some c -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '\'' ->
            Cursor.advance cursor;
            Token.Literal (Char c)
        | _ -> Token.Unknown '\''
      )
  in
  let end_ = Cursor.position cursor in
  make_token ~kind ~span:{ start = token_start; end_ }

let lex_type_var = fun cursor token_start ->
  (* Type variable: 'ident
     The quote has already been seen, now consume it and return Quote token.
     The next call to `next` will lex the identifier.
  *)
  Cursor.advance cursor;
  let end_ = Cursor.position cursor in
  make_token ~kind:Token.Quote ~span:{ start = token_start; end_ }

let next = fun cursor delim_stack ->
  let start = Cursor.position cursor in
  if Cursor.is_eof cursor then
    make_token ~kind:Token.EOF ~span:{ start; end_ = start }
  else
    match Cursor.peek cursor with
    | None -> make_token ~kind:Token.EOF ~span:{ start; end_ = start }
    | Some c when is_whitespace c -> lex_whitespace cursor start
    | Some '(' -> (
        match Cursor.peek_n cursor 1 with
        | Some '*' -> lex_comment cursor start
        | _ ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.OpenDelim Paren) ~span:(Span.make ~start ~end_)
      )
    | Some ')' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:(Token.CloseDelim Paren) ~span:(Span.make ~start ~end_)
    | Some '[' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '|' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.OpenDelim Array) ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.OpenDelim Bracket) ~span:(Span.make ~start ~end_)
      )
    | Some ']' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:(Token.CloseDelim Bracket) ~span:(Span.make ~start ~end_)
    | Some '{' -> (
        match Cursor.peek_n cursor 1 with
        | Some '|' -> lex_quoted_string cursor start
        | Some c when is_ident_continue c -> lex_quoted_string cursor start
        | _ ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.OpenDelim Brace) ~span:(Span.make ~start ~end_)
      )
    | Some '}' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:(Token.CloseDelim Brace) ~span:(Span.make ~start ~end_)
    | Some '+' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.PlusDot ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Plus ~span:(Span.make ~start ~end_)
      )
    | Some '-' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Arrow ~span:(Span.make ~start ~end_)
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.MinusDot ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Minus ~span:(Span.make ~start ~end_)
      )
    | Some '*' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '*' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.StarStar ~span:(Span.make ~start ~end_)
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.StarDot ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Star ~span:(Span.make ~start ~end_)
      )
    | Some '/' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.SlashDot ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Slash ~span:(Span.make ~start ~end_)
      )
    | Some '%' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.PercentGt ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Percent ~span:(Span.make ~start ~end_)
      )
    | Some '^' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Caret ~span:(Span.make ~start ~end_)
    | Some '=' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.FatArrow ~span:(Span.make ~start ~end_)
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.EqEq ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Eq ~span:(Span.make ~start ~end_)
      )
    | Some '<' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.LtEq ~span:(Span.make ~start ~end_)
        | Some '-' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.LeftArrow ~span:(Span.make ~start ~end_)
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Ne ~span:(Span.make ~start ~end_)
        | Some '%' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.LtPercent ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Lt ~span:(Span.make ~start ~end_)
      )
    | Some '>' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.GtEq ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Gt ~span:(Span.make ~start ~end_)
      )
    | Some '!' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.BangEq ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Bang ~span:(Span.make ~start ~end_)
      )
    | Some '&' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '&' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.And ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Ampersand ~span:(Span.make ~start ~end_)
      )
    | Some '|' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '|' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Or ~span:(Span.make ~start ~end_)
        | Some '>' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.PipeGt ~span:(Span.make ~start ~end_)
        | Some ']' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.CloseDelim Array) ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Pipe ~span:(Span.make ~start ~end_)
      )
    | Some ':' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some ':' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.ColonColon ~span:(Span.make ~start ~end_)
        | Some '=' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.ColonEq ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Colon ~span:(Span.make ~start ~end_)
      )
    | Some ';' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Semi ~span:(Span.make ~start ~end_)
    | Some ',' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Comma ~span:(Span.make ~start ~end_)
    | Some '.' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '.' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.DotDot ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.Dot ~span:(Span.make ~start ~end_)
      )
    | Some '?' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Question ~span:(Span.make ~start ~end_)
    | Some '@' -> (
        Cursor.advance cursor;
        match Cursor.peek cursor with
        | Some '@' ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.AtAt ~span:(Span.make ~start ~end_)
        | _ ->
            let end_ = Cursor.position cursor in
            make_token ~kind:Token.At ~span:(Span.make ~start ~end_)
      )
    | Some '#' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Hash ~span:(Span.make ~start ~end_)
    | Some '~' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Tilde ~span:(Span.make ~start ~end_)
    | Some '`' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Backtick ~span:(Span.make ~start ~end_)
    | Some '$' ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:Token.Dollar ~span:(Span.make ~start ~end_)
    | Some '"' -> lex_string cursor start
    | Some '\\' -> (
        match Cursor.peek_n cursor 1 with
        | Some '#' -> (
            match Cursor.peek_n cursor 2 with
            | Some c when is_ident_start c -> lex_raw_ident cursor start
            | _ ->
                Cursor.advance cursor;
                let end_ = Cursor.position cursor in
                make_token ~kind:(Token.Unknown '\\') ~span:(Span.make ~start ~end_)
          )
        | _ ->
            Cursor.advance cursor;
            let end_ = Cursor.position cursor in
            make_token ~kind:(Token.Unknown '\\') ~span:(Span.make ~start ~end_)
      )
    | Some '\'' -> (
        match Cursor.peek_n cursor 1 with
        | Some c when is_ident_start c || c = '_' -> (
            match Cursor.peek_n cursor 2 with
            | Some '\'' -> lex_char cursor start
            | _ -> lex_type_var cursor start
          )
        | Some '\\' -> (
            match (Cursor.peek_n cursor 2, Cursor.peek_n cursor 3) with
            | (Some '#', Some c) when is_ident_start c -> lex_type_var cursor start
            | _ -> lex_char cursor start
          )
        | _ -> lex_char cursor start
      )
    | Some c when is_digit c -> lex_number cursor start
    | Some c when is_ident_start c -> lex_ident cursor delim_stack start
    | Some c ->
        Cursor.advance cursor;
        let end_ = Cursor.position cursor in
        make_token ~kind:(Token.Unknown c) ~span:(Span.make ~start ~end_)

let tokenize_cursor = fun cursor ->
  let attach_pending_trivia token pending_rev =
    Token.with_leading_trivia
      token
      (
        let pending_count = List.length pending_rev in
        let leading = token.Token.leading_trivia in
        let trivia = Vector.with_capacity ~size:(pending_count + List.length leading) in
        let rec push_pending = fun __tmp1 ->
          match __tmp1 with
          | [] -> ()
          | item :: rest ->
              push_pending rest;
              Vector.push trivia ~value:item
        in
        push_pending pending_rev;
        List.for_each leading ~fn:(fun item -> Vector.push trivia ~value:item);
        Array.to_list (Vector.to_array trivia)
      )
  in
  let rec lex_all delim_stack pending_trivia_rev acc =
    yield ();
    let token = next cursor delim_stack in
    let new_stack =
      match token.Token.kind with
      | Token.OpenDelim d -> d :: delim_stack
      | Token.CloseDelim _ -> (
          match delim_stack with
          | _ :: rest -> rest
          | [] -> delim_stack
        )
      | _ -> delim_stack
    in
    match Token.trivia_of_token token with
    | Some trivia -> lex_all new_stack (trivia :: pending_trivia_rev) acc
    | None -> (
        let token = attach_pending_trivia token pending_trivia_rev in
        match token.Token.kind with
        | Token.EOF -> List.reverse (token :: acc)
        | _ -> lex_all new_stack [] (token :: acc)
      )
  in
  lex_all [] [] []

let tokenize = fun source -> tokenize_cursor (create source)
