open Std
open Types

type state = {
  content: string;
  len: int;
  mutable index: int;
  mutable line: int;
  mutable values: (string * string option) list;
}

let bom = "\239\187\191"

let parse_error = fun line message -> Error (ParseError { line; message })

let emit_parse_result = fun result ->
  match result with
  | Ok bindings -> Telemetry.emit (Events.Parsed { binding_count = List.length bindings })
  | Error (ParseError { line; message }) -> Telemetry.emit (Events.ParseFailed { line; message })
  | Error _ -> ()

let char_to_string = fun char -> String.make ~len:1 ~char

let is_horizontal_space = fun char ->
  match char with
  | ' '
  | '\t' -> true
  | _ -> false

let char_between = fun char lower upper ->
  if Char.compare char lower = Order.LT then
    false
  else
    Char.compare char upper != Order.GT

let is_name_start = fun char ->
  if Char.equal char '_' then
    true
  else if char_between char 'A' 'Z' then
    true
  else
    char_between char 'a' 'z'

let is_name_char = fun char ->
  if is_name_start char then
    true
  else if char_between char '0' '9' then
    true
  else
    Char.equal char '.'

let is_unbraced_substitution_char = fun char ->
  if is_name_start char then
    true
  else
    char_between char '0' '9'

let strip_bom = fun content ->
  if String.starts_with ~prefix:bom content then
    String.sub content ~offset:(String.length bom) ~len:(String.length content - String.length bom)
  else
    content

let normalize = fun content ->
  let content = strip_bom content in
  let len = String.length content in
  let buffer = IO.Buffer.create ~size:len in
  let rec loop index =
    if index >= len then
      IO.Buffer.contents buffer
    else
      let char = String.unsafe_get content index in
      if Char.equal char '\r' then (
        IO.Buffer.add_char buffer '\n';
        if index + 1 < len && Char.equal (String.unsafe_get content (index + 1)) '\n' then
          loop (index + 2)
        else
          loop (index + 1)
      ) else (
        IO.Buffer.add_char buffer char;
        loop (index + 1)
      )
  in
  loop 0

let make_state = fun content ->
  let content = normalize content in
  {
    content;
    len = String.length content;
    index = 0;
    line = 1;
    values = [];
  }

let current = fun state ->
  if state.index >= state.len then
    None
  else
    Some (String.unsafe_get state.content state.index)

let advance = fun state ->
  let char = String.unsafe_get state.content state.index in
  state.index <- state.index + 1;
  if Char.equal char '\n' then
    state.line <- state.line + 1;
  char

let skip_horizontal = fun state ->
  let rec loop () =
    match current state with
    | Some char when is_horizontal_space char ->
        ignore (advance state);
        loop ()
    | _ -> ()
  in
  loop ()

let skip_to_line_end = fun state ->
  let rec loop () =
    match current state with
    | None -> ()
    | Some '\n' -> ignore (advance state)
    | Some _ ->
        ignore (advance state);
        loop ()
  in
  loop ()

let skip_ignored_lines = fun state ->
  let rec loop () =
    skip_horizontal state;
    match current state with
    | Some '\n' ->
        ignore (advance state);
        loop ()
    | Some '#' ->
        skip_to_line_end state;
        loop ()
    | _ -> ()
  in
  loop ()

let lookup_value = fun state key ->
  match Env.var Env.String ~name:key with
  | Some value -> value
  | None ->
      let rec loop values =
        match values with
        | [] -> ""
        | (name, value) :: rest ->
            if String.equal name key then
              match value with
              | Some value -> value
              | None -> ""
            else
              loop rest
      in
      loop state.values

let has_value = fun state key ->
  match Env.var Env.String ~name:key with
  | Some _ -> true
  | None ->
      let rec loop values =
        match values with
        | [] -> false
        | (name, _) :: rest ->
            if String.equal name key then
              true
            else
              loop rest
      in
      loop state.values

let parse_key = fun state line ->
  match current state with
  | Some char when is_name_start char ->
      let start = state.index in
      ignore (advance state);
      let rec loop () =
        match current state with
        | Some char when is_name_char char ->
            ignore (advance state);
            loop ()
        | _ -> Ok (String.sub state.content ~offset:start ~len:(state.index - start))
      in
      loop ()
  | Some char -> parse_error line ("invalid variable name near " ^ char_to_string char)
  | None -> parse_error line "expected variable name"

let escaped_char = fun line char ->
  match char with
  | 'n' -> Ok '\n'
  | 'r' -> Ok '\r'
  | 't' -> Ok '\t'
  | '\\' -> Ok '\\'
  | '\'' -> Ok '\''
  | '"' -> Ok '"'
  | '$' -> Ok '$'
  | ' ' -> Ok ' '
  | '#' -> Ok '#'
  | '=' -> Ok '='
  | char -> parse_error line ("invalid escape sequence: \\" ^ char_to_string char)

let append_substitution = fun state buffer line ->
  ignore (advance state);
  match current state with
  | Some '{' ->
      ignore (advance state);
      let start = state.index in
      let parse_name_start () =
        match current state with
        | None -> parse_error line "unterminated variable substitution"
        | Some '\n' -> parse_error line "unterminated variable substitution"
        | Some char when is_name_start char ->
            ignore (advance state);
            Ok ()
        | Some char -> parse_error line ("invalid variable name near " ^ char_to_string char)
      in
      let rec loop () =
        match current state with
        | None -> parse_error line "unterminated variable substitution"
        | Some '\n' -> parse_error line "unterminated variable substitution"
        | Some '}' ->
            let name = String.sub state.content ~offset:start ~len:(state.index - start) in
            ignore (advance state);
            IO.Buffer.add_string buffer (lookup_value state name);
            Ok ()
        | Some char when is_name_char char ->
            ignore (advance state);
            loop ()
        | Some char -> parse_error state.line ("invalid variable name near " ^ char_to_string char)
      in
      (
        match parse_name_start () with
        | Error error -> Error error
        | Ok () -> loop ()
      )
  | Some char when is_unbraced_substitution_char char ->
      let start = state.index in
      let rec loop () =
        match current state with
        | Some char when is_unbraced_substitution_char char ->
            ignore (advance state);
            loop ()
        | _ ->
            let name = String.sub state.content ~offset:start ~len:(state.index - start) in
            IO.Buffer.add_string buffer (lookup_value state name);
            Ok ()
      in
      loop ()
  | _ ->
      IO.Buffer.add_char buffer '$';
      Ok ()

let append_escape = fun state buffer line ->
  ignore (advance state);
  match current state with
  | None -> parse_error line "unterminated escape sequence"
  | Some '\n' -> parse_error line "unterminated escape sequence"
  | Some char -> (
      ignore (advance state);
      match escaped_char line char with
      | Error error -> Error error
      | Ok escaped ->
          IO.Buffer.add_char buffer escaped;
          Ok ()
    )

let parse_single_quoted = fun state buffer line ->
  ignore (advance state);
  let rec loop () =
    match current state with
    | None -> parse_error line "unterminated single-quoted value"
    | Some '\'' ->
        ignore (advance state);
        Ok ()
    | Some char ->
        IO.Buffer.add_char buffer (advance state);
        if Char.equal char '\n' then
          loop ()
        else
          loop ()
  in
  loop ()

let parse_double_quoted = fun state buffer line ->
  ignore (advance state);
  let rec loop () =
    match current state with
    | None -> parse_error line "unterminated double-quoted value"
    | Some '"' ->
        ignore (advance state);
        Ok ()
    | Some '\\' -> (
        match append_escape state buffer line with
        | Error error -> Error error
        | Ok () -> loop ()
      )
    | Some '$' -> (
        match append_substitution state buffer line with
        | Error error -> Error error
        | Ok () -> loop ()
      )
    | Some _ ->
        IO.Buffer.add_char buffer (advance state);
        loop ()
  in
  loop ()

let parse_value = fun state line ->
  let buffer = IO.Buffer.create ~size:32 in
  let pending_spaces = ref 0 in
  let quoted_tail = ref false in
  let flush_spaces () =
    let rec loop count =
      if count <= 0 then
        ()
      else (
        IO.Buffer.add_char buffer ' ';
        loop (count - 1)
      )
    in
    loop !pending_spaces;
    pending_spaces := 0
  in
  let discard_spaces () =
    pending_spaces := 0
  in
  let ensure_after_quote () =
    if !quoted_tail then
      match current state with
      | Some '\''
      | Some '"'
      | Some '\\'
      | Some '$'
      | Some '#'
      | Some '\n'
      | None -> Ok ()
      | Some char when is_horizontal_space char -> Ok ()
      | Some _ -> parse_error line "unexpected text after quoted value"
    else
      Ok ()
  in
  let rec loop () =
    match current state with
    | None ->
        discard_spaces ();
        Ok (IO.Buffer.contents buffer)
    | Some '\n' ->
        discard_spaces ();
        ignore (advance state);
        Ok (IO.Buffer.contents buffer)
    | Some '#' ->
        if !pending_spaces > 0 then (
          discard_spaces ();
          skip_to_line_end state;
          Ok (IO.Buffer.contents buffer)
        ) else (
          flush_spaces ();
          IO.Buffer.add_char buffer (advance state);
          quoted_tail := false;
          loop ()
        )
    | Some char when is_horizontal_space char ->
        ignore (advance state);
        pending_spaces := !pending_spaces + 1;
        loop ()
    | Some '\'' -> parse_quoted parse_single_quoted
    | Some '"' -> parse_quoted parse_double_quoted
    | Some '\\' -> parse_unquoted append_escape
    | Some '$' -> parse_unquoted append_substitution
    | Some _ -> append_literal ()
  and parse_quoted parse_fn =
    ensure_after_quote ()
    |> Result.and_then
      ~fn:(fun () ->
        flush_spaces ();
        parse_fn state buffer line)
    |> Result.and_then
      ~fn:(fun () ->
        quoted_tail := true;
        loop ())
  and parse_unquoted append_fn =
    ensure_after_quote ()
    |> Result.and_then
      ~fn:(fun () ->
        flush_spaces ();
        quoted_tail := false;
        append_fn state buffer line)
    |> Result.and_then ~fn:loop
  and append_literal () =
    ensure_after_quote ()
    |> Result.and_then
      ~fn:(fun () ->
        flush_spaces ();
        quoted_tail := false;
        IO.Buffer.add_char buffer (advance state);
        loop ())
  in
  loop ()

let separator = fun state line ->
  skip_horizontal state;
  match current state with
  | Some '=' ->
      ignore (advance state);
      Ok true
  | Some ':' ->
      ignore (advance state);
      Ok true
  | Some '\n'
  | Some '#'
  | None -> Ok false
  | Some _ -> parse_error line "expected KEY=VALUE"

let parse_binding_after_key = fun state ~line ~exported key ->
  match separator state line with
  | Error error -> Error error
  | Ok false ->
      if exported then
        if has_value state key then (
          skip_to_line_end state;
          Ok None
        ) else
          parse_error line ("exported variable is not set: " ^ key)
      else
        parse_error line "expected KEY=VALUE"
  | Ok true ->
      skip_horizontal state;
      match current state with
      | Some '#' ->
          skip_to_line_end state;
          Ok (Some { key; value = ""; line })
      | Some '\n' ->
          ignore (advance state);
          Ok (Some { key; value = ""; line })
      | None -> Ok (Some { key; value = ""; line })
      | _ -> (
          match parse_value state line with
          | Error error -> Error error
          | Ok value -> Ok (Some { key; value; line })
        )

let parse_binding = fun state ->
  let line = state.line in
  match parse_key state line with
  | Error error -> Error error
  | Ok key ->
      skip_horizontal state;
      if String.equal key "export" then
        match current state with
        | Some '='
        | Some ':' -> parse_binding_after_key state ~line ~exported:false key
        | Some '\n'
        | Some '#'
        | None -> parse_error line "expected variable after export"
        | _ -> (
            match parse_key state line with
            | Error error -> Error error
            | Ok exported_key -> parse_binding_after_key state ~line ~exported:true exported_key
          )
      else
        parse_binding_after_key state ~line ~exported:false key

let parse = fun content ->
  let state = make_state content in
  let rec loop acc =
    skip_ignored_lines state;
    match current state with
    | None -> Ok (List.rev acc)
    | Some _ -> (
        match parse_binding state with
        | Error error -> Error error
        | Ok None -> loop acc
        | Ok (Some binding) ->
            state.values <- (binding.key, Some binding.value) :: state.values;
            loop (binding :: acc)
      )
  in
  let result = loop [] in
  emit_parse_result result;
  result
