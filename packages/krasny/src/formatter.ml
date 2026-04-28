open Std
open Std.Collections

module Ast = Syn.Ast
module Kind = Syn.SyntaxKind
module Layout = Layout_policy
module Slice = IO.IoVec.IoSlice
module Text = Format_text

type error = { message: string }

type write_error =
  | Cannot_format of error
  | Cannot_write of IO.error

exception Unsupported of error

let error_to_string = fun err -> err.message

let unsupported = fun message -> raise (Unsupported { message })

type sink = {
  writer: IO.Writer.t;
  flush_threshold: int;
  mutable error: IO.error option;
}

type leading_comment_kind =
  | Leading_ordinary_comment
  | Leading_docstring
  | Leading_section_docstring

type leading_delimited_trivia =
  | Leading_comment_trivia of Ast.Token.delimited_trivia
  | Leading_docstring_trivia of Ast.Token.delimited_trivia

type state = {
  buffer: IO.Buffer.t;
  sink: sink;
  width: int;
  mutable line_start: bool;
  mutable column: int;
  mutable indent: int;
  mutable pending_spaces: int;
  mutable wrote: bool;
  mutable line_count: int;
  mutable suppress_leading_token: int option;
  mutable last_leading_comment_kind: leading_comment_kind option;
  mutable suppress_list_item_leading_comments: bool;
  mutable delimited_expr_depth: int;
}

let layout_context = fun ?(role = Layout.Top_expr) ?column state ->
  let column =
    match column with
    | Some column -> column
    | None -> state.column
  in
  Layout.make_context ~role ~width:state.width ~column ~indent:state.indent ()

let layout_decision_is_inline = function
  | { Layout.mode = Inline; _ } -> true
  | _ -> false

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Formatter.append_subslice: " ^ IO.IoVec.error_message error)

let flush = fun state ->
  match state.sink.error with
  | Some error -> Error error
  | None ->
      if IO.Buffer.length state.buffer = 0 then
        Ok ()
      else
        match IO.write_all state.sink.writer ~from:state.buffer with
        | Ok () ->
            IO.Buffer.clear state.buffer;
            Ok ()
        | Error error ->
            state.sink.error <- Some error;
            Error error

let flush_if_needed = fun state ->
  if IO.Buffer.length state.buffer >= state.sink.flush_threshold then
    ignore (flush state)

let write_indent = fun state ->
  let rec loop remaining =
    if Int.(remaining > 0) then (
      IO.Buffer.add_char state.buffer ' ';
      loop (Int.sub remaining 1)
    )
  in
  loop state.indent

let write_pending_spaces = fun state ->
  let count = state.pending_spaces in
  if Int.(count > 0) then (
    for _ = 1 to count do
      IO.Buffer.add_char state.buffer ' '
    done;
    state.pending_spaces <- 0;
    flush_if_needed state;
    state.wrote <- true
  )

let write_string_segment = fun state value ~off ~len ->
  if Int.(len > 0) then (
    if state.line_start then (
      state.pending_spaces <- 0;
      write_indent state
    ) else
      write_pending_spaces state;
    IO.Buffer.add_substring state.buffer value off len;
    flush_if_needed state;
    state.wrote <- true;
    state.line_start <- false;
    state.last_leading_comment_kind <- None
  )

let write_slice_segment = fun state value ~off ~len ->
  if Int.(len > 0) then (
    if state.line_start then (
      state.pending_spaces <- 0;
      write_indent state
    ) else
      write_pending_spaces state;
    append_subslice_unchecked state.buffer value ~off ~len;
    flush_if_needed state;
    state.wrote <- true;
    state.line_start <- false;
    state.last_leading_comment_kind <- None
  )

let emit_line = fun state ->
  state.pending_spaces <- 0;
  IO.Buffer.add_char state.buffer '\n';
  flush_if_needed state;
  state.wrote <- true;
  state.line_count <- Int.add state.line_count 1;
  state.line_start <- true;
  state.column <- state.indent

let emit_text = fun state value ->
  let length = String.length value in
  if Int.(length > 0) then
    let rec loop segment_start index saw_newline =
      if Int.(index >= length) then (
        write_string_segment state value ~off:segment_start ~len:Int.(length - segment_start);
        state.column <- if saw_newline then
          Int.sub length segment_start
        else
          state.column + length
      ) else if Char.equal (String.get_unchecked value ~at:index) '\n' then
        (
          let segment_end =
            Text.trim_whitespace_only_segment value ~start:segment_start ~stop:index
          in
          write_string_segment state value ~off:segment_start ~len:Int.(segment_end - segment_start);
          state.pending_spaces <- 0;
          IO.Buffer.add_char state.buffer '\n';
          flush_if_needed state;
          state.wrote <- true;
          state.line_count <- Int.add state.line_count 1;
          state.line_start <- true;
          loop Int.(index + 1) Int.(index + 1) true
        )
      else
        loop segment_start Int.(index + 1) saw_newline
    in
    loop 0 0 false

let delimited_trivia_closing = fun (trivia: Ast.Token.delimited_trivia) ->
  match trivia.closing with
  | Some closing -> closing
  | None -> ""

let normalize_inline_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  let body = String.trim docstring.content in
  let closing = delimited_trivia_closing docstring in
  if String.is_empty body then
    docstring.opening ^ " " ^ closing
  else
    docstring.opening ^ " " ^ body ^ " " ^ closing

let normalize_inline_first_multiline_docstring = fun
  (docstring: Ast.Token.delimited_trivia)
  lines
  ~first_index
  ~last_index ->
  let closing = delimited_trivia_closing docstring in
  let buffer = IO.Buffer.create ~size:(Int.add (String.length docstring.text) 8) in
  let first_line =
    Vector.get_unchecked lines ~at:first_index
    |> String.trim
  in
  IO.Buffer.add_string buffer docstring.opening;
  if not (String.is_empty first_line) then (
    IO.Buffer.add_char buffer ' ';
    IO.Buffer.add_string buffer first_line
  );
  if Int.equal first_index last_index then (
    IO.Buffer.add_char buffer ' ';
    IO.Buffer.add_string buffer closing
  ) else (
    IO.Buffer.add_char buffer '\n';
    for index = Int.add first_index 1 to last_index do
      let line =
        Vector.get_unchecked lines ~at:index
        |> Text.trim_trailing_horizontal
      in
      IO.Buffer.add_string buffer line;
      if Int.equal index last_index then (
        IO.Buffer.add_char buffer ' ';
        IO.Buffer.add_string buffer closing
      ) else
        IO.Buffer.add_char buffer '\n'
    done
  );
  IO.Buffer.contents buffer

let normalize_multiline_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  let lines = Text.split_lines docstring.content in
  let line_count = Vector.length lines in
  if Int.(line_count <= 1) then
    normalize_inline_docstring docstring
  else
    match (Text.first_nonblank_line_index lines, Text.last_nonblank_line_index lines) with
    | (None, _)
    | (_, None) -> docstring.opening ^ "\n" ^ delimited_trivia_closing docstring
    | (Some first_index, Some last_index) ->
        let first_is_inline = Int.equal first_index 0 in
        let common_indent =
          if first_is_inline && Int.(first_index < last_index) then
            Text.common_docstring_indent
              lines
              ~start:(Int.add first_index 1)
              ~stop:last_index
          else
            Text.common_docstring_indent lines ~start:first_index ~stop:last_index
        in
        let buffer = IO.Buffer.create ~size:(Int.add (String.length docstring.text) 8) in
        let add_body_line index =
          let raw_line = Vector.get_unchecked lines ~at:index in
          let line =
            if first_is_inline && Int.equal index first_index then
              String.trim raw_line
            else
              Text.trim_trailing_horizontal (Text.strip_leading_width raw_line common_indent)
          in
          if String.is_empty line then
            IO.Buffer.add_char buffer '\n'
          else (
            IO.Buffer.add_string buffer "   ";
            IO.Buffer.add_string buffer line;
            IO.Buffer.add_char buffer '\n'
          )
        in
        IO.Buffer.add_string buffer docstring.opening;
        IO.Buffer.add_char buffer '\n';
        for index = first_index to last_index do
          add_body_line index
        done;
        IO.Buffer.add_string buffer (delimited_trivia_closing docstring);
        IO.Buffer.contents buffer

let normalize_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  if String.contains docstring.content "\n" then
    normalize_multiline_docstring docstring
  else
    normalize_inline_docstring docstring

let docstring_is_section = fun (docstring: Ast.Token.delimited_trivia) ->
  let content = String.trim docstring.content in
  if String.is_empty content then
    false
  else
    let first = String.get_unchecked content ~at:0 in
    if Char.equal first '#' then
      true
    else if not (Char.equal first '{') then
      false
    else if Int.(String.length content <= 1) then
      false
    else
      match String.get_unchecked content ~at:1 with
      | '0' .. '9' -> true
      | _ -> false

let delimited_trivia_comment_kind = fun (comment: Ast.Token.delimited_trivia) ->
  if String.equal comment.opening "(**" then
    Leading_docstring
  else
    Leading_ordinary_comment

let comment_is_star_banner = fun (comment: Ast.Token.delimited_trivia) ->
  String.equal comment.opening "(*" && String.equal (delimited_trivia_closing comment) "*)" && (
    let body = String.trim comment.content in
    Int.(String.length body >= 8) && String.for_all body ~fn:(fun char -> Char.equal char '*')
  )

let normalize_inline_comment = fun (comment: Ast.Token.delimited_trivia) ->
  if comment_is_star_banner comment then
    comment.text
  else
    (
      let body = String.trim comment.content in
      let closing = delimited_trivia_closing comment in
      if String.is_empty body then
        comment.opening ^ " " ^ closing
      else
        comment.opening ^ " " ^ body ^ " " ^ closing
    )

let normalize_multiline_comment = fun (comment: Ast.Token.delimited_trivia) ->
  let lines = Text.split_lines comment.content in
  let line_count = Vector.length lines in
  match (Text.first_nonblank_line_index lines, Text.last_nonblank_line_index lines) with
  | (None, _)
  | (_, None) -> comment.opening ^ "\n" ^ delimited_trivia_closing comment
  | (Some first_index, Some last_index) ->
      let first_is_inline = Int.equal first_index 0 in
      let body_start =
        if first_is_inline then
          Int.add first_index 1
        else
          first_index
      in
      let has_trailing_blank_content = Int.(last_index + 1 < line_count) in
      let closing_prefix =
        if has_trailing_blank_content then
          Vector.get_unchecked lines ~at:(Int.sub line_count 1)
        else
          ""
      in
      let base_indent = Text.leading_horizontal_width closing_prefix in
      let body_indent =
        if Int.(body_start <= last_index) then
          Text.common_docstring_indent lines ~start:body_start ~stop:last_index
        else
          base_indent
      in
      let body_prefix_width = Int.min body_indent 3 in
      let buffer = IO.Buffer.create ~size:(Int.add (String.length comment.text) 8) in
      let add_body_prefix () =
        for _ = 1 to body_prefix_width do
          IO.Buffer.add_char buffer ' '
        done
      in
      IO.Buffer.add_string buffer comment.opening;
      if first_is_inline then
        (
          let first_line =
            Vector.get_unchecked lines ~at:first_index
            |> String.trim
          in
          if not (String.is_empty first_line) then (
            IO.Buffer.add_char buffer ' ';
            IO.Buffer.add_string buffer first_line
          )
        );
      IO.Buffer.add_char buffer '\n';
      for index = body_start to last_index do
        let raw_line = Vector.get_unchecked lines ~at:index in
        let line = Text.trim_trailing_horizontal (Text.strip_leading_width raw_line body_indent) in
        if String.is_empty line then
          IO.Buffer.add_char buffer '\n'
        else (
          add_body_prefix ();
          IO.Buffer.add_string buffer line;
          IO.Buffer.add_char buffer '\n'
        )
      done;
      if has_trailing_blank_content then
        (
          let rec add_blank_lines index =
            if Int.(index < Int.sub line_count 1) then (
              IO.Buffer.add_char buffer '\n';
              add_blank_lines (Int.add index 1)
            )
          in
          add_blank_lines (Int.add last_index 1);
          IO.Buffer.add_string
            buffer
            (Text.trim_trailing_horizontal (Text.strip_leading_width closing_prefix base_indent))
        );
      IO.Buffer.add_string buffer (delimited_trivia_closing comment);
      IO.Buffer.contents buffer

let normalize_comment = fun (comment: Ast.Token.delimited_trivia) ->
  if String.contains comment.content "\n" then
    normalize_multiline_comment comment
  else
    normalize_inline_comment comment

let token_leading_trivia_starts_inline = fun token ->
  let text = Ast.Token.leading_text token in
  let length = String.length text in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      match String.get_unchecked text ~at:index with
      | ' '
      | '\t'
      | '\r' -> loop (Int.add index 1)
      | '\n' -> false
      | '(' -> true
      | _ -> false
  in
  loop 0

let token_single_leading_delimited_trivia = fun token ->
  let result = ref None in
  let multiple = ref false in
  Ast.Token.for_each_leading_trivia_item
    token
    ~fn:(
      function
      | Ast.Token.Whitespace -> ()
      | Ast.Token.Comment comment -> (
          match !result with
          | Some _ -> multiple := true
          | None -> result := Some (Leading_comment_trivia comment)
        )
      | Ast.Token.Docstring docstring -> (
          match !result with
          | Some _ -> multiple := true
          | None -> result := Some (Leading_docstring_trivia docstring)
        )
    );
  if !multiple then
    None
  else
    !result

let token_single_inline_leading_delimited_trivia = fun token ->
  if token_leading_trivia_starts_inline token then
    token_single_leading_delimited_trivia token
  else
    None

let record_field_trailing_trivia_text = function
  | Leading_comment_trivia comment ->
      let text = normalize_comment comment in
      if String.contains text "\n" then
        None
      else
        Some text
  | Leading_docstring_trivia docstring ->
      ignore docstring;
      None

let token_inline_leading_comment_text = fun token ->
  if token_leading_trivia_starts_inline token then
    match token_single_leading_delimited_trivia token with
    | Some (Leading_comment_trivia comment) ->
        record_field_trailing_trivia_text (Leading_comment_trivia comment)
    | Some (Leading_docstring_trivia _)
    | None -> None
  else
    None

let emit_slice = fun state ~has_newline value ->
  let length = Slice.length value in
  if Int.(length > 0) then
    if has_newline then (
      if state.line_start then (
        state.pending_spaces <- 0;
        write_indent state
      ) else
        write_pending_spaces state;
      append_subslice_unchecked state.buffer value ~off:0 ~len:length;
      flush_if_needed state;
      state.wrote <- true;
      state.line_count <- Int.add state.line_count (Text.count_slice_newlines value);
      state.line_start <- Char.equal (Slice.get_unchecked value ~at:Int.(length - 1)) '\n';
      state.column <- Text.last_slice_line_width value
    ) else (
      write_slice_segment state value ~off:0 ~len:length;
      state.column <- state.column + length
    )

let emit_space = fun state ->
  if state.line_start then
    state.column <- state.column + 1
  else (
    state.pending_spaces <- Int.add state.pending_spaces 1;
    state.column <- state.column + 1
  )

let drop_pending_spaces = fun state ->
  let count = state.pending_spaces in
  if Int.(count > 0) then (
    state.pending_spaces <- 0;
    state.column <- Int.sub state.column count
  )

let emit_spaces = fun state count ->
  let rec loop remaining =
    if Int.(remaining > 0) then (
      emit_space state;
      loop (Int.sub remaining 1)
    )
  in
  loop count

let current_line_fits_text = fun state text ->
  Int.(state.column + String.length text <= state.width)

let emit_inline_leading_trivia_from_token = fun state token ~leading_spaces ~trailing_space ->
  match token_single_inline_leading_delimited_trivia token with
  | None -> false
  | Some trivia -> (
      match record_field_trailing_trivia_text trivia with
      | None -> false
      | Some text ->
          emit_spaces state leading_spaces;
          emit_text state text;
          state.suppress_leading_token <- Some token.Ast.id;
          if trailing_space then
            emit_space state;
          true
    )

let emit_record_field_trailing_trivia_from_token = fun state token ->
  emit_inline_leading_trivia_from_token
    state
    token
    ~leading_spaces:2
    ~trailing_space:false

let with_indent = fun state extra fn ->
  let previous = state.indent in
  state.indent <- Int.add previous extra;
  if state.line_start then
    state.column <- state.indent;
  fn ();
  state.indent <- previous;
  if state.line_start then
    state.column <- state.indent

let with_delimited_expr = fun state fn ->
  let previous = state.delimited_expr_depth in
  state.delimited_expr_depth <- Int.succ previous;
  fn ();
  state.delimited_expr_depth <- previous

let with_suppressed_list_item_leading_comments = fun state fn ->
  let previous = state.suppress_list_item_leading_comments in
  state.suppress_list_item_leading_comments <- true;
  fn ();
  state.suppress_list_item_leading_comments <- previous

let emit_token_leading_comments_as_lines = fun state token ->
  match state.suppress_leading_token with
  | Some id when Int.equal id token.Ast.id -> ()
  | _ ->
      let emitted = ref false in
      let previous_kind = ref None in
      Ast.Token.for_each_leading_trivia_item
        token
        ~fn:(
          function
          | Ast.Token.Comment comment ->
              let kind = delimited_trivia_comment_kind comment in
              if !emitted then
                emit_line state;
              (
                match (!previous_kind, kind) with
                | (Some Leading_docstring, Leading_docstring) -> emit_line state
                | (Some Leading_section_docstring, _) -> emit_line state
                | (Some Leading_ordinary_comment, _)
                | (Some Leading_docstring, (Leading_ordinary_comment | Leading_section_docstring))
                | (None, _) -> ()
              );
              emit_text state (normalize_comment comment);
              state.last_leading_comment_kind <- Some kind;
              previous_kind := Some kind;
              emitted := true
          | Ast.Token.Docstring docstring ->
              let kind =
                if docstring_is_section docstring then
                  Leading_section_docstring
                else
                  Leading_docstring
              in
              if !emitted then
                emit_line state;
              (
                match (!previous_kind, kind) with
                | (Some Leading_docstring, Leading_docstring) -> emit_line state
                | (Some Leading_section_docstring, _) -> emit_line state
                | (Some Leading_ordinary_comment, _)
                | (Some Leading_docstring, (Leading_ordinary_comment | Leading_section_docstring))
                | (None, _) -> ()
              );
              emit_text state (normalize_docstring docstring);
              state.last_leading_comment_kind <- Some kind;
              previous_kind := Some kind;
              emitted := true
          | Ast.Token.Whitespace -> ()
        );
      if !emitted then (
        emit_line state;
        state.suppress_leading_token <- Some token.Ast.id
      )

let token_has_leading_ordinary_comment_needing_separator = fun token ->
  let found = ref false in
  Ast.Token.for_each_leading_trivia_item
    token
    ~fn:(
      function
      | Ast.Token.Comment comment ->
          let text = normalize_comment comment in
          if not (String.ends_with ~suffix:"\n" text) then
            found := true
      | Ast.Token.Docstring _
      | Ast.Token.Whitespace -> ()
    );
  !found

let emit_top_level_leading_comments_as_lines = fun
  ?(drop_initial_docstring = false)
  ?(compact_final_section_docstring = false)
  state
  token ->
  let previous_kind = ref None in
  let seen_non_whitespace = ref false in
  Ast.Token.for_each_leading_trivia_item
    token
    ~fn:(
      function
      | Ast.Token.Comment comment ->
          let kind = delimited_trivia_comment_kind comment in
          seen_non_whitespace := true;
          (
            match (!previous_kind, kind) with
            | (Some Leading_docstring, Leading_docstring) -> emit_line state
            | (Some Leading_section_docstring, _) -> emit_line state
            | (Some Leading_ordinary_comment, _) -> emit_line state
            | (Some Leading_docstring, (Leading_ordinary_comment | Leading_section_docstring))
            | (None, _) -> ()
          );
          emit_text state (normalize_comment comment);
          state.last_leading_comment_kind <- Some kind;
          emit_line state;
          previous_kind := Some kind
      | Ast.Token.Docstring docstring ->
          let is_section = docstring_is_section docstring in
          if drop_initial_docstring && not !seen_non_whitespace && not is_section then
            seen_non_whitespace := true
          else (
            (
              match !previous_kind with
              | None -> (
                  match state.last_leading_comment_kind with
                  | Some Leading_docstring when not is_section -> emit_line state
                  | Some Leading_section_docstring -> emit_line state
                  | Some Leading_ordinary_comment
                  | Some Leading_docstring
                  | None -> ()
                )
              | Some Leading_docstring when not is_section -> emit_line state
              | Some Leading_section_docstring when is_section -> emit_line state
              | Some Leading_section_docstring ->
                  if not compact_final_section_docstring then
                    emit_line state
              | Some Leading_ordinary_comment
              | Some Leading_docstring -> ()
            );
            seen_non_whitespace := true;
            emit_text state (normalize_docstring docstring);
            state.last_leading_comment_kind <- Some (
              if is_section then
                Leading_section_docstring
              else
                Leading_docstring
            );
            emit_line state;
            previous_kind := Some (
              if is_section then
                Leading_section_docstring
              else
                Leading_docstring
            )
          )
      | Ast.Token.Whitespace -> ()
    );
  match !previous_kind with
  | Some Leading_section_docstring ->
      if not compact_final_section_docstring then
        emit_line state;
      state.suppress_leading_token <- Some token.Ast.id
  | Some (Leading_ordinary_comment | Leading_docstring) ->
      state.suppress_leading_token <- Some token.Ast.id
  | None ->
      if !seen_non_whitespace then
        state.suppress_leading_token <- Some token.Ast.id

let emit_leading_trivia_before_token = fun state token ->
  match state.suppress_leading_token with
  | Some id when Int.equal id token.Ast.id -> state.suppress_leading_token <- None
  | _ ->
      if Ast.Token.has_leading_comment token then (
        emit_token_leading_comments_as_lines state token;
        state.suppress_leading_token <- None
      )

let emit_node_leading_trivia = fun state node ->
  match Ast.Node.first_descendant_token node with
  | Some token -> (
      match state.suppress_leading_token with
      | Some id when Int.equal id token.Ast.id -> ()
      | _ ->
          if Ast.Token.has_leading_comment token then
            emit_token_leading_comments_as_lines state token
    )
  | None -> ()

let emit_token = fun state token ->
  emit_leading_trivia_before_token state token;
  emit_slice state ~has_newline:(Ast.Token.has_newline token) (Ast.Token.slice token)

let emit_token_text = fun state token text ->
  emit_leading_trivia_before_token state token;
  emit_text state text

let emit_literal_token = fun state token ->
  let kind = Ast.Token.kind token in
  if Kind.is kind Kind.INT then
    emit_token_text state token (Text.format_int_literal (Ast.Token.text token))
  else if Kind.is kind Kind.FLOAT then
    emit_token_text state token (Text.format_float_literal (Ast.Token.text token))
  else
    emit_token state token

let emit_optional_token = fun state token ->
  match token with
  | Some token -> emit_token state token
  | None -> ()

let emit_keyword = emit_text

let emit_node_keyword = fun state node ~kind ~fallback ->
  match Ast.Node.first_child_token node ~kind with
  | Some token -> emit_token state token
  | None -> emit_keyword state fallback

let emit_top_level_leading = fun
  ?(drop_initial_docstring = false)
  ?(compact_final_section_docstring = false)
  state
  node ->
  match Ast.Node.first_descendant_token node with
  | Some token -> (
      match state.suppress_leading_token with
      | Some id when Int.equal id token.Ast.id -> ()
      | Some _
      | None ->
          if Ast.Token.has_leading_comment token then
            (
              let needs_separator = token_has_leading_ordinary_comment_needing_separator token in
              emit_top_level_leading_comments_as_lines
                ~drop_initial_docstring
                ~compact_final_section_docstring
                state
                token;
              if needs_separator then
                emit_line state
            )
    )
  | None -> ()

let emit_node_leading_comments_as_lines = fun state node ->
  match Ast.Node.first_descendant_token node with
  | Some token when Ast.Token.has_leading_comment token ->
      emit_token_leading_comments_as_lines state token
  | Some _
  | None -> ()

let with_node_leading_comments_suppressed = fun state node fn ->
  let previous = state.suppress_leading_token in
  (
    match Ast.Node.first_descendant_token node with
    | Some token when Ast.Token.has_leading_comment token ->
        state.suppress_leading_token <- Some token.Ast.id
    | Some _
    | None -> ()
  );
  fn ();
  state.suppress_leading_token <- previous

let emit_token_or_keyword = fun state token ~fallback ->
  match token with
  | Some token ->
      if Ast.Token.has_leading_comment token then
        emit_token_leading_comments_as_lines state token;
      emit_token state token
  | None -> emit_text state fallback

let token_has_leading_comment = function
  | Some token -> Ast.Token.has_leading_comment token
  | None -> false

let token_has_leading_docstring = function
  | Some token -> Ast.Token.has_leading_docstring token
  | None -> false

let node_has_leading_comment = fun node ->
  match Ast.Node.first_descendant_token node with
  | Some token -> Ast.Token.has_leading_comment token
  | None -> false

let token_text = Ast.Token.text

let token_text_is = Ast.Token.text_is

let token_text_starts_uppercase = fun token ->
  let text = token_text token in
  if String.is_empty text then
    false
  else
    match String.get_unchecked text ~at:0 with
    | 'A' .. 'Z' -> true
    | _ -> false

let node_kind = Ast.Node.kind

let token_kind_is = fun token kind -> Kind.is (Ast.Token.kind token) kind

let node_kind_is = fun node kind -> Kind.is (Ast.Node.kind node) kind

let token_flat_width = fun token -> Slice.length (Ast.Token.slice token)

let same_ast_node = fun (left: Ast.Node.t) (right: Ast.Node.t) -> Int.equal left.Ast.id right.Ast.id

let is_module_expr_kind = function
  | Kind.MODULE_EXPR
  | Kind.PATH_MODULE_EXPR
  | Kind.STRUCT_MODULE_EXPR
  | Kind.FUNCTOR_MODULE_EXPR
  | Kind.APPLY_MODULE_EXPR
  | Kind.CONSTRAINT_MODULE_EXPR
  | Kind.PAREN_MODULE_EXPR
  | Kind.OPAQUE_MODULE_EXPR -> true
  | _ -> false

let is_module_type_kind = function
  | Kind.MODULE_TYPE_EXPR
  | Kind.PATH_MODULE_TYPE
  | Kind.SIGNATURE_MODULE_TYPE
  | Kind.TYPEOF_MODULE_TYPE
  | Kind.FUNCTOR_MODULE_TYPE
  | Kind.WITH_MODULE_TYPE -> true
  | _ -> false

let first_child_node_matching = fun node ~matches ->
  let found = ref None in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None ->
          if matches (node_kind child) then
            found := Some child);
  !found

let nth_child_node_matching = fun node index ~matches ->
  let found = ref None in
  let current = ref 0 in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None ->
          if matches (node_kind child) then (
            if Int.equal !current index then
              found := Some child;
            current := Int.add !current 1
          ));
  !found

let first_child_token_matching = fun node ~matches ->
  let found = ref None in
  Ast.Node.for_each_child_token
    node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if matches (Ast.Token.kind token) then
            found := Some token);
  !found

let first_descendant_node_matching = fun (node: Ast.Node.t) ~matches ->
  let found = ref None in
  let rec visit node =
    Ast.Node.for_each_child_node
      node
      ~fn:(fun child ->
        match !found with
        | Some _ -> ()
        | None ->
            if matches (node_kind child) then
              found := Some child
            else
              visit child)
  in
  visit node;
  !found

let node_child_token_kind_is = fun (node: Ast.Node.t) index kind ->
  match Ast.Node.child_at node index with
  | Some (Syn.SyntaxTree.Token id) -> token_kind_is ({ tree = node.Ast.tree; id }: Ast.Token.t) kind
  | Some (Syn.SyntaxTree.Node _)
  | Some (Syn.SyntaxTree.Missing _)
  | None -> false

let node_child_index = fun (node: Ast.Node.t) (target: Ast.Node.t) ->
  let rec loop index =
    if Int.(index >= Ast.Node.child_count node) then
      None
    else
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Node id) when Int.equal id target.Ast.id -> Some index
      | Some _
      | None -> loop (Int.add index 1)
  in
  loop 0

let collect_node_attribute_suffix_tokens_after_child = fun (node: Ast.Node.t) (child: Ast.Node.t) ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match node_child_index node child with
  | None -> tokens
  | Some child_index ->
      let first_suffix_index = Int.add child_index 1 in
      if
        node_child_token_kind_is node first_suffix_index Kind.LBRACKET
        && (node_child_token_kind_is
          node
          (Int.add first_suffix_index 1)
          Kind.AT
        || node_child_token_kind_is
          node
          (Int.add first_suffix_index 1)
          Kind.ATAT)
      then
        let rec loop index =
          if Int.(index < Ast.Node.child_count node) then (
            (
              match Ast.Node.child_at node index with
              | Some (Syn.SyntaxTree.Token id) ->
                  Vector.push tokens ~value:({ tree = node.Ast.tree; id }: Ast.Token.t)
              | Some (Syn.SyntaxTree.Node id) ->
                  Ast.Node.for_each_token
                    ({ tree = node.Ast.tree; id }: Ast.Node.t)
                    ~fn:(fun token -> Vector.push tokens ~value:token)
              | Some (Syn.SyntaxTree.Missing _)
              | None -> ()
            );
            loop (Int.add index 1)
          )
        in
        loop first_suffix_index;
        tokens
      else
        tokens

let collect_wrapped_module_body_attribute_suffix_tokens = fun (node: Ast.Node.t) ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match first_descendant_node_matching
    node
    ~matches:(fun kind -> Kind.(kind = MODULE_EXPR || kind = MODULE_TYPE_EXPR)) with
  | None -> tokens
  | Some wrapper -> (
      match first_child_node_matching
        wrapper
        ~matches:(fun kind ->
          (is_module_expr_kind kind || is_module_type_kind kind)
          && not Kind.(kind = MODULE_EXPR || kind = MODULE_TYPE_EXPR)) with
      | None -> tokens
      | Some body -> collect_node_attribute_suffix_tokens_after_child wrapper body
    )

let node_has_token_kind = fun node kind ->
  let found = ref false in
  Ast.Node.for_each_token
    node
    ~fn:(fun token ->
      if not !found && token_kind_is token kind then
        found := true);
  !found

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token
    node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if token_kind_is token Kind.IDENT then
            found := Some token);
  !found

let last_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token
    node
    ~fn:(fun token ->
      if token_kind_is token Kind.IDENT then
        found := Some token);
  !found

let token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  Kind.(previous_kind = LBRACKET && current_kind = PIPE)
  || not
    (
      Kind.(current_kind = RPAREN)
      || Kind.(current_kind = RBRACKET)
      || Kind.(current_kind = RBRACE)
      || Kind.(current_kind = COLON)
      || Kind.(current_kind = COMMA)
      || Kind.(current_kind = SEMI)
      || Kind.(current_kind = DOT)
      || Kind.(previous_kind = LPAREN)
      || Kind.(previous_kind = LBRACKET)
      || Kind.(previous_kind = LBRACE)
      || Kind.(previous_kind = DOT)
      || Kind.(previous_kind = BACKTICK)
      || Kind.(previous_kind = QUOTE)
      || Kind.(previous_kind = AT)
      || Kind.(previous_kind = ATAT)
      || Kind.(previous_kind = TILDE)
      || Kind.(previous_kind = QUESTION)
    )

let attribute_token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  if Kind.(previous_kind = ATAT) && not Kind.(current_kind = AT) then
    true
  else
    token_wants_space_before previous token

let emit_token_stream = fun state node ->
  let previous = ref None in
  Ast.Node.for_each_token
    node
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let emit_attribute_token_stream = fun state node ->
  let previous = ref None in
  Ast.Node.for_each_token
    node
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when attribute_token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let emit_token_vector_stream = fun state tokens ->
  let previous = ref None in
  Vector.for_each
    tokens
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let first_class_module_type_token_wants_space_before = fun previous token ->
  let previous_kind = Ast.Token.kind previous in
  if Kind.(previous_kind = PERCENT) then
    false
  else
    token_wants_space_before previous token

let emit_first_class_module_type_token_vector_stream = fun state tokens ->
  let previous = ref None in
  Vector.for_each
    tokens
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when first_class_module_type_token_wants_space_before previous token ->
            emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let emit_attribute_token_vector_stream = fun state tokens ->
  let previous = ref None in
  Vector.for_each
    tokens
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when attribute_token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let core_type_token_wants_space_before = fun previous token ->
  let previous_kind = Ast.Token.kind previous in
  if Kind.(previous_kind = COLON) then
    false
  else
    token_wants_space_before previous token

let emit_core_type_token_vector_stream = fun state tokens ->
  let previous = ref None in
  Vector.for_each
    tokens
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when core_type_token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let token_wants_space_before_type_constraint = fun previous token ->
  let previous_kind = Ast.Token.kind previous in
  let current_kind = Ast.Token.kind token in
  if Kind.(previous_kind = EQ || current_kind = EQ) then
    false
  else
    token_wants_space_before previous token

let emit_type_constraint_token_vector_stream = fun state tokens ->
  let previous = ref None in
  let length = Vector.length tokens in
  let rec loop index =
    if Int.(index < length) then
      (
        let token = Vector.get_unchecked tokens ~at:index in
        (
          match !previous with
          | Some previous when token_wants_space_before_type_constraint previous token ->
              emit_space state
          | Some _
          | None -> ()
        );
        emit_token state token;
        previous := Some token;
        loop (Int.add index 1)
      )
  in
  loop 0

let render_attribute_suffix_tokens = fun state tokens ->
  if Vector.length tokens > 0 then (
    emit_space state;
    emit_attribute_token_vector_stream state tokens
  )

let render_node_attribute_suffix_after_child = fun state node child ->
  render_attribute_suffix_tokens
    state
    (collect_node_attribute_suffix_tokens_after_child node child)

let render_wrapped_module_body_attribute_suffix = fun state node ->
  render_attribute_suffix_tokens
    state
    (collect_wrapped_module_body_attribute_suffix_tokens node)

let emit_token_vector_range_stream = fun state tokens ~start ~stop ->
  let previous = ref None in
  let rec loop index =
    if Int.(index < stop) then
      (
        let token = Vector.get_unchecked tokens ~at:index in
        (
          match !previous with
          | Some previous when token_wants_space_before previous token -> emit_space state
          | Some _
          | None -> ()
        );
        emit_token state token;
        previous := Some token;
        loop (Int.add index 1)
      )
  in
  loop start

let emit_attribute_token_vector_range_stream = fun state tokens ~start ~stop ->
  let previous = ref None in
  let rec loop index =
    if Int.(index < stop) then
      (
        let token = Vector.get_unchecked tokens ~at:index in
        (
          match !previous with
          | Some previous when attribute_token_wants_space_before previous token -> emit_space state
          | Some _
          | None -> ()
        );
        emit_token state token;
        previous := Some token;
        loop (Int.add index 1)
      )
  in
  loop start

let emit_token_vector_range_compact = fun state tokens ~start ~stop ->
  let rec loop index =
    if Int.(index < stop) then (
      emit_token state (Vector.get_unchecked tokens ~at:index);
      loop (Int.add index 1)
    )
  in
  loop start

let emit_token_vector_compact = fun state tokens ->
  emit_token_vector_range_compact
    state
    tokens
    ~start:0
    ~stop:(Vector.length tokens)

let shell_token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  not
    (
      Kind.(current_kind = RBRACKET)
      || Kind.(current_kind = BAR_RBRACKET)
      || Kind.(current_kind = DOT)
      || Kind.(current_kind = COLON)
      || Kind.(previous_kind = LBRACKET)
      || Kind.(previous_kind = LBRACKET_BAR)
      || Kind.(previous_kind = DOT)
      || Kind.(previous_kind = AT)
      || Kind.(previous_kind = ATAT)
      || Kind.(previous_kind = PERCENT)
      || Kind.(previous_kind = PERCENTGT)
      || Kind.(previous_kind = LTPERCENT)
    )

let emit_shell_token_stream = fun state for_each_shell_token ->
  let previous = ref None in
  for_each_shell_token
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when shell_token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token);
  match !previous with
  | Some _ -> ()
  | None -> unsupported "attribute or extension without shell tokens"

let emit_shell_token_vector_range_stream = fun state tokens ~start ~stop ->
  emit_shell_token_stream
    state
    (fun ~fn ->
      let rec loop index =
        if Int.(index < stop) then (
          fn (Vector.get_unchecked tokens ~at:index);
          loop (Int.add index 1)
        )
      in
      loop start)

let emit_shell_node_token_stream = fun state node ->
  emit_shell_token_stream
    state
    (fun ~fn -> Ast.Node.for_each_token node ~fn)

let unsupported_node = fun label node ->
  unsupported
    (label ^ ": " ^ Kind.to_string (node_kind node))

let collect_child_exprs = fun (node: Ast.Node.t) ->
  let exprs = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match Ast.cast_result_to_option (Ast.Expr.cast child) with
      | Some expr -> Vector.push exprs ~value:expr
      | None -> ());
  exprs

let collect_child_patterns = fun (node: Ast.Node.t) ->
  let patterns = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match Ast.cast_result_to_option (Ast.Pattern.cast child) with
      | Some pattern -> Vector.push patterns ~value:pattern
      | None -> ());
  patterns

let collect_fun_parameters = fun (node: Ast.Node.t) ->
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  let rec push_parameter parameter =
    match Ast.Parameter.view parameter with
    | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; _ } when node_kind_is parameter Kind.CONSTRUCT_PATTERN ->
        push_positional_construct parameter
    | Ast.Parameter.Param _
    | Ast.Parameter.Unknown _ -> Vector.push parameters ~value:parameter
  and push_positional_construct pattern =
    match node_kind pattern with
    | Kind.CONSTRUCT_PATTERN ->
        Ast.Node.for_each_child_node
          pattern
          ~fn:(fun child ->
            match Ast.cast_result_to_option (Ast.Parameter.cast child) with
            | Some child -> push_parameter child
            | None -> ())
    | _ -> Vector.push parameters ~value:pattern
  in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match Ast.cast_result_to_option (Ast.Parameter.cast child) with
      | Some parameter -> push_parameter parameter
      | None -> ());
  parameters

let rec collect_child_type_exprs = fun (type_expr: Ast.TypeExpr.t) ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.Node.for_each_child_node
    type_expr
    ~fn:(fun child ->
      match Ast.cast_result_to_option (Ast.TypeExpr.cast child) with
      | Some item -> Vector.push items ~value:item
      | None -> ());
  if Int.equal (Vector.length items) 1 && node_kind_is type_expr Kind.TYPE_EXPR then
    let only_child = Vector.get_unchecked items ~at:0 in
    if node_kind_is only_child Kind.TUPLE_TYPE then
      collect_child_type_exprs only_child
    else
      items
  else
    items

let collect_record_fields = fun record ->
  let fields: Ast.record_expr_field_view Vector.t =
    Vector.with_capacity ~size:(Ast.Node.child_count record)
  in
  Ast.RecordExpr.for_each_field record ~fn:(fun field -> Vector.push fields ~value:field);
  fields

let collect_array_items = collect_child_exprs

let collect_type_members = fun decl ->
  let members = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeDeclaration.for_each_member decl ~fn:(fun member -> Vector.push members ~value:member);
  members

let collect_type_member_parameters = fun member ->
  let parameters = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  Ast.TypeDeclaration.Member.for_each_parameter
    member
    ~fn:(fun parameter -> Vector.push parameters ~value:parameter);
  parameters

let collect_type_member_attribute_items = fun member ->
  let attributes = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  Ast.TypeDeclaration.Member.for_each_child_node
    member
    ~fn:(fun child ->
      if node_kind_is child Kind.ATTRIBUTE_ITEM then
        Vector.push attributes ~value:child);
  attributes

let type_member_attribute_suffix_start_at = fun member close_index ->
  if not (Ast.TypeDeclaration.Member.child_token_kind_is member close_index Kind.RBRACKET) then
    None
  else
    let rec loop index depth =
      if Int.(index < 0) then
        None
      else if Ast.TypeDeclaration.Member.child_token_kind_is member index Kind.RBRACKET then
        loop (Int.sub index 1) (Int.add depth 1)
      else if Ast.TypeDeclaration.Member.child_token_kind_is member index Kind.LBRACKET then
        if Int.equal depth 1 then
          let next = Int.add index 1 in
          if
            Ast.TypeDeclaration.Member.child_token_kind_is member next Kind.AT
            || Ast.TypeDeclaration.Member.child_token_kind_is member next Kind.ATAT
          then
            Some index
          else
            None
        else
          loop (Int.sub index 1) (Int.sub depth 1)
      else
        loop (Int.sub index 1) depth
    in
    loop (Int.sub close_index 1) 1

let type_member_last_non_attribute_suffix_child_index = fun member ->
  let rec loop index =
    if Int.(index < 0) then
      (-1)
    else
      match type_member_attribute_suffix_start_at member index with
      | Some start -> loop (Int.sub start 1)
      | None -> index
  in
  loop (Int.sub (Ast.TypeDeclaration.Member.child_count member) 1)

let type_member_first_attribute_suffix_child_index = fun member ->
  let last_body_index = type_member_last_non_attribute_suffix_child_index member in
  let first_suffix_index = Int.add last_body_index 1 in
  if
    Int.(first_suffix_index < Ast.TypeDeclaration.Member.child_count member)
    && Ast.TypeDeclaration.Member.child_token_kind_is member first_suffix_index Kind.LBRACKET
  then
    Some first_suffix_index
  else
    None

let collect_type_member_attribute_suffix_tokens = fun member ->
  let tokens = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  match type_member_first_attribute_suffix_child_index member with
  | None -> tokens
  | Some first_suffix_index ->
      let rec loop index =
        if Int.(index < Ast.TypeDeclaration.Member.child_count member) then (
          (
            match Ast.TypeDeclaration.Member.child_at member index with
            | Some (Syn.SyntaxTree.Token _) -> (
                match Ast.TypeDeclaration.Member.child_token_at member index with
                | Some token -> Vector.push tokens ~value:token
                | None -> ()
              )
            | Some (Syn.SyntaxTree.Node _) -> (
                match Ast.TypeDeclaration.Member.child_node_at member index with
                | Some node ->
                    Ast.Node.for_each_token node ~fn:(fun token -> Vector.push tokens ~value:token)
                | None -> ()
              )
            | Some (Syn.SyntaxTree.Missing _)
            | None -> ()
          );
          loop (Int.add index 1)
        )
      in
      loop first_suffix_index;
      tokens

let collect_type_declaration_attribute_items = fun decl ->
  let attributes = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.Node.for_each_child_node
    decl
    ~fn:(fun child ->
      if node_kind_is child Kind.ATTRIBUTE_ITEM then
        Vector.push attributes ~value:child);
  attributes

let collect_type_extension_parameters = fun decl ->
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeExtensionDeclaration.for_each_parameter
    decl
    ~fn:(fun parameter -> Vector.push parameters ~value:parameter);
  parameters

let collect_record_type_fields = fun record_type ->
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record_type) in
  Ast.RecordType.for_each_field record_type ~fn:(fun field -> Vector.push fields ~value:field);
  fields

let collect_let_bindings = fun decl ->
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.LetDeclaration.for_each_binding decl ~fn:(fun binding -> Vector.push bindings ~value:binding);
  bindings

let collect_let_bindings_from_expr = fun expr ->
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Node.for_each_child_node
    expr
    ~fn:(fun child ->
      match Ast.cast_result_to_option (Ast.LetBinding.cast child) with
      | Some binding -> Vector.push bindings ~value:binding
      | None -> ());
  bindings

let collect_binding_operator_clauses = fun expr ->
  let clauses = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.BindingOperatorExpr.for_each_clause expr ~fn:(fun clause -> Vector.push clauses ~value:clause);
  clauses

let collect_external_name_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_name_token
    decl
    ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let collect_value_name_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ValueDeclaration.for_each_name_token decl ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let collect_external_primitive_strings = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_primitive_string
    decl
    ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let collect_external_attribute_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_attribute_token
    decl
    ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let collect_child_tokens = fun node ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_token node ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let is_expr_node_kind = function
  | Kind.LET_EXPR
  | Kind.LOCAL_OPEN_EXPR
  | Kind.LET_MODULE_EXPR
  | Kind.LET_EXCEPTION_EXPR
  | Kind.BINDING_OPERATOR_EXPR
  | Kind.FIRST_CLASS_MODULE_EXPR
  | Kind.EXTENSION_EXPR
  | Kind.UNREACHABLE_EXPR
  | Kind.IF_EXPR
  | Kind.MATCH_EXPR
  | Kind.FUN_EXPR
  | Kind.FUNCTION_EXPR
  | Kind.TRY_EXPR
  | Kind.WHILE_EXPR
  | Kind.FOR_EXPR
  | Kind.ASSERT_EXPR
  | Kind.LAZY_EXPR
  | Kind.ATTRIBUTE_EXPR
  | Kind.SEQUENCE_EXPR
  | Kind.APPLY_EXPR
  | Kind.INFIX_EXPR
  | Kind.PREFIX_EXPR
  | Kind.ASSIGN_EXPR
  | Kind.FIELD_ACCESS_EXPR
  | Kind.POLY_VARIANT_EXPR
  | Kind.LABELED_ARG
  | Kind.OPTIONAL_ARG
  | Kind.ARRAY_INDEX_EXPR
  | Kind.STRING_INDEX_EXPR
  | Kind.TYPED_EXPR
  | Kind.PATH_EXPR
  | Kind.LITERAL_EXPR
  | Kind.PAREN_EXPR
  | Kind.TUPLE_EXPR
  | Kind.LIST_EXPR
  | Kind.ARRAY_EXPR
  | Kind.RECORD_EXPR
  | Kind.RECORD_UPDATE_EXPR -> true
  | _ -> false

let is_pattern_node_kind = function
  | Kind.WILDCARD_PATTERN
  | Kind.PATH_PATTERN
  | Kind.CONSTRUCT_PATTERN
  | Kind.LITERAL_PATTERN
  | Kind.PAREN_PATTERN
  | Kind.TUPLE_PATTERN
  | Kind.LIST_PATTERN
  | Kind.ARRAY_PATTERN
  | Kind.RECORD_PATTERN
  | Kind.POLY_VARIANT_PATTERN
  | Kind.EXTENSION_PATTERN
  | Kind.ATTRIBUTE_PATTERN
  | Kind.LOCAL_OPEN_PATTERN
  | Kind.LOCALLY_ABSTRACT_TYPE_PATTERN
  | Kind.FIRST_CLASS_MODULE_PATTERN
  | Kind.INTERVAL_PATTERN
  | Kind.CONSTRAINT_PATTERN
  | Kind.ALIAS_PATTERN
  | Kind.OR_PATTERN
  | Kind.CONS_PATTERN
  | Kind.LAZY_PATTERN
  | Kind.EXCEPTION_PATTERN
  | Kind.LABELED_PARAM
  | Kind.OPTIONAL_PARAM
  | Kind.OPTIONAL_PARAM_DEFAULT -> true
  | _ -> false

let is_type_expr_node_kind = function
  | Kind.TYPE_EXPR
  | Kind.PATH_TYPE
  | Kind.VAR_TYPE
  | Kind.WILDCARD_TYPE
  | Kind.ARROW_TYPE
  | Kind.POLY_TYPE
  | Kind.LABELED_TYPE
  | Kind.TUPLE_TYPE
  | Kind.APPLY_TYPE
  | Kind.PAREN_TYPE
  | Kind.OPAQUE_TYPE -> true
  | _ -> false

let first_child_expr = fun node -> first_child_node_matching node ~matches:is_expr_node_kind

let nth_child_expr = fun node index -> nth_child_node_matching node index ~matches:is_expr_node_kind

let first_child_pattern = fun node -> first_child_node_matching node ~matches:is_pattern_node_kind

let nth_child_pattern = fun node index ->
  nth_child_node_matching
    node
    index
    ~matches:is_pattern_node_kind

let first_child_type_expr = fun node ->
  first_child_node_matching
    node
    ~matches:is_type_expr_node_kind

let nth_child_type_expr = fun node index ->
  nth_child_node_matching
    node
    index
    ~matches:is_type_expr_node_kind

let first_ident_token = fun node ->
  first_child_token_matching
    node
    ~matches:(fun kind -> Kind.(kind = IDENT))

let first_match_case_child = fun node ->
  first_child_node_matching
    node
    ~matches:(fun kind -> Kind.(kind = MATCH_CASE))

let first_let_binding_child = fun node ->
  first_child_node_matching
    node
    ~matches:(fun kind -> Kind.(kind = LET_BINDING))

module TypeExprView = struct
  type tuple_separator =
    | Star
    | Comma
    | UnknownSeparator

  type view =
    | Path of {
        path: Ast.path;
      }
    | Var of {
        name: Ast.Token.t option;
      }
    | Wildcard
    | Arrow of {
        left: Ast.TypeExpr.t option;
        right: Ast.TypeExpr.t option;
      }
    | Poly of {
        body: Ast.TypeExpr.t option;
      }
    | Tuple of { separator: tuple_separator }
    | Apply of {
        argument: Ast.TypeExpr.t option;
        constructor: Ast.TypeExpr.t option;
      }
    | Parenthesized of {
        inner: Ast.TypeExpr.t option;
      }
    | Labeled of {
        optional_token: Ast.Token.t option;
        label: Ast.Token.t option;
        annotation: Ast.TypeExpr.t option;
      }
    | Opaque of Ast.Node.t
    | Error of Ast.Node.t
    | Unknown of Ast.Node.t

  let tuple_separator = fun type_expr ->
    let found = ref UnknownSeparator in
    Ast.Node.for_each_child_token
      type_expr
      ~fn:(fun token ->
        if token_kind_is token Kind.STAR then
          found := Star
        else if token_kind_is token Kind.COMMA then
          found := Comma);
    !found

  let rec view = fun type_expr ->
    match node_kind type_expr with
    | Kind.TYPE_EXPR -> (
        match (first_child_type_expr type_expr, nth_child_type_expr type_expr 1) with
        | (Some child, None) when not (same_ast_node child type_expr) -> view child
        | _ -> Unknown type_expr
      )
    | Kind.PATH_TYPE -> Path { path = type_expr }
    | Kind.VAR_TYPE -> Var { name = first_ident_token type_expr }
    | Kind.WILDCARD_TYPE -> Wildcard
    | Kind.ARROW_TYPE ->
        Arrow {
          left = nth_child_type_expr type_expr 0;
          right = nth_child_type_expr type_expr 1;
        }
    | Kind.POLY_TYPE -> Poly { body = first_child_type_expr type_expr }
    | Kind.LABELED_TYPE ->
        Labeled {
          optional_token = first_child_token_matching
            type_expr
            ~matches:(fun kind -> Kind.(kind = QUESTION));
          label = first_ident_token type_expr;
          annotation = first_child_type_expr type_expr;
        }
    | Kind.TUPLE_TYPE -> Tuple { separator = tuple_separator type_expr }
    | Kind.APPLY_TYPE ->
        Apply {
          argument = nth_child_type_expr type_expr 0;
          constructor = nth_child_type_expr type_expr 1;
        }
    | Kind.PAREN_TYPE -> Parenthesized { inner = first_child_type_expr type_expr }
    | Kind.OPAQUE_TYPE -> Opaque type_expr
    | Kind.ERROR -> Error type_expr
    | _ -> Unknown type_expr
end

module PatternView = struct
  type view =
    | Wildcard
    | Path of {
        path: Ast.path;
      }
    | Construct of {
        callee: Ast.Pattern.t option;
        argument: Ast.Pattern.t option;
      }
    | Literal of {
        token: Ast.Token.t option;
      }
    | Parenthesized of {
        inner: Ast.Pattern.t option;
      }
    | Tuple
    | List
    | Array
    | Record
    | PolyVariant
    | Interval of {
        left: Ast.Pattern.t option;
        right: Ast.Pattern.t option;
      }
    | Constraint of {
        pattern: Ast.Pattern.t option;
        annotation: Ast.TypeExpr.t option;
      }
    | Alias of {
        pattern: Ast.Pattern.t option;
        alias: Ast.Pattern.t option;
      }
    | Or of {
        left: Ast.Pattern.t option;
        right: Ast.Pattern.t option;
      }
    | Cons of {
        head: Ast.Pattern.t option;
        tail: Ast.Pattern.t option;
      }
    | Lazy of {
        pattern: Ast.Pattern.t option;
      }
    | Exception of {
        pattern: Ast.Pattern.t option;
      }
    | LabeledParam of Ast.Parameter.t
    | OptionalParam of Ast.Parameter.t
    | OptionalParamDefault of Ast.Parameter.t
    | LocallyAbstractType
    | FirstClassModule
    | LocalOpen
    | Extension
    | Attribute of {
        inner: Ast.Pattern.t option;
      }
    | Error of Ast.Node.t
    | Unknown of Ast.Node.t

  let view = fun pattern ->
    match node_kind pattern with
    | Kind.PAREN_PATTERN -> Parenthesized { inner = first_child_pattern pattern }
    | Kind.ATTRIBUTE_PATTERN -> Attribute { inner = first_child_pattern pattern }
    | Kind.WILDCARD_PATTERN -> Wildcard
    | Kind.PATH_PATTERN -> Path { path = pattern }
    | Kind.CONSTRUCT_PATTERN ->
        Construct {
          callee = nth_child_pattern pattern 0;
          argument = nth_child_pattern pattern 1;
        }
    | Kind.LITERAL_PATTERN -> Literal { token = Ast.Pattern.literal_token pattern }
    | Kind.TUPLE_PATTERN -> Tuple
    | Kind.LIST_PATTERN -> List
    | Kind.ARRAY_PATTERN -> Array
    | Kind.RECORD_PATTERN -> Record
    | Kind.POLY_VARIANT_PATTERN -> PolyVariant
    | Kind.INTERVAL_PATTERN ->
        Interval { left = nth_child_pattern pattern 0; right = nth_child_pattern pattern 1 }
    | Kind.CONSTRAINT_PATTERN ->
        Constraint {
          pattern = first_child_pattern pattern;
          annotation = first_child_type_expr pattern;
        }
    | Kind.ALIAS_PATTERN ->
        Alias { pattern = nth_child_pattern pattern 0; alias = nth_child_pattern pattern 1 }
    | Kind.OR_PATTERN ->
        Or { left = nth_child_pattern pattern 0; right = nth_child_pattern pattern 1 }
    | Kind.CONS_PATTERN ->
        Cons { head = nth_child_pattern pattern 0; tail = nth_child_pattern pattern 1 }
    | Kind.LAZY_PATTERN -> Lazy { pattern = first_child_pattern pattern }
    | Kind.EXCEPTION_PATTERN -> Exception { pattern = first_child_pattern pattern }
    | Kind.LABELED_PARAM -> (
        match Ast.cast_result_to_option (Ast.Parameter.cast pattern) with
        | Some parameter -> LabeledParam parameter
        | None -> Unknown pattern
      )
    | Kind.OPTIONAL_PARAM -> (
        match Ast.cast_result_to_option (Ast.Parameter.cast pattern) with
        | Some parameter -> OptionalParam parameter
        | None -> Unknown pattern
      )
    | Kind.OPTIONAL_PARAM_DEFAULT -> (
        match Ast.cast_result_to_option (Ast.Parameter.cast pattern) with
        | Some parameter -> OptionalParamDefault parameter
        | None -> Unknown pattern
      )
    | Kind.LOCALLY_ABSTRACT_TYPE_PATTERN -> LocallyAbstractType
    | Kind.FIRST_CLASS_MODULE_PATTERN -> FirstClassModule
    | Kind.LOCAL_OPEN_PATTERN -> LocalOpen
    | Kind.EXTENSION_PATTERN -> Extension
    | Kind.ERROR -> Error pattern
    | _ -> Unknown pattern
end

module ExprView = struct
  type view =
    | Path of {
        path: Ast.path;
      }
    | Literal of {
        token: Ast.Token.t option;
      }
    | FieldAccess of {
        target: Ast.Expr.t option;
        field: Ast.Token.t option;
      }
    | Assign of {
        target: Ast.Expr.t option;
        operator: Ast.Token.t option;
        value: Ast.Expr.t option;
      }
    | Infix of {
        left: Ast.Expr.t option;
        operator: Ast.Token.t option;
        right: Ast.Expr.t option;
      }
    | Prefix of {
        operator: Ast.Token.t option;
        operand: Ast.Expr.t option;
      }
    | Apply of {
        callee: Ast.Expr.t option;
        argument: Ast.Expr.t option;
      }
    | LabeledArg of {
        label: Ast.Token.t option;
        value: Ast.Expr.t option;
      }
    | OptionalArg of {
        label: Ast.Token.t option;
        value: Ast.Expr.t option;
      }
    | Parenthesized of {
        inner: Ast.Expr.t option;
      }
    | If of {
        condition: Ast.Expr.t option;
        then_branch: Ast.Expr.t option;
        else_branch: Ast.Expr.t option;
      }
    | Let of {
        first_binding: Ast.LetBinding.t option;
        body: Ast.Expr.t option;
      }
    | Fun of {
        body: Ast.Expr.t option;
      }
    | Function of {
        first_case: Ast.MatchCase.t option;
      }
    | Match of {
        scrutinee: Ast.Expr.t option;
        first_case: Ast.MatchCase.t option;
      }
    | Try of {
        body: Ast.Expr.t option;
        first_case: Ast.MatchCase.t option;
      }
    | LocalOpen of {
        body: Ast.Expr.t option;
      }
    | List
    | Array
    | Record
    | RecordUpdate
    | Typed of {
        expr: Ast.Expr.t option;
        annotation: Ast.TypeExpr.t option;
      }
    | Tuple
    | Sequence of {
        left: Ast.Expr.t option;
        right: Ast.Expr.t option;
      }
    | ArrayIndex of {
        target: Ast.Expr.t option;
        index: Ast.Expr.t option;
      }
    | StringIndex of {
        target: Ast.Expr.t option;
        index: Ast.Expr.t option;
      }
    | PolyVariant of {
        payload: Ast.Expr.t option;
      }
    | BindingOperator
    | LetModule of {
        body: Ast.Expr.t option;
      }
    | LetException of {
        body: Ast.Expr.t option;
      }
    | FirstClassModule
    | While of {
        condition: Ast.Expr.t option;
        body: Ast.Expr.t option;
      }
    | Assert of {
        argument: Ast.Expr.t option;
      }
    | Lazy of {
        argument: Ast.Expr.t option;
      }
    | Attribute of {
        inner: Ast.Expr.t option;
      }
    | Extension
    | For of {
        pattern: Ast.Pattern.t option;
        start_: Ast.Expr.t option;
        stop: Ast.Expr.t option;
        body: Ast.Expr.t option;
      }
    | Unreachable
    | Error of Ast.Node.t
    | Unknown of Ast.Node.t

  let first_operator_token = fun node ->
    first_child_token_matching
      node
      ~matches:(fun kind ->
        not
          (Kind.(kind = IDENT)
          || Kind.(kind = INT)
          || Kind.(kind = FLOAT)
          || Kind.(kind = STRING)
          || Kind.(kind = CHAR)
          || Kind.(kind = TRUE_KW)
          || Kind.(kind = FALSE_KW)))

  let first_direct_token = fun node -> first_child_token_matching node ~matches:(fun _ -> true)

  let view = fun expr ->
    match node_kind expr with
    | Kind.PAREN_EXPR -> Parenthesized { inner = first_child_expr expr }
    | Kind.PATH_EXPR -> Path { path = expr }
    | Kind.LITERAL_EXPR -> Literal { token = Ast.Expr.literal_token expr }
    | Kind.FIELD_ACCESS_EXPR ->
        FieldAccess { target = nth_child_expr expr 0; field = first_ident_token expr }
    | Kind.ASSIGN_EXPR ->
        Assign {
          target = nth_child_expr expr 0;
          operator = first_direct_token expr;
          value = nth_child_expr expr 1;
        }
    | Kind.INFIX_EXPR ->
        Infix {
          left = nth_child_expr expr 0;
          operator = first_direct_token expr;
          right = nth_child_expr expr 1;
        }
    | Kind.PREFIX_EXPR ->
        Prefix { operator = first_operator_token expr; operand = first_child_expr expr }
    | Kind.APPLY_EXPR ->
        Apply { callee = nth_child_expr expr 0; argument = nth_child_expr expr 1 }
    | Kind.LABELED_ARG ->
        LabeledArg { label = first_ident_token expr; value = first_child_expr expr }
    | Kind.OPTIONAL_ARG ->
        OptionalArg { label = first_ident_token expr; value = first_child_expr expr }
    | Kind.IF_EXPR ->
        If {
          condition = nth_child_expr expr 0;
          then_branch = nth_child_expr expr 1;
          else_branch = nth_child_expr expr 2;
        }
    | Kind.LET_EXPR ->
        Let { first_binding = first_let_binding_child expr; body = nth_child_expr expr 0 }
    | Kind.FUN_EXPR -> Fun { body = first_child_expr expr }
    | Kind.FUNCTION_EXPR -> Function { first_case = first_match_case_child expr }
    | Kind.MATCH_EXPR ->
        Match { scrutinee = nth_child_expr expr 0; first_case = first_match_case_child expr }
    | Kind.TRY_EXPR ->
        Try { body = nth_child_expr expr 0; first_case = first_match_case_child expr }
    | Kind.LOCAL_OPEN_EXPR -> LocalOpen { body = nth_child_expr expr 1 }
    | Kind.LIST_EXPR -> List
    | Kind.ARRAY_EXPR -> Array
    | Kind.RECORD_EXPR -> Record
    | Kind.RECORD_UPDATE_EXPR -> RecordUpdate
    | Kind.TYPED_EXPR ->
        Typed { expr = first_child_expr expr; annotation = first_child_type_expr expr }
    | Kind.TUPLE_EXPR -> Tuple
    | Kind.SEQUENCE_EXPR ->
        Sequence { left = nth_child_expr expr 0; right = nth_child_expr expr 1 }
    | Kind.ARRAY_INDEX_EXPR ->
        ArrayIndex { target = nth_child_expr expr 0; index = nth_child_expr expr 1 }
    | Kind.STRING_INDEX_EXPR ->
        StringIndex { target = nth_child_expr expr 0; index = nth_child_expr expr 1 }
    | Kind.POLY_VARIANT_EXPR -> PolyVariant { payload = first_child_expr expr }
    | Kind.BINDING_OPERATOR_EXPR -> BindingOperator
    | Kind.LET_MODULE_EXPR -> LetModule { body = first_child_expr expr }
    | Kind.LET_EXCEPTION_EXPR -> LetException { body = first_child_expr expr }
    | Kind.FIRST_CLASS_MODULE_EXPR -> FirstClassModule
    | Kind.WHILE_EXPR ->
        While { condition = nth_child_expr expr 0; body = nth_child_expr expr 1 }
    | Kind.ASSERT_EXPR -> Assert { argument = first_child_expr expr }
    | Kind.LAZY_EXPR -> Lazy { argument = first_child_expr expr }
    | Kind.ATTRIBUTE_EXPR -> Attribute { inner = first_child_expr expr }
    | Kind.EXTENSION_EXPR -> Extension
    | Kind.FOR_EXPR ->
        For {
          pattern = first_child_pattern expr;
          start_ = nth_child_expr expr 0;
          stop = nth_child_expr expr 1;
          body = nth_child_expr expr 2;
        }
    | Kind.UNREACHABLE_EXPR -> Unreachable
    | Kind.ERROR -> Error expr
    | _ -> Unknown expr
end

let collect_node_tokens = fun node ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_token node ~fn:(fun token -> Vector.push tokens ~value:token);
  tokens

let collect_node_tokens_after_direct_token = fun node kind ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  let after = ref false in
  Ast.Node.for_each_child
    node
    ~fn:(
      function
      | Syn.SyntaxTree.Token id ->
          let token: Ast.Token.t = { tree = node.Ast.tree; id } in
          if !after then
            Vector.push tokens ~value:token
          else if token_kind_is token kind then
            after := true
      | Syn.SyntaxTree.Node id ->
          if !after then
            Ast.Node.for_each_token
              ({ tree = node.Ast.tree; id }: Ast.Node.t)
              ~fn:(fun token -> Vector.push tokens ~value:token)
      | Syn.SyntaxTree.Missing _ -> ()
    );
  tokens

let collect_prefix_operator_tokens = fun node ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  let rec loop index =
    if Int.(index < Ast.Node.child_count node) then
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Token id) ->
          Vector.push tokens ~value:({ tree = node.Ast.tree; id }: Ast.Token.t);
          loop (Int.add index 1)
      | Some (Syn.SyntaxTree.Node _)
      | Some (Syn.SyntaxTree.Missing _)
      | None -> ()
  in
  loop 0;
  tokens

let collect_direct_tokens_between_children = fun node left right ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match (node_child_index node left, node_child_index node right) with
  | (Some left_index, Some right_index) when Int.(left_index < right_index) ->
      let rec loop index =
        if Int.(index < right_index) then (
          (
            match Ast.Node.child_at node index with
            | Some (Syn.SyntaxTree.Token id) ->
                Vector.push tokens ~value:({ tree = node.Ast.tree; id }: Ast.Token.t)
            | Some (Syn.SyntaxTree.Node _)
            | Some (Syn.SyntaxTree.Missing _)
            | None -> ()
          );
          loop (Int.add index 1)
        )
      in
      loop (Int.add left_index 1);
      tokens
  | _ -> tokens

let collect_direct_tokens_after_child = fun node child ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match node_child_index node child with
  | None -> tokens
  | Some child_index ->
      let rec loop index =
        if Int.(index < Ast.Node.child_count node) then (
          (
            match Ast.Node.child_at node index with
            | Some (Syn.SyntaxTree.Token id) ->
                Vector.push tokens ~value:({ tree = node.Ast.tree; id }: Ast.Token.t)
            | Some (Syn.SyntaxTree.Node _)
            | Some (Syn.SyntaxTree.Missing _)
            | None -> ()
          );
          loop (Int.add index 1)
        )
      in
      loop (Int.add child_index 1);
      tokens

let collect_infix_operator_tokens = fun expr left operator right ->
  let tokens = collect_direct_tokens_between_children expr left right in
  if Int.equal (Vector.length tokens) 0 then
    Vector.push tokens ~value:operator;
  tokens

let token_vector_text = fun tokens ->
  let buffer = IO.Buffer.create ~size:(Vector.length tokens) in
  Vector.for_each tokens ~fn:(fun token -> IO.Buffer.add_string buffer (token_text token));
  IO.Buffer.contents buffer

let token_vector_tight_flat_width = fun tokens ->
  let length = Vector.length tokens in
  let rec loop index total =
    if Int.(index >= length) then
      total
    else
      loop
        (Int.add index 1)
        (Int.add total (token_flat_width (Vector.get_unchecked tokens ~at:index)))
  in
  loop 0 0

let token_vector_spaced_flat_width = fun tokens ->
  let length = Vector.length tokens in
  let rec loop index previous total =
    if Int.(index >= length) then
      Some total
    else
      let token = Vector.get_unchecked tokens ~at:index in
      let extra_space =
        match previous with
        | Some previous when token_wants_space_before previous token -> 1
        | _ -> 0
      in
      loop
        (Int.add index 1)
        (Some token)
        (Int.add total (Int.add extra_space (token_flat_width token)))
  in
  loop 0 None 0

let attribute_token_vector_spaced_flat_width = fun tokens ->
  let length = Vector.length tokens in
  let rec loop index previous total =
    if Int.(index >= length) then
      Some total
    else
      let token = Vector.get_unchecked tokens ~at:index in
      let extra_space =
        match previous with
        | Some previous when attribute_token_wants_space_before previous token -> 1
        | _ -> 0
      in
      loop
        (Int.add index 1)
        (Some token)
        (Int.add total (Int.add extra_space (token_flat_width token)))
  in
  loop 0 None 0

let token_vector_range_spaced_flat_width = fun tokens ~start ~stop ->
  let rec loop index previous total =
    if Int.(index >= stop) then
      Some total
    else
      let token = Vector.get_unchecked tokens ~at:index in
      let extra_space =
        match previous with
        | Some previous when token_wants_space_before previous token -> 1
        | _ -> 0
      in
      loop
        (Int.add index 1)
        (Some token)
        (Int.add total (Int.add extra_space (token_flat_width token)))
  in
  loop start None 0

let node_spaced_flat_width = fun node ->
  let tokens = collect_child_tokens node in
  token_vector_spaced_flat_width tokens

let infix_operator_tokens = fun expr left operator right ->
  collect_infix_operator_tokens
    expr
    left
    operator
    right

let infix_operator_text_from_expr = fun expr left operator right ->
  infix_operator_tokens expr left operator right
  |> token_vector_text

let infix_operator_flat_width_from_expr = fun expr left operator right ->
  infix_operator_tokens expr left operator right
  |> token_vector_tight_flat_width

let render_infix_operator_tokens = fun state expr left operator right ->
  let tokens = infix_operator_tokens expr left operator right in
  if Int.equal (Vector.length tokens) 1 then
    emit_token state (Vector.get_unchecked tokens ~at:0)
  else
    emit_token_vector_compact state tokens

let collect_child_tokens_of_kind = fun node kind ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_token
    node
    ~fn:(fun token ->
      if token_kind_is token kind then
        Vector.push tokens ~value:token);
  tokens

let token_is_keyword_operator_name = fun token ->
  match token_text token with
  | "asr"
  | "land"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "mod"
  | "or" -> true
  | _ -> false

let token_vector_range_is_keyword_operator_name = fun tokens ~start ~stop ->
  if Int.equal (Int.sub stop start) 1 then
    token_is_keyword_operator_name (Vector.get_unchecked tokens ~at:start)
  else
    false

let token_vector_range_has_ident = fun tokens ~start ~stop ->
  let rec loop index =
    if Int.(index >= stop) then
      false
    else if token_kind_is (Vector.get_unchecked tokens ~at:index) Kind.IDENT then
      true
    else
      loop (Int.add index 1)
  in
  loop start

let token_vector_range_is_operator_name = fun tokens ~start ~stop ->
  if token_vector_range_has_ident tokens ~start ~stop then
    token_vector_range_is_keyword_operator_name tokens ~start ~stop
  else
    Int.(start < stop)

let token_vector_is_operator_name = fun tokens ->
  token_vector_range_is_operator_name
    tokens
    ~start:0
    ~stop:(Vector.length tokens)

let token_vector_find_kind = fun tokens kind ->
  let length = Vector.length tokens in
  let rec loop index =
    if Int.(index >= length) then
      None
    else if token_kind_is (Vector.get_unchecked tokens ~at:index) kind then
      Some index
    else
      loop (Int.add index 1)
  in
  loop 0

let token_vector_find_last_kind = fun tokens kind ->
  let rec loop index =
    if Int.(index < 0) then
      None
    else if token_kind_is (Vector.get_unchecked tokens ~at:index) kind then
      Some index
    else
      loop (Int.sub index 1)
  in
  loop (Int.sub (Vector.length tokens) 1)

let collect_match_cases = fun expr ->
  let cases = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Expr.for_each_match_case expr ~fn:(fun case -> Vector.push cases ~value:case);
  cases

let collect_variant_constructors = fun variant_type ->
  let constructors = Vector.with_capacity ~size:(Ast.Node.child_count variant_type) in
  Ast.VariantType.for_each_constructor
    variant_type
    ~fn:(fun constructor -> Vector.push constructors ~value:constructor);
  constructors

let collect_record_pattern_fields = fun record ->
  let fields: Ast.record_pattern_field_view Vector.t =
    Vector.with_capacity ~size:(Ast.Node.child_count record)
  in
  Ast.RecordPattern.for_each_field record ~fn:(fun field -> Vector.push fields ~value:field);
  fields

let collect_signature_items_from_module_type_decl = fun decl ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleTypeDeclaration.for_each_signature_item
    decl
    ~fn:(fun item -> Vector.push items ~value:item);
  items

let collect_signature_items_from_module_decl = fun decl ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleDeclaration.for_each_signature_item decl ~fn:(fun item -> Vector.push items ~value:item);
  items

let collect_structure_items_from_node = fun node ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      if node_kind_is child Kind.STRUCTURE_ITEM then
        Vector.push items ~value:child);
  items

let collect_structure_items_from_module_decl = fun decl ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleDeclaration.for_each_structure_item decl ~fn:(fun item -> Vector.push items ~value:item);
  items

let collect_structure_item_entries_from_node = fun node ->
  let entries = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  let child_count = Ast.Node.child_count node in
  let rec collect_trailing_tokens index tokens =
    if Int.(index >= child_count) then
      index
    else
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Node id) ->
          let child: Ast.Node.t = { tree = node.Ast.tree; id } in
          if node_kind_is child Kind.STRUCTURE_ITEM then
            index
          else
            collect_trailing_tokens (Int.add index 1) tokens
      | Some (Syn.SyntaxTree.Token id) ->
          Vector.push tokens ~value:({ tree = node.Ast.tree; id }: Ast.Token.t);
          collect_trailing_tokens (Int.add index 1) tokens
      | Some (Syn.SyntaxTree.Missing _)
      | None -> collect_trailing_tokens (Int.add index 1) tokens
  in
  let rec loop index =
    if Int.(index < child_count) then
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Node id) ->
          let child: Ast.Node.t = { tree = node.Ast.tree; id } in
          if node_kind_is child Kind.STRUCTURE_ITEM then
            (
              let trailing_tokens = Vector.with_capacity ~size:2 in
              let next_index = collect_trailing_tokens (Int.add index 1) trailing_tokens in
              Vector.push entries ~value:(child, trailing_tokens);
              loop next_index
            )
          else
            loop (Int.add index 1)
      | Some (Syn.SyntaxTree.Token _)
      | Some (Syn.SyntaxTree.Missing _)
      | None -> loop (Int.add index 1)
  in
  loop 0;
  entries

let collect_signature_items_from_node = fun node ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      if node_kind_is child Kind.SIGNATURE_ITEM then
        Vector.push items ~value:child);
  items

let collect_module_declaration_members = fun decl ->
  let members = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleDeclaration.for_each_member decl ~fn:(fun member -> Vector.push members ~value:member);
  members

let emit_joined_vector = fun state values ~sep ~fn ->
  let length = Vector.length values in
  let rec loop index =
    if Int.(index < length) then (
      if Int.(index > 0) then
        sep ();
      fn (Vector.get_unchecked values ~at:index);
      loop (Int.add index 1)
    )
  in
  loop 0

let render_first_class_module_type = fun state node ->
  let tokens = collect_child_tokens node in
  let length = Vector.length tokens in
  match (
    token_vector_find_kind tokens Kind.LPAREN,
    token_vector_find_kind tokens Kind.MODULE_KW,
    token_vector_find_kind tokens Kind.RPAREN
  ) with
  | (Some opening_index, Some module_index, Some closing_index) when Int.equal
    module_index
    (Int.add opening_index 1)
  && Int.equal opening_index 0
  && Int.equal closing_index (Int.sub length 1) ->
      emit_first_class_module_type_token_vector_stream state tokens;
      true
  | _ -> false

let render_extension_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  let length = Vector.length tokens in
  if Int.(length >= 3) then
    let first = Vector.get_unchecked tokens ~at:0 in
    let second = Vector.get_unchecked tokens ~at:1 in
    let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
    if
      token_kind_is first Kind.LBRACKET
      && token_kind_is second Kind.PERCENT
      && token_kind_is last Kind.RBRACKET
    then (
      Vector.for_each tokens ~fn:(emit_token state);
      true
    ) else
      false
  else
    false

let bracketed_opaque_type_has_inline_open = fun tokens ->
  Int.(Vector.length tokens > 2)
  && (token_kind_is (Vector.get_unchecked tokens ~at:1) Kind.GT
  || token_kind_is (Vector.get_unchecked tokens ~at:1) Kind.LT)

let token_vector_has_leading_comment = fun tokens ->
  let length = Vector.length tokens in
  let rec loop index =
    if Int.(index >= length) then
      false
    else if Ast.Token.has_leading_comment (Vector.get_unchecked tokens ~at:index) then
      true
    else
      loop (Int.add index 1)
  in
  loop 0

let render_bracketed_opaque_type_row = fun state tokens ~start ~stop ->
  if Int.(start < stop) then
    if token_kind_is (Vector.get_unchecked tokens ~at:start) Kind.PIPE then (
      emit_token state (Vector.get_unchecked tokens ~at:start);
      if Int.(Int.add start 1 < stop) then (
        emit_space state;
        emit_token_vector_range_stream
          state
          tokens
          ~start:(Int.add start 1)
          ~stop
      )
    ) else (
      emit_text state "|";
      emit_space state;
      emit_token_vector_range_stream state tokens ~start ~stop
    )

let render_bracketed_opaque_type_rows = fun state tokens ~start ~stop ->
  let rec loop row_start index =
    if Int.(index >= stop) then
      (
        if Int.(row_start < stop) then (
          emit_line state;
          with_indent
            state
            2
            (fun () ->
              render_bracketed_opaque_type_row state tokens ~start:row_start ~stop)
        )
      )
    else if
      Int.(index > row_start) && token_kind_is (Vector.get_unchecked tokens ~at:index) Kind.PIPE
    then (
      emit_line state;
      with_indent
        state
        2
        (fun () ->
          render_bracketed_opaque_type_row state tokens ~start:row_start ~stop:index);
      loop index (Int.add index 1)
    ) else
      loop row_start (Int.add index 1)
  in
  loop start start

let render_bracketed_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  let length = Vector.length tokens in
  if Int.equal length 0 then
    false
  else
    let first = Vector.get_unchecked tokens ~at:0 in
    let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
    if
      (token_kind_is first Kind.LBRACKET && token_kind_is last Kind.RBRACKET)
      || (token_kind_is first Kind.LBRACKET_BAR && token_kind_is last Kind.BAR_RBRACKET)
    then
      (
        let inline_open = bracketed_opaque_type_has_inline_open tokens in
        let content_start =
          if inline_open then
            2
          else
            1
        in
        let should_inline =
          if token_vector_has_leading_comment tokens then
            false
          else
            match token_vector_spaced_flat_width tokens with
            | Some width -> Int.(width <= state.width - state.column)
            | None -> false
        in
        if should_inline then
          emit_token_vector_stream state tokens
        else (
          emit_token state first;
          (
            if inline_open then
              emit_token state (Vector.get_unchecked tokens ~at:1)
          );
          render_bracketed_opaque_type_rows
            state
            tokens
            ~start:content_start
            ~stop:(Int.sub length 1);
          emit_line state;
          emit_token state last
        );
        true
      )
    else
      false

let render_dotdot_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  if Int.equal (Vector.length tokens) 1 then
    if token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.DOTDOT then (
      emit_token state (Vector.get_unchecked tokens ~at:0);
      true
    ) else
      false
  else
    false

let render_gt_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  if Int.equal (Vector.length tokens) 1 then
    if token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.GT then (
      emit_token state (Vector.get_unchecked tokens ~at:0);
      true
    ) else
      false
  else
    false

let render_opaque_type = fun state node ->
  if render_first_class_module_type state node then
    true
  else if render_extension_opaque_type state node then
    true
  else if render_bracketed_opaque_type state node then
    true
  else if render_dotdot_opaque_type state node then
    true
  else
    render_gt_opaque_type state node

let render_path = fun state path -> Ast.Node.for_each_token path ~fn:(emit_token state)

let token_vector_is_parenthesized_operator_name = fun tokens ->
  let length = Vector.length tokens in
  if Int.(length <= 2) then
    false
  else
    let first = Vector.get_unchecked tokens ~at:0 in
    let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
    if token_kind_is first Kind.LPAREN && token_kind_is last Kind.RPAREN then
      token_vector_range_is_operator_name
        tokens
        ~start:1
        ~stop:(Int.sub length 1)
    else
      false

let render_parenthesized_operator_tokens = fun state tokens ->
  let length = Vector.length tokens in
  (
    emit_token state (Vector.get_unchecked tokens ~at:0);
    emit_space state;
    emit_token_vector_range_compact
      state
      tokens
      ~start:1
      ~stop:(Int.sub length 1);
    emit_space state;
    emit_token state (Vector.get_unchecked tokens ~at:(Int.sub length 1))
  )

let render_pattern_path = fun state path ->
  let tokens = collect_child_tokens path in
  if token_vector_is_parenthesized_operator_name tokens then
    render_parenthesized_operator_tokens state tokens
  else
    render_path state path

let render_expr_path = fun state path ->
  let tokens = collect_child_tokens path in
  if token_vector_is_parenthesized_operator_name tokens then
    render_parenthesized_operator_tokens state tokens
  else
    render_path state path

let path_contains_dot = fun path ->
  let found = ref false in
  Ast.Node.for_each_token
    path
    ~fn:(fun token ->
      if token_kind_is token Kind.DOT then
        found := true);
  !found

let expr_is_dotted_path = fun expr ->
  match ExprView.view expr with
  | Path { path } -> path_contains_dot path
  | _ -> false

let render_parenthesized_empty_pattern = fun state pattern ->
  let tokens = collect_child_tokens pattern in
  if token_vector_is_parenthesized_operator_name tokens then
    render_parenthesized_operator_tokens state tokens
  else
    emit_text state "()"

let pattern_is_operator_name = fun pattern ->
  match PatternView.view pattern with
  | Path { path } ->
      collect_child_tokens path
      |> token_vector_is_operator_name
  | _ -> false

let rec pattern_is_unit = fun pattern ->
  match PatternView.view pattern with
  | Parenthesized { inner = None } -> true
  | Constraint { pattern = Some inner; _ }
  | Attribute { inner = Some inner } -> pattern_is_unit inner
  | _ -> false

let collect_or_pattern_items = fun pattern ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  let rec collect pattern =
    match PatternView.view pattern with
    | Or { left = Some left; right = Some right } ->
        collect left;
        collect right
    | _ -> Vector.push items ~value:pattern
  in
  collect pattern;
  items

let rec render_type_expr = fun state type_expr ->
  match Ast.TypeExpr.inner_without_attribute_suffix type_expr with
  | Some inner ->
      render_type_expr state inner;
      render_type_expr_attribute_suffix state type_expr
  | None -> (
      match TypeExprView.view type_expr with
      | Path { path } -> render_path state path
      | Var { name = Some name } ->
          emit_text state "'";
          emit_token state name
      | Var { name = None } -> unsupported_node "type variable without name" type_expr
      | Wildcard -> emit_text state "_"
      | Arrow { left = Some left; right = Some right } ->
          render_type_arrow_left state left;
          emit_space state;
          emit_text state "->";
          emit_space state;
          render_type_expr state right
      | Arrow _ -> unsupported_node "incomplete arrow type" type_expr
      | Apply { argument = Some argument; constructor = Some constructor } ->
          render_type_apply_argument state argument;
          emit_space state;
          render_type_expr state constructor
      | Apply _ -> unsupported_node "incomplete apply type" type_expr
      | Parenthesized { inner = Some inner } ->
          emit_text state "(";
          render_type_expr state inner;
          emit_text state ")"
      | Parenthesized { inner = None } -> emit_text state "()"
      | Tuple { separator; _ } -> render_tuple_type_expr state type_expr separator
      | Poly { body = Some body } -> render_poly_type_expr state type_expr body
      | Poly { body = None } -> unsupported_node "poly type without body" type_expr
      | Labeled { optional_token; label = Some label; annotation = Some annotation } ->
          emit_optional_token state optional_token;
          emit_token state label;
          emit_text state ":";
          render_type_expr state annotation
      | Labeled _ -> unsupported_node "incomplete labeled type" type_expr
      | Opaque node ->
          if render_alias_opaque_type state node then
            ()
          else if not (render_opaque_type state node) then
            unsupported_node "unsupported type expression" type_expr
      | Error _
      | Unknown _ -> unsupported_node "unsupported type expression" type_expr
    )

and render_alias_opaque_type = fun state node ->
  let types = collect_child_type_exprs node in
  let tokens = collect_child_tokens node in
  match token_vector_find_kind tokens Kind.AS_KW with
  | Some as_index when Int.equal (Vector.length types) 2 ->
      let first = Vector.get_unchecked types ~at:0 in
      let alias = Vector.get_unchecked types ~at:1 in
      (
        match token_vector_find_kind tokens Kind.LPAREN with
        | Some index -> emit_token state (Vector.get_unchecked tokens ~at:index)
        | None -> emit_text state "("
      );
      render_type_expr state first;
      emit_space state;
      emit_token state (Vector.get_unchecked tokens ~at:as_index);
      emit_space state;
      render_type_expr state alias;
      (
        match token_vector_find_last_kind tokens Kind.RPAREN with
        | Some index -> emit_token state (Vector.get_unchecked tokens ~at:index)
        | None -> emit_text state ")"
      );
      true
  | _ -> false

and render_type_expr_attribute_suffix = fun state type_expr ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_attribute_suffix_token
    type_expr
    ~fn:(fun token -> Vector.push tokens ~value:token);
  let length = Vector.length tokens in
  let token_at_is index kind =
    Int.(index < length) && token_kind_is (Vector.get_unchecked tokens ~at:index) kind
  in
  let group_is_floating_attribute start =
    token_at_is start Kind.LBRACKET
    && token_at_is (Int.add start 1) Kind.ATAT
    && token_at_is (Int.add start 2) Kind.AT
  in
  let rec group_stop index depth =
    if Int.(index >= length) then
      length
    else
      let token = Vector.get_unchecked tokens ~at:index in
      let depth =
        if token_kind_is token Kind.LBRACKET then
          Int.add depth 1
        else if token_kind_is token Kind.RBRACKET then
          Int.sub depth 1
        else
          depth
      in
      if Int.equal depth 0 then
        Int.add index 1
      else
        group_stop (Int.add index 1) depth
  in
  let rec loop start =
    if Int.(start < length) then
      (
        let stop = group_stop start 0 in
        if group_is_floating_attribute start then
          emit_line state
        else
          emit_space state;
        emit_attribute_token_vector_range_stream state tokens ~start ~stop;
        loop stop
      )
  in
  loop 0

and render_type_arrow_left = fun state type_expr ->
  match TypeExprView.view type_expr with
  | Arrow _ ->
      emit_text state "(";
      render_type_expr state type_expr;
      emit_text state ")"
  | _ -> render_type_expr state type_expr

and render_type_apply_argument = fun state argument ->
  match TypeExprView.view argument with
  | Arrow _
  | Poly _
  | Tuple _ ->
      emit_text state "(";
      render_type_expr state argument;
      emit_text state ")"
  | _ -> render_type_expr state argument

and type_expr_flat_width = fun type_expr ->
  match TypeExprView.view type_expr with
  | Path { path } ->
      let tokens = collect_child_tokens path in
      let length = Vector.length tokens in
      let rec loop index previous total =
        if Int.(index >= length) then
          Some total
        else
          let token = Vector.get_unchecked tokens ~at:index in
          let token_width = Slice.length (Ast.Token.slice token) in
          let extra_space =
            match previous with
            | Some previous when token_wants_space_before previous token -> 1
            | _ -> 0
          in
          loop
            (Int.add index 1)
            (Some token)
            (Int.add total (Int.add extra_space token_width))
      in
      loop 0 None 0
  | Var { name = Some name } -> Some (Int.add 1 (Slice.length (Ast.Token.slice name)))
  | Var { name = None } -> None
  | Wildcard -> Some 1
  | Arrow { left = Some left; right = Some right } -> (
      match (type_expr_flat_width left, type_expr_flat_width right) with
      | (Some left_width, Some right_width) ->
          let left_width =
            match TypeExprView.view left with
            | Arrow _ -> Int.add left_width 2
            | _ -> left_width
          in
          Some Int.(left_width + 4 + right_width)
      | _ -> None
    )
  | Arrow _ -> None
  | Apply { argument = Some argument; constructor = Some constructor } -> (
      match (type_expr_flat_width argument, type_expr_flat_width constructor) with
      | (Some argument_width, Some constructor_width) ->
          let argument_width =
            match TypeExprView.view argument with
            | Arrow _
            | Poly _
            | Tuple _ -> Int.add argument_width 2
            | _ -> argument_width
          in
          Some Int.(argument_width + 1 + constructor_width)
      | _ -> None
    )
  | Apply _ -> None
  | Parenthesized { inner = Some inner } -> (
      match type_expr_flat_width inner with
      | Some inner_width -> Some (Int.add inner_width 2)
      | None -> None
    )
  | Parenthesized { inner = None } -> Some 2
  | Tuple _ -> type_tuple_flat_width type_expr
  | Labeled { optional_token; label = Some label; annotation = Some annotation } -> (
      match type_expr_flat_width annotation with
      | Some annotation_width ->
          let optional_width =
            match optional_token with
            | Some token -> Slice.length (Ast.Token.slice token)
            | None -> 0
          in
          Some Int.(optional_width + Slice.length (Ast.Token.slice label) + 1 + annotation_width)
      | None -> None
    )
  | Labeled _ -> None
  | Poly { body = Some body } -> (
      match type_expr_flat_width body with
      | None -> None
      | Some body_width ->
          let names = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
          Ast.TypeExpr.for_each_poly_type_name
            type_expr
            ~fn:(fun name -> Vector.push names ~value:name);
          let length = Vector.length names in
          if Int.equal length 0 then
            Some body_width
          else
            let keyword_width =
              match Ast.TypeExpr.poly_type_keyword_token type_expr with
              | Some token -> Int.add (token_flat_width token) 1
              | None -> 0
            in
            let rec loop index total =
              if Int.(index >= length) then
                Some Int.(keyword_width + total + 2 + body_width)
              else
                let name = Vector.get_unchecked names ~at:index in
                let name_width =
                  Int.add
                    (token_flat_width name)
                    (
                      if Option.is_some (Ast.TypeExpr.poly_type_keyword_token type_expr) then
                        0
                      else
                        1
                    )
                in
                let separator_width =
                  if Int.equal index 0 then
                    0
                  else
                    1
                in
                loop (Int.add index 1) Int.(total + separator_width + name_width)
            in
            loop 0 0
    )
  | Poly { body = None } -> None
  | Opaque node -> node_spaced_flat_width node
  | Error _
  | Unknown _ -> None

and render_poly_type_prefix = fun state type_expr ->
  let names = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_poly_type_name type_expr ~fn:(fun name -> Vector.push names ~value:name);
  let has_explicit_type_keyword =
    match Ast.TypeExpr.poly_type_keyword_token type_expr with
    | Some type_keyword ->
        emit_token state type_keyword;
        emit_space state;
        true
    | None -> false
  in
  let length = Vector.length names in
  if Int.(length > 0) then
    (
      let rec loop index =
        if Int.(index < length) then (
          if Int.(index > 0) then
            emit_space state;
          if not has_explicit_type_keyword then
            emit_text state "'";
          emit_token state (Vector.get_unchecked names ~at:index);
          loop (Int.add index 1)
        )
      in
      loop 0;
      emit_text state ".";
      emit_space state
    )

and render_poly_type_expr = fun state type_expr body ->
  render_poly_type_prefix state type_expr;
  render_type_expr state body

and render_type_tuple_separator = fun state separator ->
  match separator with
  | TypeExprView.Star ->
      emit_space state;
      emit_text state "*";
      emit_space state
  | TypeExprView.Comma ->
      emit_text state ",";
      emit_space state
  | TypeExprView.UnknownSeparator ->
      emit_space state;
      emit_text state "*";
      emit_space state

and render_tuple_type_expr = fun state type_expr separator ->
  let items = collect_child_type_exprs type_expr in
  let length = Vector.length items in
  if Int.(length < 2) then
    unsupported_node "incomplete tuple type" type_expr
  else
    emit_joined_vector
      state
      items
      ~sep:(fun () -> render_type_tuple_separator state separator)
      ~fn:(render_type_expr state)

and type_tuple_separator_width = function
  | TypeExprView.Star
  | TypeExprView.UnknownSeparator -> 3
  | TypeExprView.Comma -> 2

and type_tuple_flat_width = fun type_expr ->
  let items = collect_child_type_exprs type_expr in
  let length = Vector.length items in
  if Int.(length < 2) then
    None
  else
    let separator_width =
      match TypeExprView.view type_expr with
      | Tuple { separator; _ } -> type_tuple_separator_width separator
      | _ -> 0
    in
    let rec loop index total =
      if Int.(index >= length) then
        Some total
      else
        match type_expr_flat_width (Vector.get_unchecked items ~at:index) with
        | None -> None
        | Some item_width ->
            let total =
              if Int.equal index 0 then
                item_width
              else
                Int.(total + separator_width + item_width)
            in
            loop (Int.add index 1) total
    in
    loop 0 0

and type_expr_is_opaque_gt = fun type_expr ->
  match TypeExprView.view type_expr with
  | Opaque node ->
      let tokens = collect_child_tokens node in
      if Int.equal (Vector.length tokens) 1 then
        token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.GT
      else
        false
  | _ -> false

and type_expr_starts_with_gt = fun type_expr ->
  match TypeExprView.view type_expr with
  | Opaque _ -> type_expr_is_opaque_gt type_expr
  | Apply { argument = Some argument; _ } -> type_expr_starts_with_gt argument
  | _ -> false

and render_type_apply_argument_without_leading_gt = fun state argument ->
  match TypeExprView.view argument with
  | Arrow _
  | Poly _
  | Tuple _ ->
      emit_text state "(";
      render_type_expr_without_leading_gt state argument;
      emit_text state ")"
  | _ -> render_type_expr_without_leading_gt state argument

and render_type_expr_without_leading_gt = fun state type_expr ->
  match TypeExprView.view type_expr with
  | Opaque _ when type_expr_is_opaque_gt type_expr -> ()
  | Apply { argument = Some argument; constructor = Some constructor } when type_expr_is_opaque_gt
    argument -> render_type_expr state constructor
  | Apply { argument = Some argument; constructor = Some constructor } when type_expr_starts_with_gt
    argument ->
      render_type_apply_argument_without_leading_gt state argument;
      emit_space state;
      render_type_expr state constructor
  | _ -> render_type_expr state type_expr

and type_expr_labeled_coercion_parts = fun type_expr ->
  match TypeExprView.view type_expr with
  | Labeled { optional_token = None; label = Some label; annotation = Some annotation } when type_expr_starts_with_gt
    annotation -> Some (label, annotation)
  | _ -> None

and type_expr_is_coercion_annotation = fun type_expr ->
  type_expr_starts_with_gt type_expr || Option.is_some (type_expr_labeled_coercion_parts type_expr)

and render_typed_expr_annotation = fun state annotation ->
  match type_expr_labeled_coercion_parts annotation with
  | Some (source, target) ->
      emit_space state;
      emit_text state ":";
      emit_space state;
      emit_token state source;
      emit_space state;
      emit_text state ":>";
      emit_space state;
      render_type_expr_without_leading_gt state target
  | None ->
      if type_expr_starts_with_gt annotation then (
        emit_space state;
        emit_text state ":>";
        emit_space state;
        render_type_expr_without_leading_gt state annotation
      ) else (
        emit_text state ":";
        render_type_expr_after_tight_colon state ~suffix_width:0 annotation
      )

and render_type_expr_after_colon = fun state type_expr ->
  if type_expr_starts_with_gt type_expr then
    render_type_expr state type_expr
  else (
    emit_space state;
    render_type_expr state type_expr
  )

and type_expr_is_arrow = fun type_expr ->
  match TypeExprView.view type_expr with
  | Arrow _ -> true
  | Poly { body = Some body } -> type_expr_is_arrow body
  | _ -> false

and type_expr_is_closed_backtick_bracketed_opaque = fun type_expr ->
  match TypeExprView.view type_expr with
  | Opaque node ->
      let tokens = collect_child_tokens node in
      let length = Vector.length tokens in
      if Int.(length < 3) then
        false
      else if bracketed_opaque_type_has_inline_open tokens then
        false
      else
        let first = Vector.get_unchecked tokens ~at:0 in
        let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
        let first_row = Vector.get_unchecked tokens ~at:1 in
        token_kind_is first Kind.LBRACKET
        && token_kind_is last Kind.RBRACKET
        && token_kind_is first_row Kind.BACKTICK
  | _ -> false

and render_multiline_type_arrow = fun state type_expr ->
  match Ast.TypeExpr.inner_without_attribute_suffix type_expr with
  | Some inner ->
      render_multiline_type_arrow state inner;
      render_type_expr_attribute_suffix state type_expr
  | None -> (
      match TypeExprView.view type_expr with
      | Arrow { left = Some left; right = Some right } ->
          render_type_arrow_left state left;
          emit_space state;
          emit_text state "->";
          emit_line state;
          render_multiline_type_arrow state right
      | Arrow _ -> unsupported_node "incomplete arrow type" type_expr
      | Poly { body = Some body } ->
          render_poly_type_prefix state type_expr;
          render_multiline_type_arrow state body
      | Poly { body = None } -> unsupported_node "poly type without body" type_expr
      | _ -> render_type_expr state type_expr
    )

and type_expr_after_colon_decision = fun state ~suffix_width type_expr ->
  if type_expr_starts_with_gt type_expr then
    { Layout.mode = Layout.Inline; reasons = [] }
  else
    let flat_width = type_expr_flat_width type_expr in
    Layout.decide_type_after_separator
      Layout.Colon
      (layout_context ~role:Layout.Type_after_colon state)
      ~flat_width
      ~suffix_width

and type_expr_should_break_after_colon = fun state ~suffix_width type_expr ->
  match type_expr_after_colon_decision state ~suffix_width type_expr with
  | { Layout.mode = Break_after_separator; _ }
  | { Layout.mode = Block; _ } -> true
  | _ -> false

and render_type_expr_after_tight_colon = fun state ~suffix_width type_expr ->
  if type_expr_should_break_after_colon state ~suffix_width type_expr then (
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        if type_expr_is_arrow type_expr then
          render_multiline_type_arrow state type_expr
        else
          render_type_expr state type_expr)
  ) else
    render_type_expr_after_colon state type_expr

let pattern_tuple_has_parens = fun pattern ->
  Option.is_some
    (Ast.Node.first_child_token pattern ~kind:Kind.LPAREN)

let pattern_is_tuple = fun pattern ->
  match PatternView.view pattern with
  | Tuple -> true
  | _ -> false

let pattern_is_or = fun pattern ->
  match PatternView.view pattern with
  | Or _ -> true
  | _ -> false

let collect_tuple_pattern_items = fun pattern ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  let rec push_items pattern =
    Ast.Node.for_each_child_node
      pattern
      ~fn:(fun child ->
        match Ast.cast_result_to_option (Ast.Pattern.cast child) with
        | Some child -> (
            match PatternView.view child with
            | Tuple when not (pattern_tuple_has_parens child) -> push_items child
            | _ -> Vector.push items ~value:child
          )
        | None -> ())
  in
  push_items pattern;
  items

let path_single_ident_token = fun path ->
  let found = ref None in
  let count = ref 0 in
  Ast.Path.for_each_ident
    path
    ~fn:(fun token ->
      count := Int.add !count 1;
      if Int.equal !count 1 then
        found := Some token);
  if Int.equal !count 1 then
    !found
  else
    None

let rec pattern_binding_ident_token = fun pattern ->
  match PatternView.view pattern with
  | Path { path } -> path_single_ident_token path
  | Parenthesized { inner = Some inner }
  | Constraint { pattern = Some inner; _ }
  | Attribute { inner = Some inner } -> pattern_binding_ident_token inner
  | _ -> None

let parameter_pattern_matches_label = fun label pattern ->
  match pattern_binding_ident_token pattern with
  | Some binding -> Ast.Token.text_equal label binding
  | None -> false

let split_typed_parameter_pattern = fun pattern ->
  match PatternView.view pattern with
  | Constraint { pattern = Some pattern; annotation = Some annotation } ->
      Some (pattern, annotation)
  | _ -> None

let render_first_class_module_pattern_path = fun state pattern for_each_path_ident message ->
  let segments = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  for_each_path_ident ~fn:(fun ident -> Vector.push segments ~value:ident);
  if Vector.length segments = 0 then
    unsupported_node message pattern
  else
    emit_joined_vector state segments ~sep:(fun () -> emit_text state ".") ~fn:(emit_token state)

let render_first_class_module_pattern_ascription_tokens = fun state pattern colon_token ->
  let tokens = collect_child_tokens pattern in
  match (token_vector_find_kind tokens Kind.COLON, token_vector_find_kind tokens Kind.RPAREN) with
  | (Some colon_index, Some closing_index) when Int.(Int.add colon_index 1 < closing_index) ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      emit_token_vector_range_stream
        state
        tokens
        ~start:(Int.add colon_index 1)
        ~stop:closing_index
  | _ -> unsupported_node "unsupported first-class module pattern ascription" pattern

let render_first_class_module_pattern_ascription = fun state pattern ->
  match (
    Ast.FirstClassModulePattern.colon_token pattern,
    Ast.FirstClassModulePattern.ascription pattern
  ) with
  | (None, Ast.FirstClassModulePattern.NoAscription) -> ()
  | (Some colon_token, Ast.FirstClassModulePattern.PathAscription) ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      render_first_class_module_pattern_path
        state
        pattern
        (Ast.FirstClassModulePattern.for_each_ascription_path_ident pattern)
        "first-class module pattern without module type path"
  | (Some colon_token, Ast.FirstClassModulePattern.UnsupportedAscription) ->
      render_first_class_module_pattern_ascription_tokens state pattern colon_token
  | (Some _, Ast.FirstClassModulePattern.NoAscription)
  | (None, Ast.FirstClassModulePattern.PathAscription)
  | (None, Ast.FirstClassModulePattern.UnsupportedAscription) ->
      unsupported_node "unsupported first-class module pattern ascription" pattern

let render_first_class_module_pattern = fun state pattern ->
  let module_pattern =
    match Ast.cast_result_to_option (Ast.FirstClassModulePattern.cast pattern) with
    | Some module_pattern -> module_pattern
    | None -> unsupported_node "unsupported first-class module pattern" pattern
  in
  match (
    Ast.FirstClassModulePattern.opening_token module_pattern,
    Ast.FirstClassModulePattern.module_token module_pattern,
    Ast.FirstClassModulePattern.binder module_pattern,
    Ast.FirstClassModulePattern.closing_token module_pattern
  ) with
  | (Some opening_token, Some module_token, Some binder, Some closing_token) ->
      emit_token state opening_token;
      emit_token state module_token;
      emit_space state;
      emit_token state binder;
      render_first_class_module_pattern_ascription state module_pattern;
      emit_token state closing_token
  | _ -> unsupported_node "incomplete first-class module pattern" pattern

let parameter_pattern_requires_parens = fun pattern ->
  match PatternView.view pattern with
  | Alias _ -> true
  | _ -> false

let record_pattern_fields_have_leading_comment = fun (fields: Ast.record_pattern_field_view Vector.t) ->
  let found = ref false in
  Vector.for_each
    fields
    ~fn:(fun field ->
      let node =
        match field with
        | Ast.RecordPatternField { node; _ }
        | Ast.UnknownRecordPatternField { node } -> node
      in
      if node_has_leading_comment node then
        found := true);
  !found

let record_pattern_should_multiline = fun pattern fields ->
  Int.(Vector.length fields > 3) || record_pattern_fields_have_leading_comment fields

let parameter_token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  if Kind.(previous_kind = COLON || current_kind = COLON) then
    false
  else
    token_wants_space_before previous token

let emit_parameter_token_stream = fun state parameter ->
  let previous = ref None in
  Ast.Node.for_each_token
    parameter
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when parameter_token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let parameter_colon_has_leading_space = fun parameter ->
  let found = ref None in
  Ast.Node.for_each_token
    parameter
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Kind.(Ast.Token.kind token = COLON) then
            found := Some token);
  match !found with
  | Some colon -> Ast.Token.has_leading_whitespace colon
  | None -> false

let parenthesized_pattern_should_break = fun state pattern ->
  let previous = ref None in
  let width = ref 0 in
  Ast.Node.for_each_token
    pattern
    ~fn:(fun token ->
      let extra_space =
        match !previous with
        | Some previous when token_wants_space_before previous token -> 1
        | _ -> 0
      in
      width := Int.add !width (Int.add extra_space (token_flat_width token));
      previous := Some token);
  Int.(state.column + !width + 2 > state.width)

let rec render_pattern = fun state pattern ->
  match PatternView.view pattern with
  | Wildcard -> emit_text state "_"
  | Path { path } -> render_pattern_path state path
  | Literal { token = Some token } ->
      (
        match Ast.Pattern.literal_sign_token pattern with
        | Some sign -> emit_token state sign
        | None -> ()
      );
      emit_literal_token state token
  | Literal { token = None } -> unsupported_node "literal pattern without token" pattern
  | Construct { callee = Some callee; argument = Some argument } ->
      render_pattern state callee;
      emit_space state;
      render_pattern_atom state argument
  | Construct _ -> unsupported_node "incomplete construct pattern" pattern
  | Parenthesized { inner = Some inner } when pattern_is_tuple inner ->
      render_tuple_pattern state inner
  | Parenthesized { inner = Some inner } when pattern_is_operator_name inner ->
      emit_text state "(";
      emit_space state;
      render_pattern state inner;
      emit_space state;
      emit_text state ")"
  | Parenthesized { inner = Some inner } when pattern_is_or inner
  && not (parenthesized_pattern_should_break state inner) ->
      emit_text state "(";
      render_or_pattern_inline state inner;
      emit_text state ")"
  | Parenthesized { inner = Some inner } when parenthesized_pattern_should_break state inner ->
      emit_text state "(";
      emit_line state;
      with_indent state 2 (fun () -> render_pattern state inner);
      emit_line state;
      emit_text state ")"
  | Parenthesized { inner = Some inner } ->
      emit_text state "(";
      render_pattern state inner;
      emit_text state ")"
  | Parenthesized { inner = None } -> render_parenthesized_empty_pattern state pattern
  | Constraint { pattern = Some pattern; annotation = Some annotation } ->
      render_pattern state pattern;
      drop_pending_spaces state;
      emit_text state ":";
      render_type_expr_after_tight_colon state ~suffix_width:0 annotation
  | Constraint _ -> unsupported_node "incomplete constraint pattern" pattern
  | Alias { pattern = Some pattern; alias = Some alias } ->
      render_pattern state pattern;
      emit_space state;
      emit_text state "as";
      emit_space state;
      render_pattern state alias
  | Alias _ -> unsupported_node "incomplete alias pattern" pattern
  | Or { left = Some _; right = Some _ } ->
      let items = collect_or_pattern_items pattern in
      let length = Vector.length items in
      let rec loop index =
        if Int.(index < length) then (
          if Int.(index > 0) then (
            emit_line state;
            emit_text state "|";
            emit_space state
          );
          render_pattern state (Vector.get_unchecked items ~at:index);
          loop (Int.add index 1)
        )
      in
      loop 0
  | Or _ -> unsupported_node "incomplete or pattern" pattern
  | Tuple -> render_tuple_pattern state pattern
  | List ->
      let items = collect_child_patterns pattern in
      if Int.equal (Vector.length items) 0 then
        emit_text state "[]"
      else (
        emit_text state "[ ";
        emit_joined_vector
          state
          items
          ~sep:(fun () ->
            emit_text state ";";
            emit_space state)
          ~fn:(render_pattern state);
        emit_space state;
        emit_text state "]"
      )
  | Array ->
      let items = collect_child_patterns pattern in
      emit_text state "[|";
      emit_joined_vector
        state
        items
        ~sep:(fun () ->
          emit_text state ";";
          emit_space state)
        ~fn:(render_pattern state);
      emit_text state "|]"
  | Record -> render_record_pattern state pattern
  | PolyVariant -> render_poly_variant_pattern state pattern
  | Interval { left = Some left; right = Some right } ->
      render_pattern state left;
      emit_space state;
      emit_text state "..";
      emit_space state;
      render_pattern state right
  | Interval _ -> unsupported_node "incomplete interval pattern" pattern
  | Cons { head = Some head; tail = Some tail } ->
      (
        match PatternView.view head with
        | Construct _ -> render_pattern state head
        | _ -> render_pattern_atom state head
      );
      emit_space state;
      emit_text state "::";
      emit_space state;
      render_pattern state tail
  | Cons _ -> unsupported_node "incomplete cons pattern" pattern
  | Lazy { pattern = Some pattern } ->
      emit_text state "lazy";
      emit_space state;
      render_pattern state pattern
  | Lazy _ -> unsupported_node "lazy pattern without payload" pattern
  | Exception { pattern = Some pattern } ->
      emit_text state "exception";
      emit_space state;
      render_pattern state pattern
  | Exception _ -> unsupported_node "exception pattern without payload" pattern
  | LabeledParam parameter
  | OptionalParam parameter
  | OptionalParamDefault parameter -> render_parameter state parameter
  | LocallyAbstractType -> emit_token_stream state pattern
  | FirstClassModule -> render_first_class_module_pattern state pattern
  | LocalOpen -> render_local_open_pattern state pattern
  | Extension -> render_extension_pattern state pattern
  | Attribute { inner = Some _ } -> render_attribute_pattern state pattern
  | Attribute { inner = None } -> unsupported_node "attribute pattern without inner pattern" pattern
  | Error _
  | Unknown _ -> unsupported_node "unsupported pattern" pattern

and render_or_pattern_inline = fun state pattern ->
  let items = collect_or_pattern_items pattern in
  emit_joined_vector
    state
    items
    ~sep:(fun () ->
      emit_space state;
      emit_text state "|";
      emit_space state)
    ~fn:(render_pattern state)

and render_tuple_pattern = fun state pattern ->
  if parenthesized_pattern_should_break state pattern then
    (
      let items = collect_tuple_pattern_items pattern in
      let length = Vector.length items in
      emit_text state "(";
      emit_line state;
      with_indent
        state
        2
        (fun () ->
          let rec loop index =
            if Int.(index < length) then
              (
                let item = Vector.get_unchecked items ~at:index in
                if pattern_is_tuple item then
                  render_pattern_atom state item
                else
                  render_pattern state item;
                if Int.(index < Int.sub length 1) then (
                  emit_text state ",";
                  emit_line state
                );
                loop (Int.add index 1)
              )
          in
          loop 0);
      emit_line state;
      emit_text state ")"
    )
  else (
    emit_text state "(";
    render_tuple_pattern_contents state pattern;
    emit_text state ")"
  )

and render_tuple_pattern_contents = fun state pattern ->
  let items = collect_tuple_pattern_items pattern in
  emit_joined_vector
    state
    items
    ~sep:(fun () ->
      emit_text state ",";
      emit_space state)
    ~fn:(fun item ->
      if pattern_is_tuple item then
        render_pattern_atom state item
      else
        render_pattern state item)

and render_parameter_pattern = fun state parameter pattern ->
  let needs_parens =
    Ast.Parameter.has_explicit_pattern_parens parameter || parameter_pattern_requires_parens pattern
  in
  if needs_parens then (
    emit_text state "(";
    render_pattern state pattern;
    emit_text state ")"
  ) else
    render_pattern state pattern

and render_named_parameter = fun state ~sigil parameter label pattern ->
  match split_typed_parameter_pattern pattern with
  | Some (inner, annotation) when parameter_pattern_matches_label label inner ->
      emit_text state sigil;
      emit_text state "(";
      render_pattern state inner;
      emit_text state ":";
      render_type_expr state annotation;
      emit_text state ")"
  | _ when parameter_pattern_matches_label label pattern ->
      emit_text state sigil;
      emit_token state label
  | _ ->
      emit_text state sigil;
      emit_token state label;
      emit_text state ":";
      render_parameter_pattern state parameter pattern

and render_parameter = fun state parameter ->
  match Ast.Parameter.view parameter with
  | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } -> render_pattern state pattern
  | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = None } ->
      unsupported_node "positional parameter without pattern" parameter
  | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; pattern = None } ->
      emit_text state "~";
      emit_token state label
  | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; pattern = Some pattern } ->
      render_named_parameter state ~sigil:"~" parameter label pattern
  | Ast.Parameter.Param { label = Ast.Parameter.Labeled _; _ } ->
      unsupported_node "labeled parameter without label" parameter
  | Ast.Parameter.Param { label = Ast.Parameter.Optional { name = Some label; default = None }; pattern = None } ->
      emit_text state "?";
      emit_token state label
  | Ast.Parameter.Param {
      label = Ast.Parameter.Optional {
        name = Some label;
        default = None;
      };
      pattern = Some pattern;
    } ->
      render_named_parameter state ~sigil:"?" parameter label pattern
  | Ast.Parameter.Param { label = Ast.Parameter.Optional { default = Some _; _ }; _ } ->
      emit_parameter_token_stream state parameter
  | Ast.Parameter.Param { label = Ast.Parameter.Optional _; _ } ->
      unsupported_node "optional parameter without label" parameter
  | Ast.Parameter.Unknown _ -> unsupported_node "unsupported parameter" parameter

and loose_parameter_binding_annotation = fun parameter ->
  match PatternView.view parameter with
  | LabeledParam parameter -> (
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param { label = Ast.Parameter.Labeled _; pattern = Some annotation } when (not
        (Ast.Parameter.has_explicit_pattern_parens parameter))
      && parameter_colon_has_leading_space parameter -> Some annotation
      | _ -> None
    )
  | OptionalParam parameter -> (
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param {
          label = Ast.Parameter.Optional { default = None; _ };
          pattern = Some annotation;
        } when (not
        (Ast.Parameter.has_explicit_pattern_parens parameter))
      && parameter_colon_has_leading_space parameter -> Some annotation
      | _ -> None
    )
  | _ -> None

and render_parameter_label_only = fun state parameter ->
  match PatternView.view parameter with
  | LabeledParam parameter -> (
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ } ->
          emit_text state "~";
          emit_token state label
      | _ -> emit_parameter_token_stream state parameter
    )
  | OptionalParam parameter -> (
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param { label = Ast.Parameter.Optional { name = Some label; _ }; _ } ->
          emit_text state "?";
          emit_token state label
      | _ -> emit_parameter_token_stream state parameter
    )
  | _ -> render_pattern state parameter

and pattern_is_named_parameter = fun pattern ->
  let rec loop pattern =
    match PatternView.view pattern with
    | LabeledParam _
    | OptionalParam _
    | OptionalParamDefault _ -> true
    | Constraint { pattern = Some inner; _ }
    | Parenthesized { inner = Some inner }
    | Attribute { inner = Some inner } -> loop inner
    | _ -> false
  in
  loop pattern

and pattern_ends_with_named_parameter = fun pattern ->
  let rec loop pattern =
    match PatternView.view pattern with
    | Construct { argument = Some argument; _ } -> loop argument
    | Constraint { pattern = Some inner; _ }
    | Parenthesized { inner = Some inner }
    | Attribute { inner = Some inner } -> loop inner
    | LabeledParam _
    | OptionalParam _
    | OptionalParamDefault _ -> true
    | _ -> false
  in
  loop pattern

and render_local_open_pattern = fun state pattern ->
  match Ast.LocalOpenPattern.view pattern with
  | Delimited {
    module_path;
    dot_token;
    opening_token = opening;
    pattern = body;
    closing_token = closing;
  } ->
      emit_joined_vector
        state
        module_path
        ~sep:(fun () -> emit_text state ".")
        ~fn:(emit_token state);
      emit_token state dot_token;
      if token_kind_is opening Kind.LPAREN then (
        emit_token state opening;
        (
          if pattern_is_tuple body then
            render_tuple_pattern_contents state body
          else
            render_pattern state body
        );
        emit_token state closing
      ) else
        render_pattern state body
  | Unknown node -> unsupported_node "unsupported local-open pattern" node

and render_poly_variant_pattern = fun state pattern ->
  if node_has_token_kind pattern Kind.HASH then
    (
      let children = collect_child_patterns pattern in
      match Vector.length children with
      | 1 ->
          emit_text state "#";
          render_pattern state (Vector.get_unchecked children ~at:0)
      | _ -> unsupported_node "polymorphic variant inherit pattern without path" pattern
    )
  else (
    (
      match first_ident_token pattern with
      | Some tag ->
          emit_text state "`";
          emit_token state tag
      | None -> unsupported_node "polymorphic variant pattern without tag" pattern
    );
    let children = collect_child_patterns pattern in
    match Vector.length children with
    | 0 -> ()
    | 1 ->
        emit_space state;
        render_pattern state (Vector.get_unchecked children ~at:0)
    | _ -> unsupported_node "polymorphic variant pattern with multiple payloads" pattern
  )

and render_record_pattern = fun state pattern ->
  let fields = collect_record_pattern_fields pattern in
  let length = Vector.length fields in
  let render_field (field: Ast.record_pattern_field_view) =
    match field with
    | Ast.RecordPatternField { path; pattern; _ } -> (
        render_path state path;
        match pattern with
        | Some pattern ->
            emit_space state;
            emit_text state "=";
            emit_space state;
            render_pattern state pattern
        | None -> ()
      )
    | Ast.UnknownRecordPatternField { node } -> render_pattern state node
  in
  emit_text state "{";
  if record_pattern_should_multiline pattern fields then (
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let rec loop index =
          if Int.(index < length) then (
            render_field (Vector.get_unchecked fields ~at:index);
            if
              Int.(index < Int.sub length 1)
              || Option.is_some (Ast.RecordPattern.open_wildcard pattern)
            then
              emit_text state ";";
            emit_line state;
            loop (Int.add index 1)
          )
        in
        loop 0;
        match Ast.RecordPattern.open_wildcard pattern with
        | Some _ ->
            emit_text state "_";
            emit_line state
        | None -> ());
    emit_text state "}"
  ) else if Int.(length > 0) then (
    emit_space state;
    emit_joined_vector
      state
      fields
      ~sep:(fun () ->
        emit_text state ";";
        emit_space state)
      ~fn:render_field;
    (
      match Ast.RecordPattern.open_wildcard pattern with
      | Some _ ->
          if Int.(length > 0) then (
            emit_text state ";";
            emit_space state
          );
          emit_text state "_"
      | None -> ()
    );
    emit_space state;
    emit_text state "}"
  ) else
    emit_text state "}"

and render_pattern_atom = fun state pattern ->
  match PatternView.view pattern with
  | Constraint _ when parenthesized_pattern_should_break state pattern ->
      emit_text state "(";
      emit_line state;
      with_indent state 2 (fun () -> render_pattern state pattern);
      emit_line state;
      emit_text state ")"
  | Tuple -> render_pattern state pattern
  | Construct _
  | Or _
  | Cons _
  | Alias _
  | Constraint _ ->
      emit_text state "(";
      render_pattern state pattern;
      emit_text state ")"
  | _ -> render_pattern state pattern

and render_extension_pattern = fun state pattern ->
  match Ast.cast_result_to_option (Ast.ExtensionPattern.cast pattern) with
  | Some extension ->
      emit_shell_token_stream
        state
        (fun ~fn -> Ast.ExtensionPattern.for_each_shell_token extension ~fn)
  | None -> unsupported_node "unsupported extension pattern" pattern

and render_attribute_pattern = fun state pattern ->
  match Ast.cast_result_to_option (Ast.AttributePattern.cast pattern) with
  | Some attribute -> (
      match Ast.AttributePattern.inner attribute with
      | Some inner ->
          render_pattern state inner;
          emit_space state;
          emit_shell_token_stream
            state
            (fun ~fn -> Ast.AttributePattern.for_each_shell_token attribute ~fn)
      | None -> unsupported_node "attribute pattern without inner pattern" pattern
    )
  | None -> unsupported_node "unsupported attribute pattern" pattern

let rec pattern_contains_type_annotation = fun pattern annotation ->
  match PatternView.view pattern with
  | Constraint { annotation = Some existing; _ } when same_ast_node existing annotation -> true
  | _ ->
      let found = ref false in
      Ast.Pattern.for_each_child_pattern
        pattern
        ~fn:(fun child ->
          if not !found && not (same_ast_node pattern child) then
            found := pattern_contains_type_annotation child annotation);
      !found

let rec pattern_should_render_multiline = fun pattern ->
  match PatternView.view pattern with
  | Record ->
      let fields = collect_record_pattern_fields pattern in
      record_pattern_should_multiline pattern fields
  | LocalOpen -> (
      match Ast.LocalOpenPattern.view pattern with
      | Delimited { pattern = body; _ } -> pattern_should_render_multiline body
      | Unknown _ -> false
    )
  | Parenthesized { inner = Some inner }
  | Constraint { pattern = Some inner; _ }
  | Attribute { inner = Some inner } -> pattern_should_render_multiline inner
  | Construct { callee; argument } ->
      (
        match callee with
        | Some callee when pattern_should_render_multiline callee -> true
        | _ -> false
      ) || (
        match argument with
        | Some argument -> pattern_should_render_multiline argument
        | None -> false
      )
  | _ -> false

let expr_first_token_text_is = fun expr expected ->
  match Ast.Node.first_descendant_token expr with
  | Some token -> token_text_is token expected
  | None -> false

let expr_has_leading_comment = fun expr -> node_has_leading_comment expr

let collect_apply = fun expr ->
  let args = Vector.with_capacity ~size:4 in
  let rec loop budget expr =
    if Int.(budget <= 0) then
      unsupported_node "cyclic apply expression" expr;
    match ExprView.view expr with
    | Apply { callee = Some callee; argument = Some argument } when not (same_ast_node expr callee)
    && not (same_ast_node expr argument) ->
        Vector.push args ~value:argument;
        loop (Int.sub budget 1) callee
    | _ -> expr
  in
  let callee = loop 128 expr in
  if Int.(Vector.length args > 1) then
    Vector.reverse args;
  (callee, args)

let expr_classification_budget = 512

let rec expr_is_multiline = fun expr ->
  expr_is_multiline_with_budget
    expr_classification_budget
    expr

and expr_is_multiline_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    unsupported_node "cyclic expression while classifying multiline layout" expr;
  let next_budget = Int.sub budget 1 in
  match ExprView.view expr with
  | Sequence { left = Some left; right = None } when not (same_ast_node expr left) -> true
  | If _
  | Let _
  | Match _
  | Function _
  | Try _
  | LetModule _
  | LetException _
  | Sequence _
  | While _
  | For _ -> true
  | BindingOperator -> binding_operator_expr_is_multiline_with_budget next_budget expr
  | LocalOpen _ -> (
      match Ast.LocalOpenExpr.view expr with
      | LetOpen { body; _ } when (
        match ExprView.view body with
        | LocalOpen _ -> true
        | _ -> false
      ) -> true
      | LetOpen { body; _ } when not (same_ast_node expr body) ->
          expr_is_multiline_with_budget next_budget body
      | LetOpen _ -> true
      | Delimited { body; _ } when not (same_ast_node expr body) ->
          expr_is_multiline_with_budget next_budget body
      | Delimited _
      | Unknown _ -> false
    )
  | Fun { body = Some body } -> expr_is_multiline_with_budget next_budget body
  | Fun { body = None } -> true
  | Record
  | RecordUpdate -> not (record_expr_should_inline expr)
  | Parenthesized { inner = Some inner } when expr_first_token_text_is expr "begin" ->
      expr_is_multiline_with_budget next_budget inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_multiline_with_budget next_budget inner
  | Array -> not (array_expr_should_inline expr)
  | List -> not (list_expr_should_inline expr)
  | Infix { left = Some left; operator = Some operator; right = Some right } when String.equal
    (infix_operator_text_from_expr expr left operator right)
    "|>" -> true
  | Infix { left; right; _ } ->
      if expr_is_long_breaking_infix_chain expr then
        true
      else if option_expr_child_is_multiline_with_budget next_budget expr left then
        true
      else
        option_expr_child_is_multiline_with_budget next_budget expr right
  | Apply _ ->
      let (_, args) = collect_apply expr in
      vector_exists_expr_is_multiline_except_with_budget next_budget expr args
  | LabeledArg { value = Some value; _ }
  | OptionalArg { value = Some value; _ } -> expr_is_multiline_with_budget next_budget value
  | _ -> false

and expr_is_parenthesized_multiline = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_multiline inner
  | _ -> false

and same_operator_infix_count = fun budget expr operator_text ->
  if Int.(budget <= 0) then
    0
  else
    match ExprView.view expr with
    | Infix { left = Some left; operator = Some operator; right = Some right } when String.equal
      (infix_operator_text_from_expr expr left operator right)
      operator_text ->
        Int.(1
        + same_operator_infix_count
          (Int.sub budget 1)
          left
          operator_text
        + same_operator_infix_count
          (Int.sub budget 1)
          right
          operator_text)
    | _ -> 0

and infix_operator_breaks_long_chain = fun operator_text ->
  String.equal operator_text "+"
  || String.equal operator_text "&&"
  || String.equal operator_text "&"
  || String.equal operator_text "||"
  || String.equal operator_text "or"

and infix_operator_class = fun operator_text -> {
  Layout.text = operator_text;
  always_breaks_pipeline = String.equal operator_text "|>";
  breaks_when_long = infix_operator_breaks_long_chain operator_text;
}

and decide_structural_infix_chain = fun expr left operator right ->
  let operator_text = infix_operator_text_from_expr expr left operator right in
  let item_count = same_operator_infix_count expr_classification_budget expr operator_text in
  Layout.decide_infix_chain
    (Layout.make_context ~width:Int.max_int ~column:0 ~indent:0 ())
    (infix_operator_class operator_text)
    ~flat_width:(Some 0)
    ~item_count

and expr_is_long_breaking_infix_chain = fun expr ->
  match ExprView.view expr with
  | Infix { left = Some left; operator = Some operator; right = Some right } -> (
      match decide_structural_infix_chain expr left operator right with
      | { Layout.mode = Vertical; reasons } ->
          Layout.has_reason (Layout.Long_infix_chain { operator = ""; terms = 0 }) reasons
      | _ -> false
    )
  | _ -> false

and expr_is_tuple = fun expr ->
  match ExprView.view expr with
  | Tuple -> true
  | _ -> false

and expr_is_sequence = fun expr ->
  match ExprView.view expr with
  | Sequence _ -> true
  | _ -> false

and expr_has_terminal_trailing_sequence = fun expr ->
  expr_has_terminal_trailing_sequence_with_budget
    expr_classification_budget
    expr

and expr_has_terminal_trailing_sequence_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    unsupported_node "cyclic expression while checking terminal sequence" expr;
  let next_budget = Int.sub budget 1 in
  match ExprView.view expr with
  | Sequence { left = Some left; right = None } when not (same_ast_node expr left) -> true
  | Infix { right = Some right; _ } when not (same_ast_node expr right) ->
      expr_has_terminal_trailing_sequence_with_budget next_budget right
  | Match { first_case = Some _; _ } ->
      let cases = collect_match_cases expr in
      let length = Vector.length cases in
      if Int.equal length 0 then
        false
      else
        (
          match Ast.MatchCase.body (Vector.get_unchecked cases ~at:(Int.sub length 1)) with
          | Some body when not (same_ast_node expr body) ->
              expr_has_terminal_trailing_sequence_with_budget next_budget body
          | Some _
          | None -> false
        )
  | If { else_branch = Some else_branch; _ } when not (same_ast_node expr else_branch) ->
      expr_has_terminal_trailing_sequence_with_budget next_budget else_branch
  | Let { body = Some body; _ } when not (same_ast_node expr body) ->
      expr_has_terminal_trailing_sequence_with_budget next_budget body
  | _ -> false

and expr_is_apply = fun expr ->
  match ExprView.view expr with
  | Apply _ -> true
  | _ -> false

and expr_is_unit = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = None } -> true
  | _ -> false

and expr_is_prefix = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) -> expr_is_prefix inner
  | Prefix _ -> true
  | _ -> false

and expr_is_prefix_operator_text = fun expr expected ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_prefix_operator_text inner expected
  | Prefix { operator = Some operator; _ } ->
      let operator_tokens = collect_prefix_operator_tokens expr in
      let operator =
        if Int.equal (Vector.length operator_tokens) 0 then
          operator
        else
          Vector.get_unchecked operator_tokens ~at:(Int.sub (Vector.length operator_tokens) 1)
      in
      token_text_is operator expected
  | _ -> false

and expr_is_prefix_deref = fun expr -> expr_is_prefix_operator_text expr "!"

and expr_is_parenthesized_apply = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) -> expr_is_apply inner
  | _ -> false

and expr_unwrap_redundant_parentheses_for_delimited_render = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
  && not (expr_first_token_text_is expr "begin") ->
      expr_unwrap_redundant_parentheses_for_delimited_render inner
  | _ -> expr

and expr_is_fun = fun expr ->
  match ExprView.view expr with
  | Fun _ -> true
  | _ -> false

and expr_is_typed = fun expr ->
  match ExprView.view expr with
  | Typed _ -> true
  | _ -> false

and local_open_expr_can_render_atom_without_parens = fun expr ->
  match Ast.LocalOpenExpr.view expr with
  | Delimited _ -> true
  | LetOpen _
  | Unknown _ -> false

and expr_can_render_atom_without_parens = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_can_render_atom_without_parens inner
  | Tuple -> true
  | LocalOpen _ -> local_open_expr_can_render_atom_without_parens expr
  | Path _
  | Literal _
  | FieldAccess _
  | Prefix _
  | Parenthesized _
  | Array
  | List
  | Record
  | RecordUpdate
  | ArrayIndex _
  | StringIndex _
  | FirstClassModule
  | Extension
  | LabeledArg _
  | OptionalArg _
  | PolyVariant { payload = None } -> true
  | _ -> false

and expr_can_render_record_update_base_without_parens = fun expr ->
  expr_can_render_atom_without_parens expr && not (expr_is_prefix expr)

and expr_can_render_postfix_target_without_parens = fun expr ->
  expr_can_render_atom_without_parens expr && not (expr_is_prefix expr)

and expr_postfix_target_flat_width = fun budget expr ->
  match expr_flat_width_with_budget budget expr with
  | Some width ->
      if expr_can_render_postfix_target_without_parens expr then
        Some width
      else
        Some (Int.add width 2)
  | None -> None

and expr_tuple_has_parens = fun expr ->
  Option.is_some
    (Ast.Node.first_child_token expr ~kind:Kind.LPAREN)

and collect_tuple_expr_items = fun expr ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  let rec push_items expr =
    Ast.Expr.for_each_child_expr
      expr
      ~fn:(fun child ->
        match ExprView.view child with
        | Tuple when not (expr_tuple_has_parens child) -> push_items child
        | _ -> Vector.push items ~value:child)
  in
  push_items expr;
  items

and vector_exists_expr_is_multiline_except = fun parent exprs ->
  vector_exists_expr_is_multiline_except_with_budget
    expr_classification_budget
    parent
    exprs

and vector_exists_expr_is_multiline_except_with_budget = fun budget parent exprs ->
  let length = Vector.length exprs in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      let expr = Vector.get_unchecked exprs ~at:index in
      if same_ast_node parent expr then
        loop (Int.add index 1)
      else if expr_is_multiline_with_budget budget expr then
        true
      else
        loop (Int.add index 1)
  in
  loop 0

and binding_operator_expr_is_multiline_with_budget = fun _budget _expr -> true

and option_expr_child_is_multiline = fun parent child ->
  option_expr_child_is_multiline_with_budget
    expr_classification_budget
    parent
    child

and option_expr_child_is_multiline_with_budget = fun budget parent child ->
  match child with
  | Some child ->
      if same_ast_node parent child then
        false
      else
        expr_is_multiline_with_budget budget child
  | None -> false

and token_vector_flat_width = fun tokens ->
  let length = Vector.length tokens in
  let rec loop index previous total =
    if Int.(index >= length) then
      Some total
    else
      let token = Vector.get_unchecked tokens ~at:index in
      let extra_space =
        match previous with
        | Some previous when token_wants_space_before previous token -> 1
        | _ -> 0
      in
      loop
        (Int.add index 1)
        (Some token)
        (Int.add total (Int.add extra_space (token_flat_width token)))
  in
  loop 0 None 0

and node_token_flat_width = fun node ->
  let previous = ref None in
  let total = ref 0 in
  Ast.Node.for_each_token
    node
    ~fn:(fun token ->
      let extra_space =
        match !previous with
        | Some previous when token_wants_space_before previous token -> 1
        | _ -> 0
      in
      total := Int.add !total (Int.add extra_space (token_flat_width token));
      previous := Some token);
  Some !total

and expr_flat_width = fun expr -> expr_flat_width_with_budget expr_classification_budget expr

and expr_flat_width_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    None
  else
    let next_budget = Int.sub budget 1 in
    match ExprView.view expr with
    | Path { path } -> node_token_flat_width path
    | Literal { token = Some token } -> Some (token_flat_width token)
    | Literal { token = None } -> None
    | FieldAccess { target = Some target; field = Some field } -> (
        match expr_postfix_target_flat_width next_budget target with
        | Some target_width -> Some Int.(target_width + 1 + token_flat_width field)
        | None -> None
      )
    | FieldAccess _ -> None
    | Prefix { operator = Some operator; operand = Some operand } -> (
        let operator_tokens = collect_prefix_operator_tokens expr in
        let operator_width =
          match token_vector_flat_width operator_tokens with
          | Some width when Int.(width > 0) -> width
          | Some _
          | None -> token_flat_width operator
        in
        match expr_flat_width_with_budget next_budget operand with
        | Some operand_width ->
            let space_width =
              if token_text_is operator "not" then
                1
              else
                0
            in
            Some Int.(operator_width + space_width + operand_width)
        | None -> None
      )
    | Prefix _ -> None
    | Infix { left = Some left; operator = Some operator; right = Some right } -> (
        match (
          expr_flat_width_with_budget next_budget left,
          expr_flat_width_with_budget next_budget right
        ) with
        | (Some left_width, Some right_width) ->
            Some Int.(left_width
            + 2
            + infix_operator_flat_width_from_expr expr left operator right
            + right_width)
        | _ -> None
      )
    | Infix _ -> None
    | Apply _ ->
        let (callee, args) = collect_apply expr in
        if same_ast_node expr callee then
          None
        else
          (
            match expr_flat_width_with_budget next_budget callee with
            | None -> None
            | Some callee_width ->
                let length = Vector.length args in
                let rec loop index total =
                  if Int.(index >= length) then
                    Some total
                  else
                    let arg = Vector.get_unchecked args ~at:index in
                    match expr_flat_width_with_budget next_budget arg with
                    | None -> None
                    | Some arg_width ->
                        let arg_width =
                          if expr_can_render_atom_without_parens arg then
                            arg_width
                          else
                            Int.add arg_width 2
                        in
                        loop (Int.add index 1) Int.(total + 1 + arg_width)
                in
                loop 0 callee_width
          )
    | LabeledArg { label = Some label; value = Some value } -> (
        match expr_flat_width_with_budget next_budget value with
        | Some value_width ->
            let value_width =
              if expr_can_render_atom_without_parens value then
                value_width
              else
                Int.add value_width 2
            in
            Some Int.(1 + token_flat_width label + 1 + value_width)
        | None -> None
      )
    | LabeledArg { label = Some label; value = None } -> Some Int.(1 + token_flat_width label)
    | LabeledArg { label = None; _ } -> None
    | OptionalArg { label = Some label; value = Some value } -> (
        match expr_flat_width_with_budget next_budget value with
        | Some value_width ->
            let value_width =
              if expr_can_render_atom_without_parens value then
                value_width
              else
                Int.add value_width 2
            in
            Some Int.(1 + token_flat_width label + 1 + value_width)
        | None -> None
      )
    | OptionalArg { label = Some label; value = None } -> Some Int.(1 + token_flat_width label)
    | OptionalArg { label = None; _ } -> None
    | Fun { body = Some body } -> (
        match expr_flat_width_with_budget next_budget body with
        | None -> None
        | Some body_width ->
            let params = collect_fun_parameters expr in
            let length = Vector.length params in
            let rec loop index total =
              if Int.(index >= length) then
                Some Int.(total + 4 + body_width)
              else
                match node_token_flat_width (Vector.get_unchecked params ~at:index) with
                | None -> None
                | Some param_width -> loop (Int.add index 1) Int.(total + 1 + param_width)
            in
            loop 0 3
      )
    | Fun { body = None } -> None
    | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
    && expr_is_tuple inner -> expr_flat_width_with_budget next_budget inner
    | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
    && expr_is_typed inner -> (
        match expr_flat_width_with_budget next_budget inner with
        | Some inner_width -> Some (Int.add inner_width 2)
        | None -> None
      )
    | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
        expr_flat_width_with_budget next_budget inner
    | Parenthesized { inner = Some inner } -> (
        match expr_flat_width_with_budget next_budget inner with
        | Some inner_width -> Some (Int.add inner_width 2)
        | None -> None
      )
    | Parenthesized { inner = None } -> Some 2
    | Tuple ->
        let items = collect_tuple_expr_items expr in
        let length = Vector.length items in
        let rec loop index total =
          if Int.(index >= length) then
            Some (Int.add total 2)
          else
            match expr_flat_width_with_budget next_budget (Vector.get_unchecked items ~at:index) with
            | None -> None
            | Some item_width ->
                let total =
                  if Int.equal index 0 then
                    item_width
                  else
                    Int.(total + 2 + item_width)
                in
                loop (Int.add index 1) total
        in
        loop 0 0
    | Array -> array_expr_flat_width next_budget expr
    | List -> list_expr_flat_width next_budget expr
    | Record
    | RecordUpdate -> record_expr_flat_width next_budget expr
    | LocalOpen _ -> local_open_expr_flat_width next_budget expr
    | FirstClassModule -> node_token_flat_width expr
    | Assert { argument = Some argument } -> (
        match expr_flat_width_with_budget next_budget argument with
        | Some argument_width ->
            let argument_width =
              if expr_can_render_atom_without_parens argument then
                argument_width
              else
                Int.add argument_width 2
            in
            Some Int.(7 + argument_width)
        | None -> None
      )
    | Lazy { argument = Some argument } -> (
        match expr_flat_width_with_budget next_budget argument with
        | Some argument_width ->
            let argument_width =
              if expr_can_render_atom_without_parens argument then
                argument_width
              else
                Int.add argument_width 2
            in
            Some Int.(5 + argument_width)
        | None -> None
      )
    | Unreachable -> Some 1
    | PolyVariant { payload = None } -> (
        match first_ident_token expr with
        | Some tag -> Some Int.(1 + token_flat_width tag)
        | None -> None
      )
    | _ -> None

and array_expr_flat_width = fun budget expr ->
  let items = collect_array_items expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    Some 4
  else if not (array_expr_should_inline expr) then
    None
  else
    let rec loop index total =
      if Int.(index >= length) then
        Some Int.(total + 2)
      else
        match expr_flat_width_with_budget budget (Vector.get_unchecked items ~at:index) with
        | None -> None
        | Some item_width ->
            let separator_width =
              if Int.equal index 0 then
                0
              else
                2
            in
            loop (Int.add index 1) Int.(total + separator_width + item_width)
    in
    loop 0 2

and list_expr_flat_width = fun budget expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    Some 2
  else if not (list_expr_should_inline expr) then
    None
  else
    let rec loop index total =
      if Int.(index >= length) then
        let trailing_width =
          if Ast.Expr.list_has_trailing_separator expr then
            1
          else
            0
        in
        Some Int.(total + trailing_width + 2)
      else
        match expr_flat_width_with_budget budget (Vector.get_unchecked items ~at:index) with
        | None -> None
        | Some item_width ->
            let separator_width =
              if Int.equal index 0 then
                0
              else
                2
            in
            loop (Int.add index 1) Int.(total + separator_width + item_width)
    in
    loop 0 2

and record_expr_field_flat_width = fun budget (field: Ast.record_expr_field_view) ->
  match field with
  | Ast.RecordExprField { path; value = Some value; _ } -> (
      match expr_flat_width_with_budget budget value with
      | None -> None
      | Some value_width -> (
          match node_token_flat_width path with
          | Some path_width ->
              let value_width =
                if expr_can_render_atom_without_parens value then
                  value_width
                else
                  Int.add value_width 2
              in
              Some Int.(path_width + 3 + value_width)
          | None -> None
        )
    )
  | Ast.RecordExprField { path; value = None; _ } -> node_token_flat_width path
  | Ast.UnknownRecordExprField _ -> None

and record_expr_flat_width = fun budget expr ->
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  let base = Ast.RecordExpr.base expr in
  if Option.is_none base && Int.equal length 0 then
    Some 2
  else if Option.is_some base && Int.equal length 0 then
    None
  else
    let base_width =
      match base with
      | Some base -> (
          match expr_flat_width_with_budget budget base with
          | Some width ->
              let width =
                if expr_can_render_record_update_base_without_parens base then
                  width
                else
                  Int.add width 2
              in
              Some Int.(width + 6)
          | None -> None
        )
      | None -> Some 0
    in
    (
      match base_width with
      | None -> None
      | Some base_width ->
          let rec loop index total =
            if Int.(index >= length) then
              Some Int.(total + 4)
            else
              match record_expr_field_flat_width budget (Vector.get_unchecked fields ~at:index) with
              | None -> None
              | Some field_width ->
                  let separator_width =
                    if Int.equal index 0 then
                      0
                    else
                      2
                  in
                  loop (Int.add index 1) Int.(total + separator_width + field_width)
          in
          loop 0 base_width
    )

and local_open_expr_flat_width = fun budget expr ->
  let delimited_width dot_token opening_token body closing_token ~prefix_width =
    match expr_flat_width_with_budget budget body with
    | Some body_width ->
        let delimiter_width =
          if token_kind_is opening_token Kind.LPAREN then
            Int.add (token_flat_width opening_token) (token_flat_width closing_token)
          else
            0
        in
        let body_width =
          if token_kind_is opening_token Kind.LPAREN && expr_is_tuple body then
            Int.sub body_width 2
          else
            body_width
        in
        Some Int.(prefix_width + token_flat_width dot_token + delimiter_width + body_width)
    | None -> None
  in
  match Ast.LocalOpenExpr.view expr with
  | LetOpen _ -> None
  | Delimited {
    module_path;
    dot_token;
    opening_token;
    body;
    closing_token
  } -> (
      match (node_token_flat_width module_path, expr_flat_width_with_budget budget body) with
      | (Some module_width, Some _) ->
          delimited_width dot_token opening_token body closing_token ~prefix_width:module_width
      | _ -> None
    )
  | Unknown _ -> (
      let exprs = collect_child_exprs expr in
      if Int.equal (Vector.length exprs) 2 then
        let body = Vector.get_unchecked exprs ~at:1 in
        let opening_token =
          match first_child_token_matching expr ~matches:(fun kind ->
            Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)
          ) with
          | Some token -> Some token
          | None ->
              first_child_token_matching body ~matches:(fun kind ->
                Kind.(kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)
              )
        in
        let closing_token =
          match first_child_token_matching expr ~matches:(fun kind ->
            Kind.(kind = RPAREN || kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)
          ) with
          | Some token -> Some token
          | None ->
              first_child_token_matching body ~matches:(fun kind ->
                Kind.(kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)
              )
        in
        match (
          Ast.Node.first_child_token expr ~kind:Kind.DOT,
          opening_token,
          closing_token,
          expr_flat_width_with_budget budget (Vector.get_unchecked exprs ~at:0)
        ) with
        | (Some dot_token, Some opening_token, Some closing_token, Some prefix_width) ->
            delimited_width dot_token opening_token body closing_token ~prefix_width
        | _ -> None
      else
        None
    )

and expr_is_inline = fun expr -> expr_is_inline_with_budget expr_classification_budget expr

and expr_is_inline_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    unsupported_node "cyclic expression while classifying inline layout" expr;
  let next_budget = Int.sub budget 1 in
  match ExprView.view expr with
  | Path _
  | Literal _
  | FieldAccess _
  | Prefix _
  | LabeledArg _
  | OptionalArg _ -> true
  | FirstClassModule -> true
  | Attribute { inner = Some inner } ->
      if same_ast_node expr inner then
        false
      else
        expr_is_inline_with_budget next_budget inner
  | LocalOpen { body = Some body } ->
      if same_ast_node expr body then
        false
      else
        expr_is_inline_with_budget next_budget body
  | Infix { left = Some left; operator = Some operator; right = Some right } when String.equal
    (infix_operator_text_from_expr expr left operator right)
    "|>" -> false
  | Infix { left = Some left; right = Some right; _ } ->
      if same_ast_node expr left || same_ast_node expr right then
        false
      else if not (expr_is_inline_with_budget next_budget left) then
        false
      else
        expr_is_inline_with_budget next_budget right
  | Apply _ ->
      let (callee, args) = collect_apply expr in
      if same_ast_node expr callee then
        false
      else if not (expr_is_inline_with_budget next_budget callee) then
        false
      else
        let length = Vector.length args in
        let rec loop index =
          if Int.(index >= length) then
            true
          else
            let arg = Vector.get_unchecked args ~at:index in
            if same_ast_node expr arg then
              false
            else if expr_is_inline_with_budget next_budget arg then
              loop (Int.add index 1)
            else
              false
        in
        loop 0
  | Parenthesized { inner = Some inner } ->
      if same_ast_node expr inner then
        false
      else
        expr_is_inline_with_budget next_budget inner
  | Parenthesized { inner = None } -> true
  | Array -> array_expr_should_inline expr
  | List -> list_expr_should_inline expr
  | Record -> record_expr_should_inline expr
  | RecordUpdate -> record_expr_should_inline expr
  | Sequence { left = Some left; right = None } -> false
  | Typed { expr = Some inner; _ } -> expr_is_inline_with_budget next_budget inner
  | _ -> false

and array_expr_should_inline = fun expr ->
  let items = collect_array_items expr in
  let length = Vector.length items in
  let rec loop index =
    if Int.(index >= length) then
      true
    else if list_item_expr_should_inline (Vector.get_unchecked items ~at:index) then
      loop (Int.add index 1)
    else
      false
  in
  loop 0

and tuple_expr_should_inline = fun expr ->
  let items = collect_tuple_expr_items expr in
  let length = Vector.length items in
  if Int.(length > 4) then
    false
  else
    let rec loop index =
      if Int.(index >= length) then
        true
      else if expr_is_inline (Vector.get_unchecked items ~at:index) then
        loop (Int.add index 1)
      else
        false
    in
    loop 0

and list_item_expr_should_inline = fun expr ->
  if expr_is_tuple expr then
    tuple_expr_should_inline expr
  else
    expr_is_inline expr

and list_expr_should_inline = fun expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  let rec loop index =
    if Int.(index >= length) then
      true
    else if list_item_expr_should_inline (Vector.get_unchecked items ~at:index) then
      loop (Int.add index 1)
    else
      false
  in
  loop 0

and array_expr_decision = fun state expr ->
  let allow_inline = array_expr_should_inline expr in
  let flat_width = array_expr_flat_width expr_classification_budget expr in
  Layout.decide_separated Layout.Array (layout_context state) ~flat_width ~allow_inline

and list_expr_decision = fun state expr ->
  let allow_inline = list_expr_should_inline expr in
  let flat_width = list_expr_flat_width expr_classification_budget expr in
  Layout.decide_separated Layout.List (layout_context state) ~flat_width ~allow_inline

and record_expr_should_inline = fun expr ->
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  let base_inline =
    match Ast.RecordExpr.base expr with
    | Some base -> expr_is_inline base
    | None -> true
  in
  if not base_inline || Int.(length > 3) then
    false
  else
    let rec loop index =
      if Int.(index >= length) then
        true
      else
        let field = Vector.get_unchecked fields ~at:index in
        match field with
        | Ast.RecordExprField { value = None; _ } -> loop (Int.add index 1)
        | Ast.RecordExprField { value = Some value; _ } ->
            if expr_is_inline value then
              loop (Int.add index 1)
            else
              false
        | Ast.UnknownRecordExprField _ -> false
    in
    loop 0

and record_expr_decision = fun state ~force_multiline expr fields ->
  let inline_shape = (not force_multiline) && record_expr_should_inline expr in
  let flat_width = record_expr_flat_width expr_classification_budget expr in
  Layout.decide_record_expr
    (layout_context state)
    ~flat_width
    ~allow_inline:inline_shape
    ~item_count:(Vector.length fields)

let expr_can_follow_pipe_bare = fun expr ->
  match ExprView.view expr with
  | Apply _
  | Fun _
  | Function _ -> true
  | _ -> false

let expr_can_be_bare_infix_left_operand = fun expr ->
  match ExprView.view expr with
  | Apply _ -> true
  | _ -> false

let expr_can_be_bare_infix_right_operand = fun expr ->
  match ExprView.view expr with
  | Apply _
  | Fun _
  | Function _
  | If _
  | Let _
  | Match _
  | Try _ -> true
  | _ -> false

let expr_can_be_bare_pipe_left_operand = fun expr ->
  match ExprView.view expr with
  | Apply _ -> true
  | _ -> false

let expr_can_be_bare_caret_operand = fun expr ->
  match ExprView.view expr with
  | Apply _ -> true
  | _ -> false

let expr_is_infix_with_operator_text = fun expr expected ->
  match ExprView.view expr with
  | Infix { left = Some left; operator = Some operator; right = Some right } ->
      String.equal (infix_operator_text_from_expr expr left operator right) expected
  | _ -> false

let expr_is_pipeline = fun expr -> expr_is_infix_with_operator_text expr "|>"

type infix_associativity =
  | Infix_left
  | Infix_right

let infix_operator_precedence = fun operator ->
  let length = String.length operator in
  if String.equal operator "@@" then
    1
  else if String.equal operator "|>" then
    2
  else if String.equal operator "||" || String.equal operator "or" then
    3
  else if String.equal operator "&&" || String.equal operator "&" then
    4
  else if String.equal operator "::" then
    6
  else if String.equal operator "@" || String.equal operator "^" then
    7
  else if
    String.equal operator "mod"
    || String.equal operator "land"
    || String.equal operator "lor"
    || String.equal operator "lxor"
  then
    9
  else if Int.(length > 0) then
    (
      match String.get_unchecked operator ~at:0 with
      | '='
      | '<'
      | '>'
      | '|'
      | '&'
      | '$'
      | '!' -> 5
      | '+'
      | '-' -> 8
      | '*'
      | '/'
      | '%' -> 9
      | '@'
      | '^' -> 7
      | _ -> 8
    )
  else
    8

let infix_operator_associativity = fun operator ->
  if
    String.equal operator "::"
    || String.starts_with ~prefix:"@" operator
    || String.starts_with ~prefix:"^" operator
  then
    Infix_right
  else
    Infix_left

let expr_infix_operator_text = fun expr ->
  match ExprView.view expr with
  | Infix { left = Some left; operator = Some operator; right = Some right } ->
      Some (infix_operator_text_from_expr expr left operator right)
  | _ -> None

let expr_is_literal = fun expr ->
  match ExprView.view expr with
  | Literal _ -> true
  | _ -> false

let expr_is_single_line_literal = fun expr ->
  match ExprView.view expr with
  | Literal { token = Some token } -> not (Ast.Token.has_newline token)
  | Literal _ -> false
  | _ -> false

let prefix_operator_is_negative = fun operator ->
  token_text_is operator "-" || token_text_is operator "-."

let rec expr_is_nonliteral_negative_prefix = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_nonliteral_negative_prefix inner
  | Prefix { operator = Some operator; operand = Some operand } ->
      let operator_tokens = collect_prefix_operator_tokens expr in
      let last_operator =
        if Int.equal (Vector.length operator_tokens) 0 then
          operator
        else
          Vector.get_unchecked operator_tokens ~at:(Int.sub (Vector.length operator_tokens) 1)
      in
      prefix_operator_is_negative last_operator && not (expr_is_literal operand)
  | _ -> false

let render_prefix_operator_tokens = fun state operator_tokens fallback_operator ->
  if Int.equal (Vector.length operator_tokens) 0 then (
    emit_token state fallback_operator;
    fallback_operator
  ) else (
    emit_token_vector_stream state operator_tokens;
    Vector.get_unchecked operator_tokens ~at:(Int.sub (Vector.length operator_tokens) 1)
  )

let infix_left_operand_can_be_bare = fun ~parent_operator left ->
  match expr_infix_operator_text left with
  | None -> false
  | Some child_operator ->
      let parent_precedence = infix_operator_precedence parent_operator in
      let child_precedence = infix_operator_precedence child_operator in
      if Int.(child_precedence > parent_precedence) then
        true
      else if Int.equal child_precedence parent_precedence then
        match infix_operator_associativity parent_operator with
        | Infix_left -> true
        | Infix_right -> false
      else
        false

let infix_right_operand_can_be_bare = fun ~parent_operator right ->
  match expr_infix_operator_text right with
  | None -> false
  | Some child_operator ->
      let parent_precedence = infix_operator_precedence parent_operator in
      let child_precedence = infix_operator_precedence child_operator in
      if Int.(child_precedence > parent_precedence) then
        true
      else if Int.equal child_precedence parent_precedence then
        match infix_operator_associativity parent_operator with
        | Infix_left -> false
        | Infix_right -> true
      else
        false

let rec render_expr = fun ?(role = Layout.Top_expr) state expr ->
  emit_node_leading_trivia state expr;
  match ExprView.view expr with
  | Path { path } -> render_expr_path state path
  | Literal { token = Some token } -> emit_literal_token state token
  | Literal { token = None } -> unsupported_node "literal expression without token" expr
  | FieldAccess { target = Some target; field = Some field } ->
      render_expr_postfix_target state target;
      emit_text state ".";
      emit_token state field
  | FieldAccess _ -> unsupported_node "incomplete field access" expr
  | Assign { target = Some target; operator = Some operator; value = Some value } ->
      render_expr_atom state target;
      emit_space state;
      emit_token state operator;
      emit_space state;
      render_expr state value
  | Assign _ -> unsupported_node "incomplete assign expression" expr
  | Infix { left = Some left; operator = Some operator; right = Some right } ->
      render_infix_expr state expr left operator right
  | Infix _ -> unsupported_node "incomplete infix expression" expr
  | Prefix { operator = Some operator; operand = Some operand } ->
      let operator_tokens = collect_prefix_operator_tokens expr in
      let last_operator =
        if Int.equal (Vector.length operator_tokens) 0 then
          operator
        else
          Vector.get_unchecked operator_tokens ~at:(Int.sub (Vector.length operator_tokens) 1)
      in
      if prefix_operator_is_negative last_operator && expr_is_literal operand then
        emit_text state "(";
      let last_operator = render_prefix_operator_tokens state operator_tokens operator in
      if token_text_is last_operator "not" then
        emit_space state;
      render_prefix_operand state operand;
      if prefix_operator_is_negative last_operator && expr_is_literal operand then
        emit_text state ")"
  | Prefix _ -> unsupported_node "incomplete prefix expression" expr
  | Apply _ -> render_apply_expr ~role state expr
  | LabeledArg { label = Some label; value = Some value } ->
      emit_text state "~";
      emit_token state label;
      emit_text state ":";
      render_expr_atom ~role state value
  | LabeledArg { label = Some label; value = None } ->
      emit_text state "~";
      emit_token state label
  | LabeledArg { label = None; _ } -> unsupported_node "labeled argument without label" expr
  | OptionalArg { label = Some label; value = Some value } ->
      emit_text state "?";
      emit_token state label;
      emit_text state ":";
      render_expr_atom ~role state value
  | OptionalArg { label = Some label; value = None } ->
      emit_text state "?";
      emit_token state label
  | OptionalArg { label = None; _ } -> unsupported_node "optional argument without label" expr
  | Parenthesized { inner = Some inner } when expr_first_token_text_is expr "begin" ->
      let render_inner () = with_delimited_expr state (fun () -> render_expr state inner) in
      emit_text state "begin";
      emit_line state;
      with_indent state 2 render_inner;
      emit_line state;
      emit_text state "end"
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) && expr_is_tuple inner ->
      render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) && expr_is_typed inner ->
      render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
  && expr_has_leading_comment inner -> render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
  && expr_is_multiline inner -> render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      render_expr ~role state inner
  | Parenthesized { inner = Some inner } ->
      emit_text state "(";
      with_delimited_expr state (fun () -> render_expr ~role state inner);
      emit_text state ")"
  | Parenthesized { inner = None } -> emit_text state "()"
  | If { condition = Some condition; then_branch = Some then_branch; else_branch } ->
      render_if_expr state expr condition then_branch else_branch
  | If _ -> unsupported_node "incomplete if expression" expr
  | Let { body = Some body; _ } -> render_let_expr state expr body
  | Let _ -> unsupported_node "let expression without body" expr
  | Fun { body = Some body } -> render_fun_expr ~role state expr body
  | Fun { body = None } -> unsupported_node "fun expression without body" expr
  | Function { first_case = Some _ } -> render_function_expr state expr
  | Function { first_case = None } -> unsupported_node "function expression without cases" expr
  | Match { scrutinee = Some scrutinee; first_case = Some _ } ->
      render_match_expr state expr scrutinee
  | Match _ -> unsupported_node "match expression without scrutinee or cases" expr
  | Try { body = Some body; first_case = Some _ } -> render_try_expr state expr body
  | Try _ -> unsupported_node "try expression without body or cases" expr
  | LocalOpen { body = Some _ } -> render_local_open_expr state expr
  | LocalOpen { body = None } -> unsupported_node "local open expression without body" expr
  | List -> render_list_expr state expr
  | Array -> render_array_expr state expr
  | Record -> render_record_expr state ~inline:false expr
  | RecordUpdate -> render_record_expr state ~inline:false expr
  | Typed { expr = Some inner; annotation = Some annotation } ->
      (
        if type_expr_is_coercion_annotation annotation then
          render_expr state inner
        else if expr_can_render_atom_without_parens inner then
          render_expr state inner
        else
          render_expr_atom state inner
      );
      render_typed_expr_annotation state annotation
  | Typed _ -> unsupported_node "incomplete typed expression" expr
  | Tuple -> render_tuple_expr state expr
  | Sequence { left = Some left; right = Some right } ->
      if expr_is_tuple left then
        render_expr_atom state left
      else
        render_expr state left;
      (
        let tokens = collect_direct_tokens_between_children expr left right in
        match token_vector_find_kind tokens Kind.SEMI with
        | Some index ->
            let separator = Vector.get_unchecked tokens ~at:index in
            emit_text state ";";
            if Ast.Token.has_leading_comment separator then (
              emit_line state;
              emit_token_leading_comments_as_lines state separator
            ) else
              emit_line state
        | None ->
            emit_text state ";";
            emit_line state
      );
      if expr_is_tuple right then
        render_expr_atom state right
      else
        render_expr state right
  | Sequence { left = Some left; right = None } ->
      if expr_is_tuple left then
        render_expr_atom state left
      else
        render_expr state left;
      let tokens = collect_direct_tokens_after_child expr left in
      (
        match token_vector_find_kind tokens Kind.SEMI with
        | Some index ->
            let separator = Vector.get_unchecked tokens ~at:index in
            emit_text state ";";
            if Ast.Token.has_leading_comment separator then (
              emit_line state;
              emit_token_leading_comments_as_lines state separator
            )
        | None -> emit_text state ";"
      )
  | Sequence _ -> unsupported_node "incomplete sequence expression" expr
  | ArrayIndex { target = Some target; index = Some index } ->
      render_index_expr state expr target index ~fallback_open:".(" ~fallback_close:")"
  | ArrayIndex _ -> unsupported_node "incomplete array index expression" expr
  | StringIndex { target = Some target; index = Some index } ->
      render_index_expr state expr target index ~fallback_open:".[" ~fallback_close:"]"
  | StringIndex _ -> unsupported_node "incomplete string index expression" expr
  | PolyVariant { payload } -> render_poly_variant_expr state expr payload
  | BindingOperator -> render_binding_operator_expr state expr
  | LetModule { body = Some _ } -> render_let_module_expr state expr
  | LetModule { body = None } -> unsupported_node "let module expression without body" expr
  | FirstClassModule -> render_first_class_module_expr state expr
  | LetException { body = Some _ } -> render_let_exception_expr state expr
  | LetException { body = None } -> unsupported_node "let exception expression without body" expr
  | While { condition = Some condition; body = Some body } -> render_while_expr state condition body
  | While _ -> unsupported_node "incomplete while expression" expr
  | Assert { argument = Some argument } -> render_assert_expr state argument
  | Assert { argument = None } -> unsupported_node "assert expression without argument" expr
  | Lazy { argument = Some argument } -> render_lazy_expr state expr argument
  | Lazy { argument = None } -> unsupported_node "lazy expression without argument" expr
  | Attribute { inner = Some _ } -> render_attribute_expr state expr
  | Attribute { inner = None } ->
      unsupported_node "attribute expression without inner expression" expr
  | Extension -> render_extension_expr state expr
  | For {
    pattern = Some pattern;
    start_ = Some start_;
    stop = Some stop;
    body = Some body
  } -> render_for_expr state expr pattern start_ stop body
  | For _ -> unsupported_node "incomplete for expression" expr
  | Unreachable -> render_unreachable_expr state expr
  | Error _
  | Unknown _ -> unsupported_node "unsupported expression" expr

and tuple_expr_has_nonfinal_fun_item = fun expr ->
  let items = collect_tuple_expr_items expr in
  let length = Vector.length items in
  let rec loop index =
    if Int.(index >= Int.sub length 1) then
      false
    else if expr_is_fun_or_parenthesized_fun (Vector.get_unchecked items ~at:index) then
      true
    else
      loop (Int.add index 1)
  in
  loop 0

and tuple_expr_decision = fun state expr ->
  Layout.decide_tuple
    (layout_context state)
    ~flat_width:(expr_flat_width expr)
    ~has_nonfinal_fun_item:(tuple_expr_has_nonfinal_fun_item expr)

and tuple_expr_should_break = fun state expr ->
  not
    (layout_decision_is_inline (tuple_expr_decision state expr))

and render_tuple_expr_item = fun state ~length ~index item ->
  if
    expr_is_tuple item || (Int.(index < Int.sub length 1) && expr_is_fun_or_parenthesized_fun item)
  then
    render_expr_atom state item
  else
    render_expr state item

and render_tuple_expr = fun state expr ->
  let items = collect_tuple_expr_items expr in
  let length = Vector.length items in
  let render_item index item = render_tuple_expr_item state ~length ~index item in
  let render_inline () =
    emit_text state "(";
    render_tuple_expr_contents state items ~render_item;
    emit_text state ")"
  in
  if tuple_expr_should_break state expr then (
    emit_text state "(";
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let length = Vector.length items in
        let rec loop index =
          if Int.(index < length) then (
            render_item index (Vector.get_unchecked items ~at:index);
            if Int.(index < Int.sub length 1) then (
              emit_text state ",";
              emit_line state
            );
            loop (Int.add index 1)
          )
        in
        loop 0);
    emit_line state;
    emit_text state ")"
  ) else
    render_inline ()

and render_tuple_expr_contents = fun state items ~render_item ->
  let length = Vector.length items in
  let rec loop index =
    if Int.(index < length) then (
      render_item index (Vector.get_unchecked items ~at:index);
      if Int.(index < Int.sub length 1) then (
        emit_text state ",";
        emit_space state
      );
      loop (Int.add index 1)
    )
  in
  loop 0

and render_expr_atom = fun ?(role = Layout.Top_expr) state expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when expr_first_token_text_is expr "begin" ->
      render_expr state expr
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) && expr_is_fun inner ->
      emit_text state "(";
      render_expr ~role state inner;
      emit_text state ")"
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner)
  && expr_has_leading_comment inner -> render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      if expr_can_render_atom_without_parens inner then
        render_expr_atom ~role state inner
      else
        render_parenthesized_expr state inner
  | Tuple -> render_expr state expr
  | LocalOpen _ when local_open_expr_can_render_atom_without_parens expr -> render_expr state expr
  | Path _
  | Literal _
  | FieldAccess _
  | Prefix _
  | Parenthesized _
  | Array
  | List
  | Record
  | RecordUpdate
  | ArrayIndex _
  | StringIndex _
  | FirstClassModule
  | Extension
  | LabeledArg _
  | PolyVariant { payload = None }
  | OptionalArg _ -> render_expr ~role state expr
  | Fun _ ->
      emit_text state "(";
      render_expr ~role state expr;
      emit_text state ")"
  | _ -> render_parenthesized_expr state expr

and render_prefix_operand = fun state operand ->
  match ExprView.view operand with
  | Parenthesized { inner = Some inner } when not (same_ast_node operand inner)
  && expr_is_dotted_path inner -> render_parenthesized_expr state inner
  | _ when expr_is_prefix_deref operand -> render_parenthesized_expr state operand
  | _ -> render_expr_atom state operand

and render_expr_postfix_target = fun state target ->
  if expr_can_render_postfix_target_without_parens target then
    render_expr_atom state target
  else
    render_parenthesized_expr state target

and parenthesized_expr_decision = fun state expr ->
  let break_after_separator =
    match ExprView.view expr with
    | Typed { annotation = Some annotation; _ } ->
        type_expr_should_break_after_colon state ~suffix_width:1 annotation
    | _ -> false
  in
  Layout.decide_parenthesized_expr
    (layout_context state)
    ~has_leading_comment:(expr_has_leading_comment expr)
    ~is_multiline:(expr_is_multiline expr)
    ~break_after_separator

and parenthesized_expr_should_multiline = fun state expr ->
  match parenthesized_expr_decision state expr with
  | { Layout.mode = Inline; _ } -> false
  | _ -> true

and render_parenthesized_expr_with_multiline_indent = fun state expr ~body_indent ~closing_indent ->
  let expr = expr_unwrap_redundant_parentheses_for_delimited_render expr in
  if expr_is_tuple expr then
    render_expr state expr
  else
    (
      let render_inner () = with_delimited_expr state (fun () -> render_expr state expr) in
      if parenthesized_expr_should_multiline state expr then (
        emit_text state "(";
        emit_line state;
        with_indent state body_indent render_inner;
        emit_line state;
        if Int.(closing_indent > 0) then
          with_indent state closing_indent (fun () -> emit_text state ")")
        else
          emit_text state ")"
      ) else (
        emit_text state "(";
        render_inner ();
        emit_text state ")"
      )
    )

and render_parenthesized_expr = fun state expr ->
  render_parenthesized_expr_with_multiline_indent
    state
    expr
    ~body_indent:2
    ~closing_indent:0

and render_index_expr = fun state expr target index ~fallback_open ~fallback_close ->
  let opening_tokens = collect_direct_tokens_between_children expr target index in
  let closing_tokens = collect_direct_tokens_after_child expr index in
  render_expr_postfix_target state target;
  if Int.equal (Vector.length opening_tokens) 0 then
    emit_text state fallback_open
  else
    emit_token_vector_compact state opening_tokens;
  render_expr state index;
  if Int.equal (Vector.length closing_tokens) 0 then
    emit_text state fallback_close
  else
    emit_token_vector_compact state closing_tokens

and infix_expr_decision = fun state expr left operator right ->
  let operator_text = infix_operator_text_from_expr expr left operator right in
  let flat_width = expr_flat_width expr in
  let item_count =
    if infix_operator_breaks_long_chain operator_text then
      Int.add (same_operator_infix_count expr_classification_budget left operator_text) 1
    else
      0
  in
  Layout.decide_infix_chain
    (layout_context state)
    (infix_operator_class operator_text)
    ~flat_width
    ~item_count

and infix_expr_should_break = fun state expr left operator right ->
  match infix_expr_decision state expr left operator right with
  | { Layout.mode = Vertical; _ }
  | { Layout.mode = Block; _ } -> true
  | _ -> false

and collect_infix_chain = fun ~operator_text left operator right ->
  let parts = Vector.with_capacity ~size:8 in
  let first = ref left in
  let rec push_left expr =
    match ExprView.view expr with
    | Infix { left = Some left; operator = Some operator; right = Some right } when String.equal
      (token_text operator)
      operator_text ->
        push_left left;
        Vector.push parts ~value:(operator, right)
    | _ -> first := expr
  in
  push_left left;
  Vector.push parts ~value:(operator, right);
  (!first, parts)

and render_multiline_infix_expr = fun state left operator right ->
  let operator_text = token_text operator in
  let (first, parts) = collect_infix_chain ~operator_text left operator right in
  render_infix_left_operand state ~operator_text first;
  Vector.for_each
    parts
    ~fn:(fun (operator, operand) ->
      let operator_text = token_text operator in
      emit_line state;
      emit_token state operator;
      emit_space state;
      render_infix_right_operand state ~operator_text operand)

and render_infix_left_operand = fun state ~operator_text left ->
  if String.equal operator_text "@@" then
    render_expr state left
  else if expr_can_be_bare_infix_left_operand left then
    render_expr state left
  else if
    String.equal operator_text "|>"
    && (expr_can_be_bare_pipe_left_operand left
    || expr_is_infix_with_operator_text left operator_text)
  then
    render_expr state left
  else if
    String.equal operator_text "^"
    && (expr_can_be_bare_caret_operand left || expr_is_infix_with_operator_text left operator_text)
  then
    render_expr state left
  else if
    String.equal operator_text "::" && expr_is_infix_with_operator_text left operator_text
  then
    render_expr state left
  else if infix_left_operand_can_be_bare ~parent_operator:operator_text left then
    render_expr state left
  else
    render_expr_atom state left

and render_infix_right_operand = fun state ~operator_text right ->
  let render_right () =
    if
      String.equal operator_text "@@"
      || expr_can_be_bare_infix_right_operand right
      || (String.equal operator_text "|>" && expr_can_follow_pipe_bare right)
      || (String.equal operator_text "^" && expr_can_be_bare_caret_operand right)
      || infix_right_operand_can_be_bare ~parent_operator:operator_text right
    then
      render_expr state right
    else
      render_expr_atom state right
  in
  match Ast.Node.first_descendant_token right with
  | Some token -> (
      match token_single_inline_leading_delimited_trivia token with
      | Some (Leading_comment_trivia _) when emit_inline_leading_trivia_from_token
        state
        token
        ~leading_spaces:0
        ~trailing_space:true -> render_right ()
      | Some (Leading_comment_trivia _)
      | Some (Leading_docstring_trivia _)
      | None -> render_right ()
    )
  | None -> render_right ()

and render_infix_expr = fun state expr left operator right ->
  let operator_text = infix_operator_text_from_expr expr left operator right in
  if String.equal operator_text "|>" || infix_expr_should_break state expr left operator right then
    render_multiline_infix_expr state left operator right
  else (
    render_infix_left_operand state ~operator_text left;
    emit_space state;
    render_infix_operator_tokens state expr left operator right;
    emit_space state;
    render_infix_right_operand state ~operator_text right
  )

and render_delimited_local_open_body_expr = fun state expr ->
  match ExprView.view expr with
  | Infix { left = Some left; operator = Some operator; right = Some right } when expr_is_prefix_deref
    left ->
      let operator_text = infix_operator_text_from_expr expr left operator right in
      if
        String.equal operator_text "|>" || infix_expr_should_break state expr left operator right
      then
        render_multiline_infix_expr state left operator right
      else (
        render_parenthesized_expr state left;
        emit_space state;
        render_infix_operator_tokens state expr left operator right;
        emit_space state;
        render_infix_right_operand state ~operator_text right
      )
  | _ -> render_expr state expr

and render_let_body_with_leading_comment = fun state body ->
  emit_node_leading_comments_as_lines state body;
  match ExprView.view body with
  | Infix { left = Some left; operator = Some operator; right = Some right } when String.equal
    (token_text operator)
    "|>" -> render_multiline_infix_expr state left operator right
  | _ -> render_expr state body

and render_apply_flat = fun state callee args ->
  let (callee, base_args) = collect_apply callee in
  let render_arg arg =
    emit_space state;
    render_apply_argument state arg
  in
  render_expr_atom state callee;
  Vector.for_each base_args ~fn:render_arg;
  Vector.for_each args ~fn:render_arg

and render_infix_expr_with_right_apply = fun state expr left operator right args ->
  let operator_text = infix_operator_text_from_expr expr left operator right in
  render_infix_left_operand state ~operator_text left;
  emit_space state;
  render_infix_operator_tokens state expr left operator right;
  emit_space state;
  render_apply_flat state right args

and render_keyword_body_expr = fun state expr ->
  match ExprView.view expr with
  | Tuple -> render_expr_atom state expr
  | _ -> render_expr state expr

and if_condition_decision = fun state condition ->
  let flat_width =
    if expr_has_leading_comment condition || expr_is_multiline condition then
      None
    else
      expr_flat_width condition
  in
  Layout.decide_if_condition (layout_context state) ~flat_width ~suffix_width:6

and if_condition_should_break_after_keyword = fun state condition ->
  not
    (layout_decision_is_inline (if_condition_decision state condition))

and render_parenthesized_sequence_keyword_body = fun state body ->
  emit_text state "(";
  emit_line state;
  with_indent state 2 (fun () -> with_delimited_expr state (fun () -> render_expr state body));
  emit_line state;
  emit_text state ")"

and render_in_body_expr = fun state expr ->
  match ExprView.view expr with
  | Tuple -> render_expr state expr
  | _ -> render_expr state expr

and render_poly_variant_expr = fun state expr payload ->
  render_poly_variant_tag state expr;
  match payload with
  | Some payload ->
      emit_space state;
      render_expr_atom state payload
  | None -> ()

and render_poly_variant_tag = fun state expr ->
  match first_ident_token expr with
  | Some tag ->
      emit_text state "`";
      emit_token state tag
  | None -> unsupported_node "polymorphic variant expression without tag" expr

and render_apply_argument = fun ?(role = Layout.Top_expr) state arg ->
  let rec render_split_arg arg =
    match ExprView.view arg with
    | PolyVariant { payload = Some payload } ->
        render_poly_variant_tag state arg;
        emit_space state;
        render_split_arg payload
    | LabeledArg { label = Some label; value = Some value } -> (
        match ExprView.view value with
        | PolyVariant { payload = Some payload } ->
            emit_text state "~";
            emit_token state label;
            emit_text state ":";
            render_poly_variant_tag state value;
            emit_space state;
            render_split_arg payload
        | _ -> render_expr_atom ~role state arg
      )
    | OptionalArg { label = Some label; value = Some value } -> (
        match ExprView.view value with
        | PolyVariant { payload = Some payload } ->
            emit_text state "?";
            emit_token state label;
            emit_text state ":";
            render_poly_variant_tag state value;
            emit_space state;
            render_split_arg payload
        | _ -> render_expr_atom ~role state arg
      )
    | _ ->
        if expr_is_nonliteral_negative_prefix arg then
          render_parenthesized_expr state arg
        else
          render_expr_atom ~role state arg
  in
  render_split_arg arg

and expr_is_fun_or_parenthesized_fun = fun expr ->
  match ExprView.view expr with
  | Fun _ -> true
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_fun_or_parenthesized_fun inner
  | _ -> false

and fun_body_should_force_break_in_broken_arg = fun body ->
  if expr_has_leading_comment body || expr_is_multiline body || expr_is_pipeline body then
    true
  else
    match ExprView.view body with
    | Apply _ ->
        let (_, args) = collect_apply body in
        Int.(Vector.length args > 2) || apply_args_have_heavy_nested_apply args
    | _ -> false

and fun_expr_should_force_body_break_in_broken_arg = fun expr ->
  match ExprView.view expr with
  | Fun { body = Some body } -> fun_body_should_force_break_in_broken_arg body
  | Fun { body = None } -> true
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      fun_expr_should_force_body_break_in_broken_arg inner
  | _ -> false

and render_broken_apply_argument = fun state ~index arg ->
  let force =
    match ExprView.view arg with
    | Fun _
    | Parenthesized _ -> fun_expr_should_force_body_break_in_broken_arg arg
    | LabeledArg { value = Some value; _ }
    | OptionalArg { value = Some value; _ } -> fun_expr_should_force_body_break_in_broken_arg value
    | _ -> false
  in
  match ExprView.view arg with
  | Record
  | RecordUpdate -> render_record_expr ~force_multiline:true state ~inline:false arg
  | _ ->
      if force then
        render_apply_argument ~role:(Layout.Apply_arg { index; broken_parent = true }) state arg
      else
        render_apply_argument state arg

and expr_is_constructor_like_callee = fun expr ->
  match ExprView.view expr with
  | Path _ -> (
      match last_ident_token expr with
      | Some token -> token_text_starts_uppercase token
      | None -> false
    )
  | PolyVariant _ -> true
  | _ -> false

and expr_is_single_constructor_apply = fun expr ->
  match ExprView.view expr with
  | Apply _ ->
      let (callee, args) = collect_apply expr in
      Int.equal (Vector.length args) 1 && expr_is_constructor_like_callee callee
  | _ -> false

and expr_is_single_constructor_multiline_apply = fun expr ->
  match ExprView.view expr with
  | Apply _ ->
      let (_, args) = collect_apply expr in
      expr_is_single_constructor_apply expr && vector_exists_expr_is_multiline_except expr args
  | _ -> false

and expr_is_heavy_nested_apply_value = fun expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      expr_is_heavy_nested_apply_value inner
  | Apply _ ->
      let (callee, args) = collect_apply expr in
      not (same_ast_node expr callee) && Int.(Vector.length args > 1)
  | _ -> false

and apply_argument_has_heavy_nested_apply = fun arg ->
  match ExprView.view arg with
  | LabeledArg { value = Some value; _ }
  | OptionalArg { value = Some value; _ } -> expr_is_heavy_nested_apply_value value
  | Parenthesized { inner = Some inner } when not (same_ast_node arg inner) ->
      expr_is_heavy_nested_apply_value inner
  | _ -> false

and apply_args_have_heavy_nested_apply = fun args ->
  let length = Vector.length args in
  if Int.(length <= 2) then
    false
  else
    let rec loop index =
      if Int.(index >= length) then
        false
      else if apply_argument_has_heavy_nested_apply (Vector.get_unchecked args ~at:index) then
        true
      else
        loop (Int.add index 1)
    in
    loop 0

and application_flat_width = fun expr ->
  match expr_flat_width expr with
  | Some _ as width -> width
  | None -> node_token_flat_width expr

and application_callee_class = fun callee ->
  if expr_is_constructor_like_callee callee then
    Layout.Constructor_like
  else
    Layout.Ordinary

and application_layout_decision_from_column = fun
  ?(suffix_width = 0)
  ~force_parent_break
  state
  expr
  callee
  args
  ~column ->
  Layout.decide_application
    (layout_context ~column state)
    ~flat_width:(application_flat_width expr)
    ~suffix_width
    ~arg_count:(Vector.length args)
    ~callee_class:(application_callee_class callee)
    ~force_parent_break
    ~has_multiline_args:(vector_exists_expr_is_multiline_except expr args)
    ~has_heavy_nested_apply:(apply_args_have_heavy_nested_apply args)

and apply_expr_decision_from_column = fun ?(suffix_width = 0) state expr args ~column ->
  let (callee, _) = collect_apply expr in
  application_layout_decision_from_column
    ~force_parent_break:false
    ~suffix_width
    state
    expr
    callee
    args
    ~column

and apply_expr_exceeds_width_from_column = fun ?(suffix_width = 0) state expr args ~column ->
  if Int.equal (Vector.length args) 0 then
    false
  else
    match application_flat_width expr with
    | None -> false
    | Some _ as flat_width ->
        not (Layout.fits_flat
          (layout_context ~column state)
          ~suffix_width
          flat_width)

and apply_expr_should_break_from_column = fun ?(suffix_width = 0) state expr args ~column ->
  if Int.equal (Vector.length args) 0 then
    false
  else
    match apply_expr_decision_from_column ~suffix_width state expr args ~column with
    | { Layout.mode = Hang _; _ }
    | { Layout.mode = Vertical; _ }
    | { Layout.mode = Block; _ } -> true
    | _ -> false

and apply_expr_should_break = fun state expr args ->
  apply_expr_should_break_from_column
    state
    expr
    args
    ~column:state.column

and render_apply_expr = fun ?(role = Layout.Top_expr) state expr ->
  let force_current_apply_break =
    match role with
    | Layout.Function_body { force_apply_break = true } -> true
    | _ -> false
  in
  let (callee, args) = collect_apply expr in
  let arg_count = Vector.length args in
  if same_ast_node expr callee then
    unsupported_node "apply expression resolved to itself" expr;
  let decision =
    application_layout_decision_from_column
      ~force_parent_break:force_current_apply_break
      state
      expr
      callee
      args
      ~column:state.column
  in
  let break_all_args =
    match decision.Layout.mode with
    | Layout.Hang _
    | Layout.Vertical
    | Layout.Block -> true
    | _ -> false
  in
  let break_before_multiline_args =
    match decision.Layout.mode with
    | Layout.Isolate_child_blocks -> true
    | _ -> false
  in
  match ExprView.view callee with
  | Infix { left = Some left; operator = Some operator; right = Some right } ->
      render_infix_expr_with_right_apply state callee left operator right args
  | _ ->
      render_expr_atom state callee;
      let rec loop index breaking =
        if Int.(index < arg_count) then
          (
            let arg = Vector.get_unchecked args ~at:index in
            let should_break =
              break_all_args
              || expr_has_leading_comment arg
              || (break_before_multiline_args && (breaking || expr_is_multiline arg))
            in
            if should_break then (
              emit_line state;
              with_indent state 2 (fun () -> render_broken_apply_argument state ~index arg);
              loop (Int.add index 1) true
            ) else (
              emit_space state;
              render_apply_argument state arg;
              loop (Int.add index 1) breaking
            )
          )
      in
      loop 0 false

and render_if_expr = fun
  ?(else_if_continuation = false)
  state
  expr
  condition
  then_branch
  else_branch ->
  emit_node_keyword state expr ~kind:Kind.IF_KW ~fallback:"if";
  if if_condition_should_break_after_keyword state condition then (
    emit_line state;
    with_indent state 2 (fun () -> render_expr state condition);
    emit_line state
  ) else (
    emit_space state;
    render_expr state condition;
    emit_space state
  );
  let then_sequence_inner =
    match ExprView.view then_branch with
    | Parenthesized { inner = Some inner } when not (same_ast_node then_branch inner)
    && expr_is_sequence inner -> Some inner
    | _ -> None
  in
  emit_node_keyword state expr ~kind:Kind.THEN_KW ~fallback:"then";
  (
    match then_sequence_inner with
    | Some inner ->
        emit_space state;
        render_parenthesized_sequence_keyword_body state inner
    | None ->
        emit_line state;
        with_indent state 2 (fun () -> render_keyword_body_expr state then_branch)
  );
  (
    match else_branch with
    | None -> ()
    | Some branch ->
        let else_token = Ast.Node.first_child_token expr ~kind:Kind.ELSE_KW in
        let else_has_leading_comment =
          match else_token with
          | Some token -> Ast.Token.has_leading_comment token
          | None -> false
        in
        if Option.is_some then_sequence_inner && not else_has_leading_comment then
          emit_space state
        else
          emit_line state;
        (
          match else_token with
          | Some token when Ast.Token.has_leading_comment token ->
              if else_if_continuation then
                emit_token_leading_comments_as_lines state token
              else
                with_indent state 2 (fun () -> emit_token_leading_comments_as_lines state token)
          | Some _
          | None -> ()
        );
        emit_token_or_keyword state else_token ~fallback:"else";
        (
          match ExprView.view branch with
          | If { condition = Some condition; then_branch = Some then_branch; else_branch } when not
            (expr_has_leading_comment branch) ->
              emit_space state;
              render_if_expr
                ~else_if_continuation:true
                state
                branch
                condition
                then_branch
                else_branch
          | Parenthesized { inner = Some inner } when not (same_ast_node branch inner)
          && expr_is_sequence inner ->
              emit_space state;
              render_parenthesized_sequence_keyword_body state inner
          | _ ->
              emit_line state;
              with_indent state 2 (fun () -> render_keyword_body_expr state branch)
        )
  )

and fun_params_exceed_width = fun state params ->
  let length = Vector.length params in
  let rec loop index total =
    if Int.(index >= length) then
      Int.(state.column + total + 3 > state.width)
    else
      match node_token_flat_width (Vector.get_unchecked params ~at:index) with
      | Some width -> loop (Int.add index 1) Int.(total + 1 + width)
      | None -> true
  in
  loop 0 3

and render_fun_expr = fun ?(role = Layout.Top_expr) state expr body ->
  let params = collect_fun_parameters expr in
  let param_count = Vector.length params in
  let break_params = Int.(param_count > 0) && fun_params_exceed_width state params in
  let role_forces_body_break =
    match role with
    | Layout.Apply_arg { broken_parent = true; _ } -> true
    | _ -> false
  in
  let render_body ?(force_apply_break = false) () =
    let body_role =
      if force_apply_break then
        Layout.Function_body { force_apply_break = true }
      else
        Layout.Top_expr
    in
    let render () =
      match ExprView.view body with
      | Tuple -> render_expr_atom ~role:body_role state body
      | _ -> render_expr ~role:body_role state body
    in
    render ()
  in
  emit_text state "fun";
  if break_params then (
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let rec loop index =
          if Int.(index < param_count) then (
            render_pattern state (Vector.get_unchecked params ~at:index);
            if Int.(index = Int.sub param_count 1) then (
              emit_space state;
              emit_text state "->"
            ) else
              emit_line state;
            loop (Int.add index 1)
          )
        in
        loop 0)
  ) else (
    Vector.for_each
      params
      ~fn:(fun param ->
        emit_space state;
        render_pattern state param);
    emit_space state;
    emit_text state "->"
  );
  let body_exceeds_width =
    match ExprView.view body with
    | Array
    | List
    | Record
    | RecordUpdate
    | Tuple -> false
    | _ -> (
        match expr_flat_width body with
        | Some width -> Int.(state.column + 1 + width > state.width)
        | None -> false
      )
  in
  if
    break_params
    || role_forces_body_break
    || expr_has_leading_comment body
    || expr_is_multiline body
    || body_exceeds_width
  then (
    emit_line state;
    with_indent
      state
      2
      (fun () -> render_body ~force_apply_break:(body_exceeds_width && expr_is_apply body) ())
  ) else (
    emit_space state;
    render_body ()
  )

and render_function_expr = fun state expr ->
  let inline = not state.line_start in
  emit_text state "function";
  emit_line state;
  let cases = collect_match_cases expr in
  let force_body_break = match_cases_have_parenthesized_multiline_body cases in
  if inline then
    with_indent state 2 (fun () -> render_match_cases_with_body_break state cases force_body_break)
  else
    render_match_cases_with_body_break state cases force_body_break

and render_match_expr = fun state expr scrutinee ->
  emit_text state "match";
  emit_space state;
  render_expr state scrutinee;
  emit_space state;
  emit_text state "with";
  emit_line state;
  let cases = collect_match_cases expr in
  render_match_cases state cases

and render_try_expr = fun state expr body ->
  emit_text state "try";
  if expr_is_multiline body then (
    emit_line state;
    with_indent state 2 (fun () -> render_expr state body);
    emit_line state
  ) else (
    emit_space state;
    render_expr state body;
    emit_space state
  );
  emit_text state "with";
  emit_line state;
  let cases = collect_match_cases expr in
  render_match_cases state cases

and render_assert_expr = fun state argument ->
  emit_text state "assert";
  emit_space state;
  render_expr_atom state argument

and render_lazy_expr = fun state expr argument ->
  emit_node_keyword state expr ~kind:Kind.LAZY_KW ~fallback:"lazy";
  emit_space state;
  render_expr_atom state argument

and render_unreachable_expr = fun state expr ->
  emit_node_keyword
    state
    expr
    ~kind:Kind.DOT
    ~fallback:"."

and render_attribute_expr = fun state expr ->
  match Ast.cast_result_to_option (Ast.AttributeExpr.cast expr) with
  | Some attribute -> (
      match Ast.AttributeExpr.inner attribute with
      | Some inner ->
          render_expr state inner;
          emit_space state;
          emit_shell_token_stream
            state
            (fun ~fn -> Ast.AttributeExpr.for_each_shell_token attribute ~fn)
      | None -> unsupported_node "attribute expression without inner expression" expr
    )
  | None -> unsupported_node "unsupported attribute expression" expr

and render_extension_expr = fun state expr ->
  match Ast.cast_result_to_option (Ast.ExtensionExpr.cast expr) with
  | Some extension ->
      emit_shell_token_stream
        state
        (fun ~fn -> Ast.ExtensionExpr.for_each_shell_token extension ~fn)
  | None -> unsupported_node "unsupported extension expression" expr

and render_let_exception_expr = fun state expr ->
  match Ast.cast_result_to_option (Ast.LetExceptionExpr.cast expr) with
  | None -> unsupported_node "unsupported let exception expression" expr
  | Some let_exception -> (
      (
        match Ast.LetExceptionExpr.let_token let_exception with
        | Some token -> emit_token state token
        | None -> emit_text state "let"
      );
      emit_space state;
      (
        match Ast.LetExceptionExpr.exception_token let_exception with
        | Some token -> emit_token state token
        | None -> emit_text state "exception"
      );
      emit_space state;
      (
        match Ast.LetExceptionExpr.name let_exception with
        | Some name -> emit_token state name
        | None -> unsupported_node "let exception without name" expr
      );
      (
        match Ast.LetExceptionExpr.of_token let_exception with
        | Some of_token ->
            emit_space state;
            emit_token state of_token;
            emit_space state;
            let payload_tokens = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
            Ast.LetExceptionExpr.for_each_payload_token
              let_exception
              ~fn:(fun token -> Vector.push payload_tokens ~value:token);
            emit_token_vector_stream state payload_tokens
        | None -> ()
      );
      emit_space state;
      (
        match Ast.LetExceptionExpr.in_token let_exception with
        | Some token -> emit_token state token
        | None -> emit_text state "in"
      );
      emit_line state;
      match Ast.LetExceptionExpr.body let_exception with
      | Some body -> render_expr state body
      | None -> unsupported_node "let exception expression without body" expr
    )

and render_loop_body = fun state body ->
  emit_line state;
  with_indent state 2 (fun () -> render_expr state body);
  emit_line state

and render_while_expr = fun state condition body ->
  emit_text state "while";
  emit_space state;
  render_expr state condition;
  emit_space state;
  emit_text state "do";
  render_loop_body state body;
  emit_text state "done"

and render_for_expr = fun state expr pattern start_ stop body ->
  emit_text state "for";
  emit_space state;
  render_pattern state pattern;
  emit_space state;
  emit_text state "=";
  emit_space state;
  render_expr state start_;
  emit_space state;
  (
    match Ast.Node.first_child_token expr ~kind:Kind.DOWNTO_KW with
    | Some token -> emit_token state token
    | None -> emit_text state "to"
  );
  emit_space state;
  render_expr state stop;
  emit_space state;
  emit_text state "do";
  render_loop_body state body;
  emit_text state "done"

and render_match_cases = fun state cases -> render_match_cases_with_body_break state cases false

and render_match_cases_with_body_break = fun state cases force_body_break ->
  let length = Vector.length cases in
  let rec loop index =
    if Int.(index < length) then (
      render_match_case_with_body_break
        state
        (Vector.get_unchecked cases ~at:index)
        force_body_break;
      if Int.(index < Int.sub length 1) then
        emit_line state;
      loop (Int.add index 1)
    )
  in
  loop 0

and match_cases_have_multiline_body = fun cases ->
  let length = Vector.length cases in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      match Ast.MatchCase.body (Vector.get_unchecked cases ~at:index) with
      | Some body when expr_is_multiline body -> true
      | Some _
      | None -> loop (Int.add index 1)
  in
  loop 0

and match_cases_have_parenthesized_multiline_body = fun cases ->
  let length = Vector.length cases in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      match Ast.MatchCase.body (Vector.get_unchecked cases ~at:index) with
      | Some body when expr_is_parenthesized_multiline body -> true
      | Some _
      | None -> loop (Int.add index 1)
  in
  loop 0

and match_case_body_exceeds_width = fun state body ->
  let body_width =
    match expr_flat_width body with
    | Some width -> Some width
    | None -> node_token_flat_width body
  in
  match body_width with
  | Some width -> Int.(state.column + 1 + width > state.width)
  | None -> false

and render_parenthesized_match_case_body = fun state expr ->
  match ExprView.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) ->
      render_parenthesized_expr_with_multiline_indent state inner ~body_indent:4 ~closing_indent:2
  | _ -> render_expr state expr

and render_match_case = fun state case -> render_match_case_with_body_break state case false

and render_match_case_with_body_break = fun state case force_body_break ->
  match Ast.MatchCase.view case with
  | Ast.MatchCase.Unknown _ -> unsupported_node "match case without pattern or body" case
  | Ast.MatchCase.Case { pattern; guard; body } ->
      emit_text state "|";
      emit_space state;
      render_pattern state pattern;
      (
        match guard with
        | Some guard ->
            emit_space state;
            emit_text state "when";
            emit_space state;
            render_expr state guard
        | None -> ()
      );
      emit_space state;
      emit_text state "->";
      (
    match body with
    | body when expr_is_parenthesized_apply body && force_body_break ->
        emit_line state;
        with_indent state 4 (fun () -> render_expr_atom state body)
    | body when expr_is_parenthesized_apply body ->
        emit_space state;
        render_expr_atom state body
    | body when expr_is_parenthesized_multiline body ->
        emit_space state;
        render_parenthesized_match_case_body state body
    | body when force_body_break && expr_is_unit body ->
        emit_space state;
        render_expr state body
    | body when force_body_break ->
        emit_line state;
        with_indent state 4 (fun () -> render_expr state body)
    | body when expr_is_tuple body
    && Int.(state.delimited_expr_depth > 0)
    && expr_has_leading_comment body ->
        emit_line state;
        with_indent
          state
          4
          (fun () ->
            emit_node_leading_comments_as_lines state body;
            render_expr_atom state body)
    | body when expr_is_tuple body && Int.(state.delimited_expr_depth > 0) ->
        emit_space state;
        render_expr_atom state body
    | body when expr_is_tuple body && expr_has_leading_comment body ->
        emit_line state;
        with_indent
          state
          4
          (fun () ->
            emit_node_leading_comments_as_lines state body;
            render_expr_atom state body)
    | body when expr_is_tuple body ->
        emit_space state;
        render_expr_atom state body
    | body when expr_has_leading_comment body ->
        emit_line state;
        with_indent
          state
          4
          (fun () ->
            emit_node_leading_comments_as_lines state body;
            render_expr state body)
    | body when expr_is_multiline body ->
        emit_line state;
        with_indent state 4 (fun () -> render_expr state body)
    | body when (not (expr_is_single_line_literal body))
    && match_case_body_exceeds_width state body ->
        emit_line state;
        with_indent state 4 (fun () -> render_expr state body)
    | body ->
        emit_space state;
        render_expr state body
      )

and local_open_body_should_inline = fun state body ->
  let is_nested_let_open =
    match ExprView.view body with
    | LocalOpen _ -> (
        match Ast.LocalOpenExpr.view body with
        | LetOpen _ -> true
        | Delimited _
        | Unknown _ -> false
      )
    | _ -> false
  in
  if is_nested_let_open || expr_has_leading_comment body || expr_is_multiline body then
    false
  else
    match expr_flat_width body with
    | Some width -> Int.(state.column + 1 + width <= state.width)
    | None -> false

and local_open_body_keeps_first_infix_operand_after_in = fun state body ->
  if expr_has_leading_comment body then
    false
  else
    match ExprView.view body with
    | Infix { left = Some left; operator = Some operator; right = Some right } ->
        let operator_text = infix_operator_text_from_expr body left operator right in
        String.equal operator_text "^" && infix_expr_should_break state body left operator right
    | _ -> false

and render_local_open_expr = fun state expr ->
  let render_delimited_body opening body closing =
    if token_kind_is opening Kind.LPAREN then (
      emit_token state opening;
      let spaced =
        match ExprView.view body with
        | Path { path } ->
            let tokens = collect_child_tokens path in
            token_vector_is_operator_name tokens
        | _ -> false
      in
      if spaced then
        emit_space state;
      (
        if expr_is_tuple body then
          (
            let items = collect_tuple_expr_items body in
            let length = Vector.length items in
            let render_item index item = render_tuple_expr_item state ~length ~index item in
            render_tuple_expr_contents state items ~render_item
          )
        else
          render_delimited_local_open_body_expr state body
      );
      if spaced then
        emit_space state;
      emit_token state closing
    ) else
      render_expr state body
  in
  match Ast.LocalOpenExpr.view expr with
  | LetOpen { module_path; body; _ } ->
      emit_text state "let";
      emit_space state;
      emit_text state "open";
      emit_space state;
      render_path state module_path;
      emit_space state;
      emit_text state "in";
      if
        local_open_body_should_inline state body
        || local_open_body_keeps_first_infix_operand_after_in state body
      then (
        emit_space state;
        render_expr state body
      ) else (
        emit_line state;
        render_expr state body
      )
  | Delimited {
    module_path;
    dot_token;
    opening_token = opening;
    body;
    closing_token = closing
  } ->
      render_path state module_path;
      emit_token state dot_token;
      if token_kind_is opening Kind.LBRACKET then
        with_suppressed_list_item_leading_comments
          state
          (fun () ->
            render_delimited_body opening body closing)
      else
        render_delimited_body opening body closing
  | Unknown node -> (
      let exprs = collect_child_exprs expr in
      if Int.equal (Vector.length exprs) 2 then
        let body = Vector.get_unchecked exprs ~at:1 in
        let opening =
          match first_child_token_matching expr ~matches:(fun kind ->
            Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)
          ) with
          | Some token -> Some token
          | None ->
              first_child_token_matching body ~matches:(fun kind ->
                Kind.(kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)
              )
        in
        let closing =
          match first_child_token_matching expr ~matches:(fun kind ->
            Kind.(kind = RPAREN || kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)
          ) with
          | Some token -> Some token
          | None ->
              first_child_token_matching body ~matches:(fun kind ->
                Kind.(kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)
              )
        in
        match (Ast.Node.first_child_token expr ~kind:Kind.DOT, opening, closing) with
        | (Some dot_token, Some opening, Some closing) ->
            render_expr state (Vector.get_unchecked exprs ~at:0);
            emit_token state dot_token;
            if token_kind_is opening Kind.LBRACKET then
              with_suppressed_list_item_leading_comments
                state
                (fun () ->
                  render_delimited_body opening body closing)
            else
              render_delimited_body opening body closing
        | _ -> unsupported_node "unsupported local-open expression" node
      else
        unsupported_node "unsupported local-open expression" node
    )

and token_vector_range_is_path = fun tokens ~start ~stop ->
  let rec loop index saw_ident expect_ident =
    if Int.(index >= stop) then
      saw_ident && not expect_ident
    else
      let token = Vector.get_unchecked tokens ~at:index in
      if token_kind_is token Kind.IDENT && expect_ident then
        loop
          (Int.add index 1)
          true
          false
      else if token_kind_is token Kind.DOT && saw_ident && not expect_ident then
        loop
          (Int.add index 1)
          saw_ident
          true
      else
        false
  in
  if Int.(start < stop) then
    loop start false true
  else
    false

and render_token_path_range = fun state node tokens ~start ~stop message ->
  if not (token_vector_range_is_path tokens ~start ~stop) then
    unsupported_node message node
  else
    let rec loop index =
      if Int.(index < stop) then (
        emit_token state (Vector.get_unchecked tokens ~at:index);
        loop (Int.add index 1)
      )
    in
    loop start

and render_parenthesized_val_module_body = fun state node ->
  let tokens = collect_child_tokens node in
  match (
    token_vector_find_kind tokens Kind.LPAREN,
    token_vector_find_kind tokens Kind.VAL_KW,
    token_vector_find_kind tokens Kind.COLON,
    token_vector_find_kind tokens Kind.RPAREN
  ) with
  | (Some opening_index, Some val_index, None, Some closing_index) ->
      emit_token state (Vector.get_unchecked tokens ~at:opening_index);
      emit_token state (Vector.get_unchecked tokens ~at:val_index);
      emit_space state;
      render_token_path_range
        state
        node
        tokens
        ~start:(Int.add val_index 1)
        ~stop:closing_index
        "first-class module unpack without value path";
      emit_token state (Vector.get_unchecked tokens ~at:closing_index)
  | (Some opening_index, Some val_index, Some colon_index, Some closing_index) ->
      emit_token state (Vector.get_unchecked tokens ~at:opening_index);
      emit_token state (Vector.get_unchecked tokens ~at:val_index);
      emit_space state;
      render_token_path_range
        state
        node
        tokens
        ~start:(Int.add val_index 1)
        ~stop:colon_index
        "first-class module unpack without value path";
      emit_space state;
      emit_token state (Vector.get_unchecked tokens ~at:colon_index);
      emit_space state;
      emit_token_vector_range_stream
        state
        tokens
        ~start:(Int.add colon_index 1)
        ~stop:closing_index;
      emit_token state (Vector.get_unchecked tokens ~at:closing_index)
  | _ -> unsupported_node "incomplete parenthesized module unpack" node

and module_expr_specific_node = fun node ->
  match node_kind node with
  | Kind.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
  | kind when is_module_expr_kind kind -> Some node
  | _ -> None

and module_type_specific_node = fun node ->
  match node_kind node with
  | Kind.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
  | kind when is_module_type_kind kind -> Some node
  | _ -> None

and collect_child_module_exprs = fun node ->
  let exprs = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match module_expr_specific_node child with
      | Some expr -> Vector.push exprs ~value:expr
      | None -> ());
  exprs

and render_signature_body = fun state items ~end_token ->
  let has_terminal_trivia = token_has_leading_comment end_token in
  let terminal_inline_trivia =
    match end_token with
    | Some token -> token_inline_leading_comment_text token
    | None -> None
  in
  if Int.equal (Vector.length items) 0 then (
    emit_text state "sig";
    if has_terminal_trivia then (
      emit_line state;
      (
        match end_token with
        | Some token ->
            with_indent state 2 (fun () -> emit_token_leading_comments_as_lines state token)
        | None -> ()
      )
    ) else
      emit_space state;
    emit_text state "end"
  ) else (
    emit_text state "sig";
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        render_signature_items state items;
        match (end_token, terminal_inline_trivia) with
        | (Some token, Some trivia) ->
            emit_space state;
            emit_text state trivia;
            state.suppress_leading_token <- Some token.Ast.id
        | (Some token, None) when has_terminal_trivia ->
            emit_line state;
            emit_line state;
            emit_token_leading_comments_as_lines state token
        | (Some _, None)
        | (None, _) -> ());
    if (not has_terminal_trivia) || Option.is_some terminal_inline_trivia then
      emit_line state;
    emit_text state "end"
  )

and render_signature_module_type_node = fun state node ->
  let items = collect_signature_items_from_node node in
  render_signature_body
    state
    items
    ~end_token:(Ast.Node.first_child_token node ~kind:Kind.END_KW)

and render_module_typeof_node = fun state node ->
  emit_text state "module type of";
  emit_space state;
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_token
    node
    ~fn:(fun token ->
      if token_kind_is token Kind.IDENT || token_kind_is token Kind.DOT then
        Vector.push tokens ~value:token);
  if Int.equal (Vector.length tokens) 0 then
    unsupported_node "module type-of without path" node
  else
    emit_token_vector_stream state tokens

and render_module_type_constraint_node = fun state node ->
  match Ast.cast_result_to_option (Ast.ModuleTypeConstraint.cast node) with
  | None -> unsupported_node "unsupported module type constraint" node
  | Some constraint_ -> (
      match Ast.ModuleTypeConstraint.view constraint_ with
      | Type { path; operator; body } ->
          emit_text state "type";
          emit_space state;
          render_path state path;
          emit_space state;
          emit_token state operator;
          emit_space state;
          render_type_expr state body
      | Module { path; operator; body } ->
          emit_text state "module";
          emit_space state;
          render_path state path;
          emit_space state;
          emit_token state operator;
          emit_space state;
          render_module_type_constraint_module_body state body
      | Unknown _ -> unsupported_node "unsupported module type constraint" node
    )

and render_module_type_constraint_module_body = fun state body ->
  match module_expr_specific_node body with
  | Some module_expr -> render_module_expr_node state module_expr
  | None -> (
      match module_type_specific_node body with
      | Some module_type -> render_module_type_node state module_type
      | None -> unsupported_node "module constraint without module body" body
    )

and render_with_module_type_node = fun state node ->
  let rendered_base = ref false in
  let pending_connector = ref None in
  Ast.Node.for_each_child
    node
    ~fn:(fun child ->
      match child with
      | Syn.SyntaxTree.Token id ->
          let token: Ast.Token.t = { tree = node.Ast.tree; id } in
          if token_kind_is token Kind.WITH_KW || token_kind_is token Kind.AND_KW then
            pending_connector := Some token
      | Syn.SyntaxTree.Node id ->
          let child: Ast.Node.t = { tree = node.Ast.tree; id } in
          if is_module_type_kind (node_kind child) && not !rendered_base then (
            render_module_type_node state child;
            rendered_base := true
          ) else
            (
              match Ast.cast_result_to_option (Ast.ModuleTypeConstraint.cast child) with
              | Some _ ->
                  emit_space state;
                  (
                    match !pending_connector with
                    | Some connector ->
                        emit_token state connector;
                        pending_connector := None
                    | None -> emit_text state "with"
                  );
                  emit_space state;
                  render_module_type_constraint_node state child
              | None -> ()
            )
      | Syn.SyntaxTree.Missing _ -> ());
  if not !rendered_base then
    unsupported_node "with module type without base" node

and render_module_type_node = fun state node ->
  let original = node in
  match module_type_specific_node original with
  | None -> unsupported_node "unsupported module type" node
  | Some node ->
      (
        match node_kind node with
        | Kind.PATH_MODULE_TYPE -> emit_token_stream state node
        | Kind.SIGNATURE_MODULE_TYPE -> render_signature_module_type_node state node
        | Kind.TYPEOF_MODULE_TYPE -> render_module_typeof_node state node
        | Kind.WITH_MODULE_TYPE -> render_with_module_type_node state node
        | _ -> unsupported_node "unsupported module type" node
      );
      render_node_attribute_suffix_after_child state original node

and render_struct_module_expr_node = fun state node ->
  let items = collect_structure_items_from_node node in
  let end_token = Ast.Node.first_child_token node ~kind:Kind.END_KW in
  let has_terminal_trivia = token_has_leading_comment end_token in
  if Int.equal (Vector.length items) 0 then (
    emit_text state "struct";
    if has_terminal_trivia then (
      emit_line state;
      (
        match end_token with
        | Some token ->
            with_indent state 2 (fun () -> emit_token_leading_comments_as_lines state token)
        | None -> ()
      )
    ) else
      emit_space state;
    emit_text state "end"
  ) else (
    emit_text state "struct";
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        render_structure_items state items;
        match end_token with
        | Some token when has_terminal_trivia ->
            emit_line state;
            emit_line state;
            emit_token_leading_comments_as_lines state token
        | _ -> ());
    if not has_terminal_trivia then
      emit_line state;
    emit_text state "end"
  )

and render_module_expr_atom_node = fun state node ->
  match module_expr_specific_node node with
  | Some node when node_kind_is node Kind.PATH_MODULE_EXPR
  || node_kind_is node Kind.STRUCT_MODULE_EXPR
  || node_kind_is node Kind.PAREN_MODULE_EXPR
  || node_kind_is node Kind.APPLY_MODULE_EXPR -> render_module_expr_node state node
  | Some node ->
      emit_text state "(";
      render_module_expr_node state node;
      emit_text state ")"
  | None -> unsupported_node "unsupported module expression" node

and render_apply_module_expr_node = fun state node ->
  let parts = collect_child_module_exprs node in
  match Vector.length parts with
  | 2 ->
      render_module_expr_atom_node state (Vector.get_unchecked parts ~at:0);
      emit_space state;
      emit_text state "(";
      render_module_expr_node state (Vector.get_unchecked parts ~at:1);
      emit_text state ")"
  | _ -> unsupported_node "incomplete applied module expression" node

and render_paren_module_expr_node = fun state node ->
  match first_child_node_matching node ~matches:is_module_expr_kind with
  | Some child ->
      emit_text state "(";
      render_module_expr_node state child;
      emit_text state ")"
  | None -> render_parenthesized_val_module_body state node

and render_module_expr_node = fun state node ->
  let original = node in
  match module_expr_specific_node original with
  | None -> unsupported_node "unsupported module expression" node
  | Some node ->
      (
        match node_kind node with
        | Kind.PATH_MODULE_EXPR -> emit_token_stream state node
        | Kind.STRUCT_MODULE_EXPR -> render_struct_module_expr_node state node
        | Kind.APPLY_MODULE_EXPR -> render_apply_module_expr_node state node
        | Kind.PAREN_MODULE_EXPR -> render_paren_module_expr_node state node
        | _ -> unsupported_node "unsupported module expression" node
      );
      render_node_attribute_suffix_after_child state original node

and render_let_module_body = fun state module_expr ->
  match Ast.LetModuleExpr.module_body_node module_expr with
  | Some node when node_kind_is node Kind.PATH_MODULE_EXPR -> render_module_expr_node state node
  | Some node when node_kind_is node Kind.PAREN_MODULE_EXPR -> render_module_expr_node state node
  | Some node when node_kind_is node Kind.STRUCT_MODULE_EXPR -> render_module_expr_node state node
  | Some node when node_kind_is node Kind.APPLY_MODULE_EXPR -> render_module_expr_node state node
  | Some node -> unsupported_node "unsupported let module body" node
  | None -> (
      match Ast.LetModuleExpr.module_body module_expr with
      | Path ->
          render_module_body_path
            state
            (Ast.LetModuleExpr.for_each_module_body_path_ident module_expr)
      | Struct ->
          emit_text state "struct";
          emit_space state;
          emit_text state "end"
      | Unsupported -> unsupported_node "unsupported let module body" module_expr
    )

and render_let_module_expr = fun state expr ->
  let module_expr =
    match Ast.cast_result_to_option (Ast.LetModuleExpr.cast expr) with
    | Some module_expr -> module_expr
    | None -> unsupported_node "unsupported let module expression" expr
  in
  match (
    Ast.LetModuleExpr.let_token module_expr,
    Ast.LetModuleExpr.module_token module_expr,
    Ast.LetModuleExpr.name module_expr,
    Ast.LetModuleExpr.equals_token module_expr,
    Ast.LetModuleExpr.in_token module_expr,
    Ast.LetModuleExpr.body module_expr
  ) with
  | (Some let_token, Some module_token, Some name, Some equals_token, Some in_token, Some body) ->
      emit_token state let_token;
      emit_space state;
      emit_token state module_token;
      emit_space state;
      emit_token state name;
      emit_space state;
      emit_token state equals_token;
      emit_space state;
      render_let_module_body state module_expr;
      emit_space state;
      emit_token state in_token;
      emit_line state;
      render_expr state body
  | _ -> unsupported_node "incomplete let module expression" expr

and render_first_class_module_path = fun state expr for_each_path_ident message ->
  let segments = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  for_each_path_ident ~fn:(fun ident -> Vector.push segments ~value:ident);
  if Vector.length segments = 0 then
    unsupported_node message expr
  else
    emit_joined_vector state segments ~sep:(fun () -> emit_text state ".") ~fn:(emit_token state)

and render_first_class_module_ascription_tokens = fun state expr colon_token ->
  let tokens = collect_child_tokens expr in
  match (token_vector_find_kind tokens Kind.COLON, token_vector_find_kind tokens Kind.RPAREN) with
  | (Some colon_index, Some closing_index) when Int.(Int.add colon_index 1 < closing_index) ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      emit_token_vector_range_stream
        state
        tokens
        ~start:(Int.add colon_index 1)
        ~stop:closing_index
  | _ -> unsupported_node "unsupported first-class module ascription" expr

and render_first_class_module_ascription = fun state expr ->
  match (Ast.FirstClassModuleExpr.colon_token expr, Ast.FirstClassModuleExpr.ascription expr) with
  | (None, Ast.FirstClassModuleExpr.NoAscription) -> ()
  | (Some colon_token, Ast.FirstClassModuleExpr.PathAscription) ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      render_first_class_module_path
        state
        expr
        (Ast.FirstClassModuleExpr.for_each_ascription_path_ident expr)
        "first-class module expression without module type path"
  | (Some colon_token, Ast.FirstClassModuleExpr.UnsupportedAscription) ->
      render_first_class_module_ascription_tokens state expr colon_token
  | (Some _, Ast.FirstClassModuleExpr.NoAscription)
  | (None, Ast.FirstClassModuleExpr.PathAscription)
  | (None, Ast.FirstClassModuleExpr.UnsupportedAscription) ->
      unsupported_node "unsupported first-class module ascription" expr

and render_first_class_module_expr = fun state expr ->
  let module_expr =
    match Ast.cast_result_to_option (Ast.FirstClassModuleExpr.cast expr) with
    | Some module_expr -> module_expr
    | None -> unsupported_node "unsupported first-class module expression" expr
  in
  match (
    Ast.FirstClassModuleExpr.opening_token module_expr,
    Ast.FirstClassModuleExpr.closing_token module_expr
  ) with
  | (Some opening_token, Some closing_token) -> (
      emit_token state opening_token;
      match Ast.Node.first_child_token module_expr ~kind:Kind.VAL_KW with
      | Some val_token ->
          let exprs = collect_child_exprs module_expr in
          emit_token state val_token;
          emit_space state;
          (
            match Vector.length exprs with
            | 1 -> render_expr state (Vector.get_unchecked exprs ~at:0)
            | _ -> unsupported_node "first-class module unpack without expression" expr
          );
          render_first_class_module_ascription state module_expr;
          emit_token state closing_token
      | None -> (
          match (
            Ast.FirstClassModuleExpr.module_token module_expr,
            Ast.FirstClassModuleExpr.module_path module_expr
          ) with
          | (Some module_token, Ast.FirstClassModuleExpr.ModulePath) ->
              emit_token state module_token;
              emit_space state;
              render_first_class_module_path
                state
                module_expr
                (Ast.FirstClassModuleExpr.for_each_module_path_ident module_expr)
                "first-class module expression without module path";
              render_first_class_module_ascription state module_expr;
              emit_token state closing_token
          | _ -> unsupported_node "unsupported first-class module expression" expr
        )
    )
  | _ -> unsupported_node "incomplete first-class module expression" expr

and render_binding_operator_clause = fun state (clause: Ast.BindingOperatorExpr.clause) ->
  match (clause.keyword, clause.operator) with
  | (Some keyword, Some operator) ->
      emit_token state keyword;
      emit_token state operator;
      emit_space state;
      render_let_binding_tail state clause.binding
  | _ -> unsupported "incomplete binding operator clause"

and binding_operator_clause_is_multiline = fun (clause: Ast.BindingOperatorExpr.clause) ->
  match Ast.LetBinding.body clause.binding with
  | Some body -> not (let_binding_body_renders_inline body)
  | None -> false

and render_binding_operator_expr = fun state expr ->
  let view =
    match Ast.cast_result_to_option (Ast.BindingOperatorExpr.cast expr) with
    | Some view -> view
    | None -> unsupported_node "unsupported binding operator expression" expr
  in
  let clauses = collect_binding_operator_clauses view in
  match (
    Vector.length clauses,
    Ast.BindingOperatorExpr.in_token view,
    Ast.BindingOperatorExpr.body view
  ) with
  | (0, _, _) -> unsupported_node "binding operator expression without binding" expr
  | (_, None, _) -> unsupported_node "binding operator expression without in" expr
  | (_, _, None) -> unsupported_node "binding operator expression without body" expr
  | (length, Some in_token, Some body) ->
      let before_clauses = state.line_count in
      let rec loop index =
        if Int.(index < length) then (
          render_binding_operator_clause state (Vector.get_unchecked clauses ~at:index);
          if Int.(index < Int.sub length 1) then
            emit_line state;
          loop (Int.add index 1)
        )
      in
      loop 0;
      if
        Int.equal length 1
        && Int.equal before_clauses state.line_count
        && not (binding_operator_clause_is_multiline (Vector.get_unchecked clauses ~at:0))
      then
        emit_space state
      else
        emit_line state;
      emit_token state in_token;
      emit_line state;
      render_in_body_expr state body

and let_binding_body_renders_inline = fun body ->
  if expr_has_leading_comment body then
    false
  else
    match ExprView.view body with
    | Fun { body = Some body } -> not (expr_has_leading_comment body || expr_is_multiline body)
    | Apply _ ->
        let (_, args) = collect_apply body in
        not (apply_args_have_heavy_nested_apply args || expr_is_multiline body)
    | Record
    | RecordUpdate -> record_expr_should_inline body
    | Function _
    | Assign _ -> false
    | _ -> not (expr_is_multiline body)

and let_binding_body_exceeds_width = fun ?(suffix_width = 0) state body ->
  let body_column = Int.add state.column 1 in
  match ExprView.view body with
  | Apply _ ->
      let (_, args) = collect_apply body in
      apply_expr_exceeds_width_from_column ~suffix_width state body args ~column:body_column
  | _ -> (
      match expr_flat_width body with
      | Some width -> Int.(body_column + width + suffix_width > state.width)
      | None -> false
    )

and let_binding_apply_body_exceeds_inline_equals_width = fun ?(suffix_width = 0) state body ->
  match ExprView.view body with
  | Apply _ ->
      let (_, args) = collect_apply body in
      apply_expr_exceeds_width_from_column
        ~suffix_width
        state
        body
        args
        ~column:(Int.add state.column 1)
  | _ -> false

and let_binding_rhs_decision = fun
  ?(body_suffix_width = 0)
  state
  body
  ~force_body_break
  ~binding_pattern_is_unit
  ~parameter_count ->
  let inline_body =
    match ExprView.view body with
    | Array
    | List
    | Record
    | Fun _
    | Function _ -> true
    | _ -> false
  in
  let is_assignment =
    match ExprView.view body with
    | Assign _ -> true
    | _ -> false
  in
  let single_constructor_payload =
    (not binding_pattern_is_unit)
    && (expr_is_single_constructor_multiline_apply body
    || (expr_is_single_constructor_apply body
    && let_binding_body_exceeds_width ~suffix_width:body_suffix_width state body))
  in
  let known_width_overflow =
    (Int.equal parameter_count 0
    && (not binding_pattern_is_unit)
    && let_binding_apply_body_exceeds_inline_equals_width ~suffix_width:body_suffix_width state body)
    || ((not binding_pattern_is_unit)
    && expr_is_apply body
    && let_binding_body_exceeds_width ~suffix_width:body_suffix_width state body)
    || let_binding_body_exceeds_width ~suffix_width:body_suffix_width state body
  in
  let flat_width =
    match ExprView.view body with
    | Apply _ -> (
        match expr_flat_width body with
        | Some _ as width -> width
        | None -> node_token_flat_width body
      )
    | _ -> expr_flat_width body
  in
  Layout.decide_let_binding_rhs
    (layout_context ~role:Layout.Let_rhs state)
    ~flat_width
    ~suffix_width:body_suffix_width
    ~force_body_break
    ~has_leading_comment:(expr_has_leading_comment body)
    ~is_pipeline:(expr_is_pipeline body)
    ~is_assignment
    ~inline_body
    ~single_constructor_payload
    ~known_width_overflow
    ~is_multiline:(expr_is_multiline body)

and let_binding_tail_should_render_multiline = fun binding ->
  match Ast.LetBinding.pattern binding with
  | Some pattern -> pattern_should_render_multiline pattern
  | None -> false

and let_binding_tail_renders_inline_before_in = fun binding ->
  if let_binding_tail_should_render_multiline binding then
    false
  else
    match Ast.LetBinding.body binding with
    | Some body -> let_binding_body_renders_inline body
    | None -> false

and render_let_expr = fun state expr body ->
  let bindings = collect_let_bindings_from_expr expr in
  let rec_token = Ast.Node.first_child_token expr ~kind:Kind.REC_KW in
  let and_tokens = collect_child_tokens_of_kind expr Kind.AND_KW in
  let and_tokens_have_leading_comment =
    let rec loop index =
      if Int.(index >= Vector.length and_tokens) then
        false
      else if Ast.Token.has_leading_comment (Vector.get_unchecked and_tokens ~at:index) then
        true
      else
        loop (Int.add index 1)
    in
    loop 0
  in
  let length = Vector.length bindings in
  let let_in_inline_suffix = " in" in
  let render_binding_head ?(body_suffix_width = 0) index binding =
    (
      if Int.equal index 0 then
        emit_text state "let"
      else
        (
          let and_index = Int.sub index 1 in
          let and_token =
            if Int.(and_index < Vector.length and_tokens) then
              Some (Vector.get_unchecked and_tokens ~at:and_index)
            else
              None
          in
          emit_token_or_keyword state and_token ~fallback:"and"
        )
    );
    if Int.equal index 0 then
      (
        match rec_token with
        | Some token ->
            emit_space state;
            emit_token state token
        | None -> ()
      );
    emit_space state;
    render_let_binding_tail_with_body_break ~body_suffix_width state binding false
  in
  if Int.equal length 1 then
    (
      let binding = Vector.get_unchecked bindings ~at:0 in
      let binding_head_multiline = let_binding_tail_should_render_multiline binding in
      let before_binding_lines = state.line_count in
      render_binding_head ~body_suffix_width:(String.length let_in_inline_suffix) 0 binding;
      let binding_rendered_multiline = Int.(state.line_count > before_binding_lines) in
      match Ast.LetBinding.body binding with
      | Some _ when binding_head_multiline ->
          emit_line state;
          emit_text state "in";
          emit_line state;
          render_in_body_expr state body
      | Some binding_body when (not binding_rendered_multiline)
      && let_binding_body_renders_inline binding_body
      && current_line_fits_text state let_in_inline_suffix ->
          emit_space state;
          emit_text state "in";
          emit_line state;
          render_in_body_expr state body
      | _ ->
          emit_line state;
          emit_text state "in";
          emit_line state;
          render_in_body_expr state body
    )
  else
    (
      let rec loop index =
        if Int.(index < length) then
          (
            let body_suffix_width =
              if Int.equal index (Int.sub length 1) then
                String.length let_in_inline_suffix
              else
                0
            in
            render_binding_head
              ~body_suffix_width
              index
              (Vector.get_unchecked bindings ~at:index);
            if Int.(index < Int.sub length 1) then
              emit_line state;
            loop (Int.add index 1)
          )
      in
      loop 0;
      if
        (not and_tokens_have_leading_comment)
        && let_binding_tail_renders_inline_before_in
          (Vector.get_unchecked bindings ~at:(Int.sub length 1))
        && current_line_fits_text state let_in_inline_suffix
      then
        emit_space state
      else
        emit_line state;
      emit_text state "in";
      emit_line state;
      render_in_body_expr state body
    )

and render_let_binding_tail = fun state binding ->
  render_let_binding_tail_with_body_break
    state
    binding
    false

and render_let_binding_tail_with_body_break = fun
  ?(body_suffix_width = 0)
  state
  binding
  force_body_break ->
  let binding_pattern = Ast.LetBinding.pattern binding in
  let binding_body = Ast.LetBinding.body binding in
  let annotation = Ast.LetBinding.type_annotation binding in
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count binding) in
  Ast.LetBinding.for_each_parameter binding ~fn:(fun param -> Vector.push parameters ~value:param);
  let parameter_count = Vector.length parameters in
  let return_annotation_wants_leading_space =
    if Int.equal parameter_count 0 then
      (
        match binding_pattern with
        | Some pattern -> pattern_ends_with_named_parameter pattern
        | None -> false
      )
    else
      let last = Vector.get_unchecked parameters ~at:(Int.sub parameter_count 1) in
      pattern_is_named_parameter last || (
        match binding_pattern with
        | Some pattern -> pattern_ends_with_named_parameter pattern
        | None -> false
      )
  in
  let emit_return_annotation_colon () =
    if return_annotation_wants_leading_space then
      emit_space state
    else
      drop_pending_spaces state;
    emit_text state ":"
  in
  let loose_binding_annotation =
    if Int.(parameter_count > 0) then
      (
        let last_index = Int.sub parameter_count 1 in
        let last = Vector.get_unchecked parameters ~at:last_index in
        match loose_parameter_binding_annotation last with
        | Some annotation -> Some (last_index, annotation)
        | None -> None
      )
    else
      None
  in
  let pattern_contains_annotation annotation =
    match binding_pattern with
    | Some pattern -> pattern_contains_type_annotation pattern annotation
    | None -> false
  in
  let binding_pattern_is_unit =
    match binding_pattern with
    | Some pattern -> pattern_is_unit pattern
    | None -> false
  in
  let render_binding_pattern () =
    match (binding_pattern, annotation) with
    | (Some constrained_pattern, Some annotation) -> (
        match PatternView.view constrained_pattern with
        | Constraint { pattern = Some pattern; annotation = Some existing } when same_ast_node
          existing
          annotation
        && pattern_ends_with_named_parameter pattern ->
            render_pattern state pattern;
            emit_space state;
            emit_text state ":";
            render_type_expr_after_tight_colon state ~suffix_width:0 annotation
        | _ -> render_pattern state constrained_pattern
      )
    | (Some pattern, None) -> render_pattern state pattern
    | (None, _) -> unsupported_node "let binding without pattern" binding
  in
  let binding_parameter_return_annotation index parameter =
    if Int.equal index (Int.sub parameter_count 1) then
      (
        match PatternView.view parameter with
        | Constraint { pattern = Some pattern; annotation = Some annotation } when pattern_ends_with_named_parameter
          pattern -> Some (pattern, annotation)
        | _ -> None
      )
    else
      None
  in
  render_binding_pattern ();
  let rec render_parameters index =
    if Int.(index < parameter_count) then
      (
        let parameter = Vector.get_unchecked parameters ~at:index in
        emit_space state;
        (
          match loose_binding_annotation with
          | Some (loose_index, _) when Int.equal index loose_index ->
              render_parameter_label_only state parameter
          | Some _ -> render_pattern state parameter
          | None -> (
              match binding_parameter_return_annotation index parameter with
              | Some (pattern, annotation) ->
                  render_pattern state pattern;
                  emit_space state;
                  emit_text state ":";
                  render_type_expr_after_tight_colon state ~suffix_width:0 annotation
              | None -> render_pattern state parameter
            )
        );
        render_parameters (Int.add index 1)
      )
  in
  render_parameters 0;
  (
    match (annotation, loose_binding_annotation) with
    | (Some annotation, Some _) ->
        emit_return_annotation_colon ();
        emit_space state;
        render_type_expr state annotation
    | (None, Some (_, annotation)) ->
        emit_return_annotation_colon ();
        emit_space state;
        emit_token_stream state annotation
    | (Some annotation, None) when not (pattern_contains_annotation annotation) ->
        emit_return_annotation_colon ();
        render_type_expr_after_tight_colon state ~suffix_width:0 annotation
    | (Some _, None)
    | (None, None) -> ()
  );
  emit_space state;
  emit_text state "=";
  match binding_body with
  | Some body -> (
      match (let_binding_rhs_decision
        ~body_suffix_width
        state
        body
        ~force_body_break
        ~binding_pattern_is_unit
        ~parameter_count).Layout.mode with
      | Layout.Inline ->
          emit_space state;
          if expr_is_tuple body then
            render_expr_atom state body
          else
            render_expr state body
      | Layout.Block
      | Layout.Vertical
      | Layout.Hang _
      | Layout.Isolate_child_blocks
      | Layout.Break_after_separator ->
          emit_line state;
          with_indent
            state
            2
            (fun () ->
              if expr_has_leading_comment body then
                render_let_body_with_leading_comment state body
              else
                render_expr state body)
    )
  | None -> unsupported_node "let binding without body" binding

and render_record_update_base = fun state base ->
  if expr_can_render_record_update_base_without_parens base then
    render_expr state base
  else
    render_parenthesized_expr state base

and render_record_expr = fun ?(force_multiline = false) state ~inline expr ->
  let base = Ast.RecordExpr.base expr in
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  let render_inline () =
    emit_text state "{";
    emit_space state;
    (
      match base with
      | Some base ->
          render_record_update_base state base;
          emit_space state;
          emit_text state "with";
          emit_space state
      | None -> ()
    );
    emit_joined_vector
      state
      fields
      ~sep:(fun () ->
        emit_text state ";";
        emit_space state)
      ~fn:(render_record_field state ~inline:true);
    emit_space state;
    emit_text state "}"
  in
  if Option.is_none base && Int.equal length 0 then
    emit_text state "{}"
  else if Option.is_some base && Int.equal length 0 then
    unsupported_node "record update without fields" expr
  else if inline then
    render_inline ()
  else
    match record_expr_decision state ~force_multiline expr fields with
    | { Layout.mode = Inline; _ } -> render_inline ()
    | { Layout.mode = Block; _ }
    | { Layout.mode = Vertical; _ }
    | { Layout.mode = Hang _; _ }
    | { Layout.mode = Isolate_child_blocks; _ }
    | { Layout.mode = Break_after_separator; _ } -> (
        emit_text state "{";
        emit_line state;
        with_indent
          state
          2
          (fun () ->
            (
              match base with
              | Some base ->
                  render_record_update_base state base;
                  emit_space state;
                  emit_text state "with";
                  emit_line state
              | None -> ()
            );
            let rec loop index =
              if Int.(index < length) then
                (
                  let field = Vector.get_unchecked fields ~at:index in
                  render_record_field state ~inline:false field;
                  (
                    match field with
                    | Ast.RecordExprField {
                        value = Some value;
                        _;
                      } when expr_has_terminal_trailing_sequence value -> ()
                    | Ast.RecordExprField _
                    | Ast.UnknownRecordExprField _ -> emit_text state ";"
                  );
                  if Int.(index < Int.sub length 1) then
                    emit_line state;
                  loop (Int.add index 1)
                )
            in
            loop 0);
        emit_line state;
        emit_text state "}"
      )

and render_record_field = fun state ~inline (field: Ast.record_expr_field_view) ->
  match field with
  | Ast.UnknownRecordExprField { node } -> unsupported_node "record field without path" node
  | Ast.RecordExprField { path; value; _ } -> (
      render_path state path;
      match value with
      | None -> ()
      | Some value ->
          emit_space state;
          emit_text state "=";
          let multiline_value =
            if inline then
              false
            else
              expr_is_multiline value
          in
          if multiline_value then (
            emit_line state;
            with_indent
              state
              2
              (fun () ->
                if expr_is_tuple value || expr_is_fun_or_parenthesized_fun value then
                  render_expr_atom state value
                else
                  render_expr state value)
          ) else (
            emit_space state;
            if expr_is_tuple value || expr_is_fun_or_parenthesized_fun value then
              render_expr_atom state value
            else
              render_expr state value
          )
    )

and render_array_expr = fun state expr ->
  let items = collect_array_items expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    emit_text state "[||]"
  else if layout_decision_is_inline (array_expr_decision state expr) then (
    emit_text state "[|";
    emit_joined_vector
      state
      items
      ~sep:(fun () ->
        emit_text state ";";
        emit_space state)
      ~fn:(fun item ->
        match ExprView.view item with
        | Record -> render_record_expr state ~inline:true item
        | _ -> render_expr state item);
    emit_text state "|]"
  ) else (
    emit_text state "[|";
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let rec loop index =
          if Int.(index < length) then
            (
              let item = Vector.get_unchecked items ~at:index in
              (
                match ExprView.view item with
                | Record -> render_record_expr state ~inline:true item
                | _ -> render_expr state item
              );
              emit_text state ";";
              if Int.(index < Int.sub length 1) then
                emit_line state;
              loop (Int.add index 1)
            )
        in
        loop 0);
    emit_line state;
    emit_text state "|]"
  )

and render_list_expr = fun state expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    emit_text state "[]"
  else if layout_decision_is_inline (list_expr_decision state expr) then (
    emit_text state "[ ";
    emit_joined_vector
      state
      items
      ~sep:(fun () ->
        emit_text state ";";
        emit_space state)
      ~fn:(fun item -> render_expr state item);
    if Ast.Expr.list_has_trailing_separator expr then
      emit_text state ";";
    emit_space state;
    emit_text state "]"
  ) else (
    emit_text state "[";
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let rec loop index =
          if Int.(index < length) then
            (
              let item = Vector.get_unchecked items ~at:index in
              let render_item () = render_expr state item in
              if state.suppress_list_item_leading_comments then
                with_node_leading_comments_suppressed state item render_item
              else
                render_item ();
              emit_text state ";";
              if Int.(index < Int.sub length 1) then
                emit_line state;
              loop (Int.add index 1)
            )
        in
        loop 0);
    emit_line state;
    emit_text state "]"
  )

and render_open_declaration = fun state decl ->
  emit_text state "open";
  (
    match Ast.Node.first_child_token decl ~kind:Kind.BANG with
    | Some bang -> emit_token state bang
    | None -> ()
  );
  emit_space state;
  let first = ref true in
  Ast.OpenDeclaration.for_each_path_ident
    decl
    ~fn:(fun ident ->
      if !first then
        first := false
      else
        emit_text state ".";
      emit_token state ident)

and render_include_declaration = fun state decl ->
  emit_text state "include";
  emit_space state;
  match Ast.IncludeDeclaration.body_node decl with
  | Some node when is_module_type_kind (node_kind node) -> render_module_type_node state node
  | Some node when is_module_expr_kind (node_kind node) -> render_module_expr_node state node
  | Some node -> unsupported_node "unsupported include declaration target" node
  | None -> unsupported_node "include declaration without target" decl

and render_record_type_field = fun state ~last field ->
  emit_node_leading_comments_as_lines state field;
  match Ast.RecordField.view field with
  | Ast.RecordField.Field { mutable_token; name; colon_token; annotation } ->
      (
        match mutable_token with
        | Some token ->
            emit_token state token;
            emit_space state
        | None -> ()
      );
      emit_token state name;
      emit_token state colon_token;
      let suffix_width =
        if last then
          0
        else
          1
      in
      render_type_expr_after_tight_colon state ~suffix_width annotation;
      if not last then
        emit_text state ";"
  | Ast.RecordField.Unknown node ->
      unsupported_node "unsupported record type field" node

and record_type_field_flat_width = fun field ->
  match Ast.RecordField.view field with
  | Ast.RecordField.Field { mutable_token; name; annotation; _ } -> (
      match type_expr_flat_width annotation with
      | Some annotation_width ->
          let mutable_width =
            match mutable_token with
            | Some token -> Int.add (Slice.length (Ast.Token.slice token)) 1
            | None -> 0
          in
          Some Int.(mutable_width + Slice.length (Ast.Token.slice name) + 2 + annotation_width)
      | None -> None
    )
  | Ast.RecordField.Unknown _ -> None

and record_type_flat_width = fun fields ->
  let length = Vector.length fields in
  if Int.equal length 0 then
    Some 2
  else
    let rec loop index total =
      if Int.(index >= length) then
        Some Int.(total + 4)
      else
        match record_type_field_flat_width (Vector.get_unchecked fields ~at:index) with
        | None -> None
        | Some field_width ->
            let separator_width =
              if Int.equal index 0 then
                0
              else
                2
            in
            loop (Int.add index 1) Int.(total + separator_width + field_width)
    in
    loop 0 0

and record_type_has_terminal_trivia = fun record_type ->
  match Ast.RecordType.closing_token record_type with
  | Some closing -> Ast.Token.has_leading_comment closing
  | None -> false

and record_type_fields_have_leading_comment = fun fields ->
  let length = Vector.length fields in
  let rec loop index =
    if Int.(index >= length) then
      false
    else if node_has_leading_comment (Vector.get_unchecked fields ~at:index) then
      true
    else
      loop (Int.add index 1)
  in
  loop 0

and type_expr_is_light_record_field_annotation = fun annotation ->
  match TypeExprView.view annotation with
  | Path { path } ->
      let tokens = collect_child_tokens path in
      Int.equal (Vector.length tokens) 1
  | Var _
  | Wildcard -> true
  | _ -> false

and record_type_fields_are_light_inline = fun fields ->
  let length = Vector.length fields in
  let rec loop index =
    if Int.(index >= length) then
      true
    else
      let field = Vector.get_unchecked fields ~at:index in
      match Ast.RecordField.view field with
      | Ast.RecordField.Field { annotation; _ } ->
          if type_expr_is_light_record_field_annotation annotation then
            loop (Int.add index 1)
          else
            false
      | Ast.RecordField.Unknown _ -> false
  in
  loop 0

and record_type_fields_have_multi_field_mutable = fun fields ->
  let length = Vector.length fields in
  if Int.(length <= 1) then
    false
  else
    let rec loop index =
      if Int.(index >= length) then
        false
      else if
        match Ast.RecordField.view (Vector.get_unchecked fields ~at:index) with
        | Ast.RecordField.Field { mutable_token = Some _; _ } -> true
        | Ast.RecordField.Field { mutable_token = None; _ }
        | Ast.RecordField.Unknown _ -> false
      then
        true
      else
        loop (Int.add index 1)
    in
    loop 0

and record_type_should_inline = fun state ~allow_inline record_type fields ->
  let has_leading_comment = record_type_fields_have_leading_comment fields in
  let has_trailing_comment = record_type_has_terminal_trivia record_type in
  let inline_shape =
    allow_inline
    && not has_trailing_comment
    && not has_leading_comment
    && not (record_type_fields_have_multi_field_mutable fields)
    && record_type_fields_are_light_inline fields
  in
  let flat_width = record_type_flat_width fields in
  match Layout.decide_record_type
    (layout_context state)
    ~flat_width
    ~allow_inline:inline_shape
    ~has_leading_comment
    ~has_trailing_comment
    ~item_count:(Vector.length fields) with
  | { Layout.mode = Inline; _ } -> true
  | _ -> false

and render_record_type_closing = fun state ~leading_trivia_indent record_type ->
  match Ast.RecordType.closing_token record_type with
  | Some closing ->
      if Ast.Token.has_leading_comment closing && Int.(leading_trivia_indent > 0) then (
        with_indent
          state
          leading_trivia_indent
          (fun () -> emit_token_leading_comments_as_lines state closing);
        if state.line_start then
          state.column <- state.indent
      ) else
        emit_token_leading_comments_as_lines state closing;
      emit_token state closing
  | None -> emit_text state "}"

and render_record_type_inline = fun state record_type fields ->
  let length = Vector.length fields in
  if Int.equal length 0 then (
    emit_text state "{";
    render_record_type_closing state ~leading_trivia_indent:0 record_type
  ) else (
    emit_text state "{";
    emit_space state;
    emit_joined_vector
      state
      fields
      ~sep:(fun () ->
        emit_text state ";";
        emit_space state)
      ~fn:(render_record_type_field state ~last:true);
    emit_space state;
    render_record_type_closing state ~leading_trivia_indent:0 record_type
  )

and render_record_type_field_trailing_trivia = fun state record_type fields index ->
  let length = Vector.length fields in
  let next_token =
    if Int.(index < Int.sub length 1) then
      Ast.Node.first_descendant_token (Vector.get_unchecked fields ~at:(Int.add index 1))
    else
      Ast.RecordType.closing_token record_type
  in
  match next_token with
  | Some token -> ignore (emit_record_field_trailing_trivia_from_token state token)
  | None -> ()

and render_record_type = fun state ~allow_inline record_type ->
  let fields = collect_record_type_fields record_type in
  (
    match Ast.RecordType.private_token record_type with
    | Some token ->
        emit_token state token;
        emit_space state
    | None -> ()
  );
  if record_type_should_inline state ~allow_inline record_type fields then
    render_record_type_inline state record_type fields
  else (
    emit_text state "{";
    if Vector.length fields > 0 then (
      emit_line state;
      with_indent
        state
        2
        (fun () ->
          let length = Vector.length fields in
          let rec loop index =
            if Int.(index < length) then (
              render_record_type_field
                state
                ~last:false
                (Vector.get_unchecked fields ~at:index);
              render_record_type_field_trailing_trivia state record_type fields index;
              if Int.(index < Int.sub length 1) then
                emit_line state;
              loop (Int.add index 1)
            )
          in
          loop 0);
      emit_line state
    );
    render_record_type_closing state ~leading_trivia_indent:2 record_type
  )

and external_declaration_suffix_width = fun primitive_strings attribute_tokens ->
  let primitive_width =
    match token_vector_spaced_flat_width primitive_strings with
    | Some width -> width
    | None -> 0
  in
  let attribute_width =
    if Int.equal (Vector.length attribute_tokens) 0 then
      0
    else
      match attribute_token_vector_spaced_flat_width attribute_tokens with
      | Some width -> Int.add 1 width
      | None -> 0
  in
  Int.(3 + primitive_width + attribute_width)

and render_external_declaration_equals = fun state equals_token ->
  emit_token state equals_token

and render_external_primitive_body = fun state primitive_strings attribute_tokens ->
  emit_joined_vector
    state
    primitive_strings
    ~sep:(fun () -> emit_space state)
    ~fn:(emit_token state);
  if Vector.length attribute_tokens > 0 then (
    emit_space state;
    emit_attribute_token_vector_stream state attribute_tokens
  )

and render_external_primitive_suffix = fun
  state
  equals_token
  primitive_strings
  attribute_tokens
  ~leading_space ->
  if leading_space then
    emit_space state;
  render_external_declaration_equals state equals_token;
  emit_space state;
  render_external_primitive_body state primitive_strings attribute_tokens

and render_external_primitive_body_after_equals = fun
  state
  equals_token
  primitive_strings
  attribute_tokens ->
  (
    if Int.(state.column + 2 <= state.width) then (
      emit_space state;
      render_external_declaration_equals state equals_token
    ) else (
      emit_line state;
      with_indent state 2 (fun () -> render_external_declaration_equals state equals_token)
    )
  );
  emit_line state;
  with_indent
    state
    2
    (fun () ->
      render_external_primitive_body state primitive_strings attribute_tokens)

and render_external_declaration = fun state decl ->
  match Ast.ExternalDeclaration.view decl with
  | Ast.ExternalDeclaration.External {
      name = name_tokens;
      colon_token;
      annotation;
      equals_token;
      primitives = primitive_strings;
      attributes = attribute_tokens;
    } ->
      emit_text state "external";
      emit_space state;
      emit_token_vector_stream state name_tokens;
      (
        emit_token state colon_token;
        let suffix_width = external_declaration_suffix_width primitive_strings attribute_tokens in
        if type_expr_should_break_after_colon state ~suffix_width annotation then
          if type_expr_should_break_after_colon state ~suffix_width:2 annotation then (
            render_type_expr_after_tight_colon state ~suffix_width:2 annotation;
            render_external_primitive_body_after_equals
              state
              equals_token
              primitive_strings
              attribute_tokens
          ) else (
            render_type_expr_after_colon state annotation;
            render_external_primitive_body_after_equals
              state
              equals_token
              primitive_strings
              attribute_tokens
          )
        else (
          render_type_expr_after_colon state annotation;
          render_external_primitive_suffix
            state
            equals_token
            primitive_strings
            attribute_tokens
            ~leading_space:true
        )
      )
  | Ast.ExternalDeclaration.Unknown node ->
      unsupported_node "unsupported external declaration" node

and render_exception_payload = fun state payload ->
  match payload with
  | Ast.ExceptionDeclaration.TypeExpr type_expr -> render_type_expr state type_expr
  | Ast.ExceptionDeclaration.Record record_type ->
      render_record_type state ~allow_inline:true record_type

and render_exception_rhs = fun state rhs ->
  match rhs with
  | Ast.ExceptionDeclaration.Bare -> ()
  | Ast.ExceptionDeclaration.Alias { equals_token; path } ->
      emit_space state;
      emit_token state equals_token;
      emit_space state;
      render_path state path
  | Ast.ExceptionDeclaration.Payload { of_token; payload } ->
      emit_space state;
      emit_token state of_token;
      emit_space state;
      render_exception_payload state payload
  | Ast.ExceptionDeclaration.Unknown node -> unsupported_node "unsupported exception declaration" node

and render_exception_declaration = fun state decl ->
  (
    match Ast.ExceptionDeclaration.keyword_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state "exception"
  );
  emit_space state;
  (
    match Ast.ExceptionDeclaration.name decl with
    | Some name -> emit_token state name
    | None -> unsupported_node "exception declaration without name" decl
  );
  render_exception_rhs state (Ast.ExceptionDeclaration.view decl)

and render_variant_constructor_payload = fun state payload ->
  match payload with
  | Ast.VariantConstructor.TypeExpr type_expr -> render_type_expr state type_expr
  | Ast.VariantConstructor.Record record_type ->
      with_indent state 2 (fun () -> render_record_type state ~allow_inline:true record_type)

and render_variant_constructor_gadt_rhs = fun
  state
  ~colon_token
  ~record_payload
  ~arrow_token
  ~result ->
  match record_payload with
  | Some record_type ->
      emit_token state colon_token;
      emit_space state;
      with_indent state 2 (fun () -> render_record_type state ~allow_inline:false record_type);
      (
        match arrow_token with
        | Some token ->
            emit_space state;
            emit_token state token
        | None ->
            emit_space state;
            emit_text state "->"
      );
      emit_space state;
      render_type_expr state result
  | None ->
      emit_token state colon_token;
      render_type_expr_after_tight_colon state ~suffix_width:0 result

and render_variant_constructor_rhs = fun state rhs ->
  match rhs with
  | Ast.VariantConstructor.Plain -> ()
  | Ast.VariantConstructor.Payload { of_token; payload } ->
      emit_space state;
      emit_token state of_token;
      emit_space state;
      render_variant_constructor_payload state payload
  | Ast.VariantConstructor.Gadt {
      colon_token;
      record_payload;
      arrow_token;
      result;
    } ->
      render_variant_constructor_gadt_rhs state ~colon_token ~record_payload ~arrow_token ~result

and render_variant_constructor = fun state ~private_token constructor ->
  (
    match private_token with
    | Some token ->
        emit_token state token;
        emit_space state
    | None -> ()
  );
  match Ast.VariantConstructor.view constructor with
  | Ast.VariantConstructor.Constructor { pipe_token; name; rhs } ->
      (
        match pipe_token with
        | Some pipe_token ->
            emit_token_leading_comments_as_lines state pipe_token;
            emit_token state pipe_token;
            emit_space state
        | None ->
            emit_text state "|";
            emit_space state
      );
      emit_token state name;
      render_variant_constructor_rhs state rhs
  | Ast.VariantConstructor.Unknown node ->
      (
        match Ast.VariantConstructor.pipe_token constructor with
        | Some pipe_token ->
            emit_token_leading_comments_as_lines state pipe_token;
            emit_token state pipe_token;
            emit_space state
        | None ->
            emit_text state "|";
            emit_space state
      );
      unsupported_node "unsupported variant constructor" node

and render_variant_type = fun state ~private_token variant_type ->
  let constructors = collect_variant_constructors variant_type in
  let length = Vector.length constructors in
  if Int.(length > 0) then (
    emit_line state;
    with_indent
      state
      2
      (fun () ->
        let rec loop index =
          if Int.(index < length) then
            (
              let constructor_private_token =
                if Int.equal index 0 then
                  private_token
                else
                  None
              in
              render_variant_constructor
                state
                ~private_token:constructor_private_token
                (Vector.get_unchecked constructors ~at:index);
              if Int.(index < Int.sub length 1) then
                emit_line state;
              loop (Int.add index 1)
            )
        in
        loop 0)
  )

and render_type_parameter = fun state parameter ->
  match parameter with
  | Ast.TypeDeclaration.Named {
    name;
    quote;
    variance;
    injective
  } ->
      emit_optional_token state variance;
      emit_optional_token state injective;
      emit_optional_token state quote;
      emit_token state name
  | Ast.TypeDeclaration.Wildcard { wildcard; variance; injective } ->
      emit_optional_token state variance;
      emit_optional_token state injective;
      emit_token state wildcard

and render_type_parameters = fun state parameters ->
  match Vector.length parameters with
  | 0 -> ()
  | 1 ->
      render_type_parameter state (Vector.get_unchecked parameters ~at:0);
      emit_space state
  | length ->
      emit_text state "(";
      if Int.(length > 4) then (
        emit_line state;
        with_indent
          state
          2
          (fun () ->
            emit_joined_vector
              state
              parameters
              ~sep:(fun () ->
                emit_text state ",";
                emit_line state)
              ~fn:(render_type_parameter state));
        emit_line state
      ) else
        emit_joined_vector
          state
          parameters
          ~sep:(fun () ->
            emit_text state ",";
            emit_space state)
          ~fn:(render_type_parameter state);
      emit_text state ")";
      emit_space state

and type_member_has_equals = fun member ->
  let found = ref false in
  Ast.TypeDeclaration.Member.for_each_child_token
    member
    ~fn:(fun token ->
      if token_kind_is token Kind.EQ then
        found := true);
  !found

and type_member_private_token = fun member ->
  let saw_equals = ref false in
  let found = ref None in
  Ast.TypeDeclaration.Member.for_each_child_token
    member
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if !saw_equals then
            (
              if token_kind_is token Kind.PRIVATE_KW then
                found := Some token
            )
          else if token_kind_is token Kind.EQ then
            saw_equals := true);
  (
    match !found with
    | Some _ -> ()
    | None ->
        Ast.TypeDeclaration.Member.for_each_child_node
          member
          ~fn:(fun node ->
            match !found with
            | Some _ -> ()
            | None -> found := Ast.Node.first_child_token node ~kind:Kind.PRIVATE_KW)
  );
  !found

and type_member_is_alias_extensible = fun member ->
  let equals_count = ref 0 in
  let saw_dotdot_after_alias = ref false in
  Ast.TypeDeclaration.Member.for_each_child_token
    member
    ~fn:(fun token ->
      if token_kind_is token Kind.EQ then
        equals_count := Int.add !equals_count 1
      else if Int.((!equals_count) >= 2) && token_kind_is token Kind.DOTDOT then
        saw_dotdot_after_alias := true);
  !saw_dotdot_after_alias

and render_alias_extensible_suffix = fun state member ->
  if type_member_is_alias_extensible member then (
    emit_space state;
    emit_text state "=";
    emit_space state;
    emit_text state ".."
  )

and type_expr_should_break_after_equals = fun state type_expr ->
  if type_expr_is_closed_backtick_bracketed_opaque type_expr then
    true
  else if not (type_expr_is_arrow type_expr) then
    false
  else
    match type_expr_flat_width type_expr with
    | Some width -> Int.(state.column + 1 + width > state.width)
    | None -> false

and render_type_expr_after_equals = fun state type_expr ->
  if type_expr_is_closed_backtick_bracketed_opaque type_expr then (
    emit_space state;
    render_type_expr state type_expr
  ) else if type_expr_should_break_after_equals state type_expr then (
    emit_line state;
    with_indent state 2 (fun () -> render_multiline_type_arrow state type_expr)
  ) else (
    emit_space state;
    render_type_expr state type_expr
  )

and type_member_representation_equals_token = fun member ->
  let equals_seen = ref 0 in
  let found = ref None in
  Ast.TypeDeclaration.Member.for_each_child_token
    member
    ~fn:(fun token ->
      if Option.is_none !found && token_kind_is token Kind.EQ then (
        equals_seen := Int.add !equals_seen 1;
        if Int.equal !equals_seen 2 then
          found := Some token
      ));
  !found

and render_type_member_representation_separator = fun state member ->
  emit_space state;
  (
    match type_member_representation_equals_token member with
    | Some token -> emit_token state token
    | None -> emit_text state "="
  )

and collect_type_member_body_tokens = fun member ->
  let tokens = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  let collecting = ref false in
  let tree = (Ast.TypeDeclaration.Member.declaration member).tree in
  Ast.TypeDeclaration.Member.for_each_child
    member
    ~fn:(
      function
      | Syn.SyntaxTree.Token id ->
          let token: Ast.Token.t = { tree; id } in
          if !collecting then
            Vector.push tokens ~value:token
          else if token_kind_is token Kind.EQ then
            collecting := true
      | Syn.SyntaxTree.Node id ->
          if !collecting then
            let node: Ast.Node.t = { tree; id } in
            Ast.Node.for_each_token node ~fn:(fun token -> Vector.push tokens ~value:token)
      | Syn.SyntaxTree.Missing _ -> ()
    );
  tokens

and collect_type_member_constraint_tokens = fun member ->
  let tokens = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  let collecting = ref false in
  let tree = (Ast.TypeDeclaration.Member.declaration member).tree in
  let stop =
    match type_member_first_attribute_suffix_child_index member with
    | Some index -> index
    | None -> Ast.TypeDeclaration.Member.child_count member
  in
  let rec loop index =
    if Int.(index < stop) then (
      (
        match Ast.TypeDeclaration.Member.child_at member index with
        | Some (Syn.SyntaxTree.Token id) ->
            let token: Ast.Token.t = { tree; id } in
            if token_kind_is token Kind.CONSTRAINT_KW then
              collecting := true;
            if !collecting then
              Vector.push tokens ~value:token
        | Some (Syn.SyntaxTree.Node id) ->
            if !collecting then
              let node: Ast.Node.t = { tree; id } in
              Ast.Node.for_each_token node ~fn:(fun token -> Vector.push tokens ~value:token)
        | Some (Syn.SyntaxTree.Missing _)
        | None -> ()
      );
      loop (Int.add index 1)
    )
  in
  loop 0;
  tokens

and render_type_member_constraints = fun state member ->
  let tokens = collect_type_member_constraint_tokens member in
  if Int.(Vector.length tokens > 0) then (
    emit_line state;
    with_indent state 2 (fun () -> emit_type_constraint_token_vector_stream state tokens)
  )

and render_private_alias_type_member_body = fun state member ->
  match type_member_private_token member with
  | None -> false
  | Some _ ->
      let body_tokens = collect_type_member_body_tokens member in
      if Int.equal (Vector.length body_tokens) 0 then
        false
      else (
        emit_space state;
        emit_text state "=";
        emit_space state;
        emit_token_vector_stream state body_tokens;
        true
      )

and render_type_attribute_items = fun state attributes ->
  let length = Vector.length attributes in
  if Int.(length > 0) then
    let rec loop index =
      if Int.(index < length) then (
        emit_line state;
        render_attribute_item state (Vector.get_unchecked attributes ~at:index);
        loop (Int.add index 1)
      )
    in
    loop 0

and render_type_attribute_items_inline = fun state attributes ->
  let length = Vector.length attributes in
  if Int.(length > 0) then (
    emit_space state;
    emit_joined_vector
      state
      attributes
      ~sep:(fun () -> emit_space state)
      ~fn:(render_attribute_item state)
  )

and render_type_member = fun state member ->
  (
    match Ast.TypeDeclaration.Member.shell_token member with
    | Some token -> emit_token state token
    | None -> emit_text state "type"
  );
  (
    match Ast.TypeDeclaration.Member.nonrec_token member with
    | Some token ->
        emit_space state;
        emit_token state token
    | None -> ()
  );
  emit_space state;
  render_type_parameters state (collect_type_member_parameters member);
  (
    match Ast.TypeDeclaration.Member.name member with
    | Some name -> emit_token state name
    | None -> unsupported "type declaration member without name"
  );
  (
    match (
      Ast.TypeDeclaration.Member.variant_type member,
      Ast.TypeDeclaration.Member.record_type member,
      Ast.TypeDeclaration.Member.manifest member
    ) with
    | (Some variant_type, _, Some manifest) ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest;
        render_type_member_representation_separator state member;
        render_variant_type state ~private_token:(type_member_private_token member) variant_type
    | (Some variant_type, _, None) ->
        emit_space state;
        emit_text state "=";
        render_variant_type state ~private_token:(type_member_private_token member) variant_type
    | (None, Some record_type, Some manifest) ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest;
        render_type_member_representation_separator state member;
        emit_space state;
        render_record_type state ~allow_inline:true record_type
    | (None, Some record_type, None) ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_record_type state ~allow_inline:true record_type
    | (None, None, Some manifest) ->
        emit_space state;
        emit_text state "=";
        render_type_expr_after_equals state manifest;
        render_alias_extensible_suffix state member
    | (None, None, None) when type_member_has_equals member ->
        if not (render_private_alias_type_member_body state member) then
          unsupported "unsupported type declaration body"
    | (None, None, None) -> ()
  );
  render_type_member_constraints state member;
  let suffix_tokens = collect_type_member_attribute_suffix_tokens member in
  if Int.(Vector.length suffix_tokens > 0) then (
    emit_space state;
    emit_attribute_token_vector_stream state suffix_tokens
  ) else
    render_type_attribute_items_inline state (collect_type_member_attribute_items member)

and render_type_declaration = fun state decl ->
  let members = collect_type_members decl in
  let length = Vector.length members in
  let single_pseudo_member =
    Int.equal length 1
    && Int.equal (Ast.TypeDeclaration.Member.start_index (Vector.get_unchecked members ~at:0)) 0
    && Int.equal
      (Ast.TypeDeclaration.Member.stop_index (Vector.get_unchecked members ~at:0))
      (Ast.Node.child_count decl)
  in
  let rec loop index =
    if Int.(index < length) then (
      if Int.(index > 0) then (
        emit_line state;
        emit_line state
      );
      render_type_member state (Vector.get_unchecked members ~at:index);
      loop (Int.add index 1)
    )
  in
  loop 0;
  if not single_pseudo_member then
    render_type_attribute_items_inline state (collect_type_declaration_attribute_items decl)

and render_type_extension_declaration = fun state decl ->
  (
    match Ast.TypeExtensionDeclaration.keyword_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state "type"
  );
  emit_space state;
  render_type_parameters state (collect_type_extension_parameters decl);
  let name_tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeExtensionDeclaration.for_each_name_ident
    decl
    ~fn:(fun token -> Vector.push name_tokens ~value:token);
  if Vector.length name_tokens = 0 then
    unsupported_node "type extension declaration without name" decl
  else
    emit_joined_vector state name_tokens ~sep:(fun () -> emit_text state ".") ~fn:(emit_token state);
  emit_space state;
  (
    match (
      Ast.TypeExtensionDeclaration.plus_token decl,
      Ast.TypeExtensionDeclaration.equals_token decl
    ) with
    | (Some plus_token, Some equals_token) ->
        emit_token state plus_token;
        emit_token state equals_token
    | _ -> emit_text state "+="
  );
  (
    match Ast.TypeExtensionDeclaration.variant_type decl with
    | Some variant_type -> render_variant_type state ~private_token:None variant_type
    | None -> unsupported "unsupported type extension declaration body"
  )

and render_module_body_path = fun state for_each_path_ident ->
  let first = ref true in
  for_each_path_ident
    ~fn:(fun ident ->
      if !first then
        first := false
      else
        emit_text state ".";
      emit_token state ident)

and render_module_declaration_separator = fun state decl ~default ->
  let tight =
    match Ast.ModuleDeclaration.separator_token decl with
    | Some token -> token_kind_is token Kind.COLON
    | None -> String.equal default ":"
  in
  if not tight then
    emit_space state;
  (
    match Ast.ModuleDeclaration.separator_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state default
  );
  emit_space state

and render_module_type_declaration_equals = fun state decl ->
  emit_space state;
  (
    match Ast.ModuleTypeDeclaration.equals_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state "="
  );
  emit_space state

and render_shell_body_after_separator = fun state node separator_kind ~label ->
  let tokens = collect_node_tokens node in
  match token_vector_find_kind tokens separator_kind with
  | None -> unsupported label
  | Some separator_index ->
      emit_space state;
      emit_token state (Vector.get_unchecked tokens ~at:separator_index);
      emit_space state;
      emit_shell_token_vector_range_stream
        state
        tokens
        ~start:(Int.add separator_index 1)
        ~stop:(Vector.length tokens)

and render_module_typeof_body = fun state decl ->
  render_module_declaration_separator state decl ~default:":";
  emit_text state "module type of";
  emit_space state;
  render_module_body_path state (Ast.ModuleDeclaration.for_each_typeof_body_path_ident decl)

and render_value_declaration = fun state decl ->
  match Ast.ValueDeclaration.view decl with
  | Ast.ValueDeclaration.Value { name = name_tokens; colon_token; annotation } ->
      emit_text state "val";
      emit_space state;
      if token_vector_is_parenthesized_operator_name name_tokens then
        render_parenthesized_operator_tokens state name_tokens
      else
        emit_token_vector_stream state name_tokens;
      emit_token state colon_token;
      let type_tokens = collect_node_tokens_after_direct_token decl Kind.COLON in
      let annotation_tokens = collect_node_tokens annotation in
      if Int.(Vector.length type_tokens > Vector.length annotation_tokens) then (
        emit_space state;
        emit_core_type_token_vector_stream state type_tokens
      ) else
        render_type_expr_after_tight_colon state ~suffix_width:0 annotation
  | Ast.ValueDeclaration.Unknown node ->
      unsupported_node "unsupported value declaration" node

and render_let_declaration = fun state decl ->
  render_let_declaration_with_body_break
    state
    decl
    false

and render_let_declaration_with_body_break = fun state decl force_body_break ->
  let bindings = collect_let_bindings decl in
  let rec_token = Ast.LetDeclaration.rec_token decl in
  let and_tokens = collect_child_tokens_of_kind decl Kind.AND_KW in
  let length = Vector.length bindings in
  let rec loop index =
    if Int.(index < length) then (
      (
        if Int.equal index 0 then
          emit_text state "let"
        else
          (
            let and_index = Int.sub index 1 in
            let and_token =
              if Int.(and_index < Vector.length and_tokens) then
                Some (Vector.get_unchecked and_tokens ~at:and_index)
              else
                None
            in
            emit_token_or_keyword state and_token ~fallback:"and"
          )
      );
      if Int.equal index 0 then
        (
          match rec_token with
          | Some token ->
              emit_space state;
              emit_token state token
          | None -> ()
        );
      emit_space state;
      render_let_binding_tail_with_body_break
        state
        (Vector.get_unchecked bindings ~at:index)
        force_body_break;
      if Int.(index < Int.sub length 1) then (
        emit_line state;
        emit_line state
      );
      loop (Int.add index 1)
    )
  in
  loop 0

and render_module_declaration_member = fun state member ->
  let previous_token = ref None in
  let previous_node = ref false in
  let paren_depth = ref 0 in
  let member_token_wants_space_before previous token =
    if token_kind_is token Kind.COLON then
      false
    else
      token_wants_space_before previous token
  in
  let emit_member_token token =
    (
      match (!previous_token, !previous_node) with
      | (_, true) -> emit_space state
      | (Some previous, false) when member_token_wants_space_before previous token ->
          emit_space state
      | (Some _, false)
      | (None, false) -> ()
    );
    emit_token state token;
    (
      if token_kind_is token Kind.LPAREN then
        paren_depth := Int.add !paren_depth 1
      else if token_kind_is token Kind.RPAREN then
        paren_depth := Int.max 0 (Int.sub !paren_depth 1)
    );
    previous_token := Some token;
    previous_node := false
  in
  let emit_member_node render node =
    (
      match (!previous_token, !previous_node) with
      | (Some _, _)
      | (None, true) -> emit_space state
      | (None, false) -> ()
    );
    render state node;
    previous_token := None;
    previous_node := true
  in
  let rec loop index =
    if Int.(index < Ast.ModuleDeclaration.Member.child_count member) then
      (
        match Ast.ModuleDeclaration.Member.child_node_at member index with
        | Some node when is_module_type_kind (node_kind node) ->
            emit_member_node render_module_type_node node;
            loop (Int.add index 1)
        | Some node when is_module_expr_kind (node_kind node) ->
            emit_member_node render_module_expr_node node;
            loop (Int.add index 1)
        | Some node -> unsupported_node "unsupported module declaration child" node
        | None -> (
            match Ast.ModuleDeclaration.Member.child_token_at member index with
            | Some token ->
                emit_member_token token;
                loop (Int.add index 1)
            | None -> loop (Int.add index 1)
          )
      )
  in
  loop 0

and render_module_declaration_members = fun state decl ->
  let members = collect_module_declaration_members decl in
  let length = Vector.length members in
  if Int.equal length 0 then
    unsupported_node "module declaration without members" decl
  else
    let rec loop index =
      if Int.(index < length) then (
        if Int.(index > 0) then (
          emit_line state;
          emit_line state
        );
        render_module_declaration_member state (Vector.get_unchecked members ~at:index);
        loop (Int.add index 1)
      )
    in
    loop 0

and module_declaration_has_ascribed_body = fun decl ->
  let found = ref false in
  Ast.ModuleDeclaration.for_each_member
    decl
    ~fn:(fun member ->
      match (
        Ast.ModuleDeclaration.Member.module_type member,
        Ast.ModuleDeclaration.Member.module_expr member
      ) with
      | (Some _, Some _) -> found := true
      | _ -> ());
  !found

and module_declaration_has_head_parameter = fun decl ->
  let found = ref false in
  Ast.ModuleDeclaration.for_each_member
    decl
    ~fn:(fun member ->
      let rec loop index =
        if not !found && Int.(index < Ast.ModuleDeclaration.Member.child_count member) then
          (
            match Ast.ModuleDeclaration.Member.child_token_at member index with
            | Some token when token_kind_is token Kind.EQ || token_kind_is token Kind.COLON -> ()
            | Some token when token_kind_is token Kind.LPAREN -> found := true
            | Some _
            | None -> loop (Int.add index 1)
          )
      in
      loop 0);
  !found

and module_declaration_has_empty_struct_body = fun decl ->
  match Ast.ModuleDeclaration.body decl with
  | Ast.ModuleDeclaration.Expr { body } -> (
      match Ast.ModuleExpr.view body with
      | Ast.ModuleExpr.Structure _ -> Vector.is_empty (collect_structure_items_from_module_decl decl)
      | _ -> false
    )
  | _ -> false

and module_declaration_body_requires_member_render = fun decl body ->
  match body with
  | Ast.ModuleDeclaration.Expr { body } -> (
      match Ast.ModuleExpr.view body with
      | Ast.ModuleExpr.Path _
      | Ast.ModuleExpr.Structure _
      | Ast.ModuleExpr.Apply _ -> false
      | Ast.ModuleExpr.Opaque _ -> not (node_has_token_kind decl Kind.PERCENT)
      | Ast.ModuleExpr.Functor _
      | Ast.ModuleExpr.Constraint _
      | Ast.ModuleExpr.Error _
      | Ast.ModuleExpr.Unknown _ -> true
    )
  | Ast.ModuleDeclaration.Type { body } -> (
      match Ast.ModuleTypeExpr.view body with
      | Ast.ModuleTypeExpr.Path _
      | Ast.ModuleTypeExpr.Signature _
      | Ast.ModuleTypeExpr.Typeof _ -> false
      | Ast.ModuleTypeExpr.With _
      | Ast.ModuleTypeExpr.Functor _
      | Ast.ModuleTypeExpr.Error _
      | Ast.ModuleTypeExpr.Unknown _ -> true
    )
  | Ast.ModuleDeclaration.Unsupported _ -> not (node_has_token_kind decl Kind.PERCENT)

and render_module_declaration = fun state decl ->
  let body = Ast.ModuleDeclaration.body decl in
  if
    module_declaration_has_ascribed_body decl
    || module_declaration_has_head_parameter decl
    || module_declaration_body_requires_member_render decl body
  then
    render_module_declaration_members state decl
  else
    (
      emit_text state "module";
      (
        match Ast.ModuleDeclaration.rec_token decl with
        | Some token ->
            emit_space state;
            emit_token state token
        | None -> ()
      );
      emit_space state;
      (
        match Ast.ModuleDeclaration.name decl with
        | Some name -> emit_token state name
        | None -> unsupported_node "module declaration without name" decl
      );
      match body with
      | Ast.ModuleDeclaration.Expr { body } -> (
          match Ast.ModuleExpr.view body with
          | Ast.ModuleExpr.Path { path } ->
              render_module_declaration_separator state decl ~default:"=";
              render_module_body_path state (Ast.Path.for_each_ident path);
              render_wrapped_module_body_attribute_suffix state decl
          | Ast.ModuleExpr.Structure _ ->
              let items = collect_structure_items_from_module_decl decl in
              if Vector.is_empty items then (
                emit_space state;
                emit_text state "=";
                emit_space state;
                emit_text state "struct";
                emit_space state;
                emit_text state "end"
              ) else (
                let end_token = Ast.ModuleDeclaration.end_token decl in
                let has_terminal_trivia = token_has_leading_comment end_token in
                emit_space state;
                emit_text state "=";
                emit_space state;
                emit_text state "struct";
                emit_line state;
                with_indent
                  state
                  2
                  (fun () ->
                    render_structure_items state items;
                    match end_token with
                    | Some token when has_terminal_trivia ->
                        emit_line state;
                        emit_line state;
                        emit_token_leading_comments_as_lines state token
                    | _ -> ());
                if not has_terminal_trivia then
                  emit_line state;
                emit_text state "end"
              );
              render_wrapped_module_body_attribute_suffix state decl
          | Ast.ModuleExpr.Apply _ ->
              render_module_declaration_separator state decl ~default:"=";
              render_module_expr_node state body;
              render_wrapped_module_body_attribute_suffix state decl
          | Ast.ModuleExpr.Opaque _ ->
              render_shell_body_after_separator
                state
                decl
                Kind.EQ
                ~label:"module extension declaration without '='"
          | Ast.ModuleExpr.Functor _
          | Ast.ModuleExpr.Constraint _
          | Ast.ModuleExpr.Error _
          | Ast.ModuleExpr.Unknown _ -> render_module_declaration_members state decl
        )
      | Ast.ModuleDeclaration.Type { body } -> (
          match Ast.ModuleTypeExpr.view body with
          | Ast.ModuleTypeExpr.Path { path } ->
              render_module_declaration_separator state decl ~default:":";
              render_module_body_path state (Ast.Path.for_each_ident path);
              render_wrapped_module_body_attribute_suffix state decl
          | Ast.ModuleTypeExpr.Signature _ ->
              render_module_declaration_separator state decl ~default:":";
              let items = collect_signature_items_from_module_decl decl in
              render_signature_body state items ~end_token:(Ast.ModuleDeclaration.end_token decl);
              render_wrapped_module_body_attribute_suffix state decl
          | Ast.ModuleTypeExpr.Typeof _ -> render_module_typeof_body state decl
          | Ast.ModuleTypeExpr.With _
          | Ast.ModuleTypeExpr.Functor _
          | Ast.ModuleTypeExpr.Error _
          | Ast.ModuleTypeExpr.Unknown _ -> render_module_declaration_members state decl
        )
      | Ast.ModuleDeclaration.Unsupported _ ->
          render_shell_body_after_separator
            state
            decl
            Kind.EQ
            ~label:"module extension declaration without '='"
    )

and render_empty_struct_module_declaration_multiline = fun state decl ->
  emit_text state "module";
  (
    match Ast.ModuleDeclaration.rec_token decl with
    | Some token ->
        emit_space state;
        emit_token state token
    | None -> ()
  );
  emit_space state;
  (
    match Ast.ModuleDeclaration.name decl with
    | Some name -> emit_token state name
    | None -> unsupported_node "module declaration without name" decl
  );
  emit_space state;
  emit_text state "=";
  emit_space state;
  emit_text state "struct";
  emit_line state;
  emit_line state;
  emit_text state "end";
  render_wrapped_module_body_attribute_suffix state decl

and render_module_type_declaration = fun state decl ->
  emit_text state "module type";
  emit_space state;
  (
    match Ast.ModuleTypeDeclaration.name decl with
    | Some name -> emit_token state name
    | None -> unsupported_node "module type declaration without name" decl
  );
  (
    match Ast.ModuleTypeDeclaration.body decl with
    | Abstract -> ()
    | Manifest { body } -> (
        match Ast.ModuleTypeExpr.view body with
        | Path { path } ->
            render_module_type_declaration_equals state decl;
            render_module_body_path state (Ast.Path.for_each_ident path);
            render_wrapped_module_body_attribute_suffix state decl
        | Signature _ ->
            render_module_type_declaration_equals state decl;
            let items = collect_signature_items_from_module_type_decl decl in
            render_signature_body state items ~end_token:(Ast.ModuleTypeDeclaration.end_token decl);
            render_wrapped_module_body_attribute_suffix state decl
        | With { base; constraints; _ } ->
            render_module_type_declaration_equals state decl;
            (
              match base with
              | Some base -> render_module_type_node state base
              | None -> unsupported_node "constrained module type declaration without base" decl
            );
            let length = Vector.length constraints in
            let rec loop index =
              if Int.(index < length) then (
                emit_space state;
                if Int.equal index 0 then
                  emit_text state "with"
                else
                  emit_text state "and";
                emit_space state;
                render_module_type_constraint_node state (Vector.get_unchecked constraints ~at:index);
                loop (Int.add index 1)
              )
            in
            loop 0;
            render_wrapped_module_body_attribute_suffix state decl
        | Typeof _
        | Functor _
        | Error _
        | Unknown _ ->
            if node_has_token_kind decl Kind.PERCENT then
              render_shell_body_after_separator
                state
                decl
                Kind.EQ
                ~label:"module type extension declaration without '='"
            else
              unsupported_node "unsupported module type declaration body" decl
      )
    | Unsupported _ ->
        if node_has_token_kind decl Kind.PERCENT then
          render_shell_body_after_separator
            state
            decl
            Kind.EQ
            ~label:"module type extension declaration without '='"
        else
          unsupported_node "unsupported module type declaration body" decl
  )

and render_structure_items = fun state items ->
  let length = Vector.length items in
  let rec render_suffix_attributes index =
    if Int.(index < length) then
      let item = Vector.get_unchecked items ~at:index in
      if structure_item_is_attribute item then (
        emit_space state;
        render_attribute_item_suffix state item;
        render_suffix_attributes (Int.add index 1)
      ) else
        index
    else
      index
  in
  let emit_separator item next_item =
    if structure_items_join_tightly item next_item then
      emit_line state
    else (
      emit_line state;
      emit_line state
    )
  in
  let rec loop index =
    if Int.(index < length) then
      (
        let item = Vector.get_unchecked items ~at:index in
        render_structure_item state item;
        if Int.(index < Int.sub length 1) then
          if
            structure_item_is_attribute (Vector.get_unchecked items ~at:(Int.add index 1))
            && not (structure_item_is_extension item)
          then
            if structure_item_is_expr item then (
              emit_text state ";;";
              emit_line state;
              emit_line state;
              loop (Int.add index 1)
            ) else
              (
                let next_index = render_suffix_attributes (Int.add index 1) in
                if Int.(next_index < length) then
                  emit_separator item (Vector.get_unchecked items ~at:next_index);
                loop next_index
              )
          else (
            emit_separator item (Vector.get_unchecked items ~at:(Int.add index 1));
            loop (Int.add index 1)
          )
        else
          loop (Int.add index 1)
      )
  in
  loop 0

and render_structure_item_entries = fun state entries ->
  let length = Vector.length entries in
  let rec render_suffix_attributes index =
    if Int.(index < length) then
      let (item, _) = Vector.get_unchecked entries ~at:index in
      if structure_item_is_attribute item then (
        emit_space state;
        render_attribute_item_suffix state item;
        render_suffix_attributes (Int.add index 1)
      ) else
        index
    else
      index
  in
  let emit_separator item next_item =
    if structure_items_join_tightly item next_item then
      emit_line state
    else (
      emit_line state;
      emit_line state
    )
  in
  let rec loop index =
    if Int.(index < length) then
      (
        let (item, trailing_tokens) = Vector.get_unchecked entries ~at:index in
        render_structure_item_with_trailing_tokens state item trailing_tokens;
        if Int.(Vector.length trailing_tokens > 0) then
          emit_token_vector_compact state trailing_tokens;
        if Int.(index < Int.sub length 1) then
          (
            let (next_item, _) = Vector.get_unchecked entries ~at:(Int.add index 1) in
            if structure_item_is_attribute next_item && not (structure_item_is_extension item) then
              if structure_item_is_expr item then (
                if Int.equal (Vector.length trailing_tokens) 0 then
                  emit_text state ";;";
                emit_line state;
                emit_line state;
                loop (Int.add index 1)
              ) else if Int.equal (Vector.length trailing_tokens) 0 then
                (
                  let next_index = render_suffix_attributes (Int.add index 1) in
                  if Int.(next_index < length) then
                    let (next_item, _) = Vector.get_unchecked entries ~at:next_index in
                    emit_separator item next_item;
                  loop next_index
                )
              else (
                emit_separator item next_item;
                loop (Int.add index 1)
              )
            else (
              emit_separator item next_item;
              loop (Int.add index 1)
            )
          );
        if Int.equal index (Int.sub length 1) then
          loop (Int.add index 1)
      )
  in
  loop 0

and render_structure_item_with_trailing_tokens = fun state item trailing_tokens ->
  if Int.equal (Vector.length trailing_tokens) 0 then
    render_structure_item state item
  else
    match Ast.StructureItem.view item with
    | Let decl -> render_let_declaration_with_body_break state decl true
    | Module decl when module_declaration_has_empty_struct_body decl ->
        render_empty_struct_module_declaration_multiline state decl
    | _ -> render_structure_item state item

and render_signature_items = fun state items ->
  let length = Vector.length items in
  let rec loop index =
    if Int.(index < length) then
      (
        let item = Vector.get_unchecked items ~at:index in
        let drop_initial_docstring =
          if Int.equal index 0 then
            false
          else
            signature_item_should_drop_initial_docstring
              (Vector.get_unchecked items ~at:(Int.sub index 1))
              item
        in
        let compact_final_section_docstring =
          if Int.equal index 0 then
            false
          else
            signature_item_should_compact_leading_section
              (Vector.get_unchecked items ~at:(Int.sub index 1))
              item
        in
        render_signature_item ~drop_initial_docstring ~compact_final_section_docstring state item;
        if Int.(index < Int.sub length 1) then
          let next_item = Vector.get_unchecked items ~at:(Int.add index 1) in
          (
            match Ast.Node.first_descendant_token next_item with
            | Some token -> (
                match token_inline_leading_comment_text token with
                | Some comment ->
                    emit_space state;
                    emit_text state comment;
                    state.suppress_leading_token <- Some token.Ast.id
                | None -> ()
              )
            | None -> ()
          );
        if signature_item_is_attribute next_item && not (signature_item_is_extension item) then
          emit_space state
        else if
          signature_items_join_tightly item next_item
          || signature_item_should_compact_leading_section item next_item
        then
          emit_line state
        else (
          emit_line state;
          emit_line state
        );
        loop (Int.add index 1)
      )
  in
  loop 0

and render_attribute_item = fun state item -> emit_attribute_token_stream state item

and render_extension_item = fun state item ->
  emit_shell_token_stream
    state
    (fun ~fn -> Ast.ExtensionItem.for_each_shell_token item ~fn)

and render_attribute_item_suffix = fun state item ->
  (
    match Ast.Node.first_descendant_token item with
    | Some token -> state.suppress_leading_token <- Some token.Ast.id
    | None -> ()
  );
  render_attribute_item state item

and collect_item_attribute_suffixes = fun item declaration ->
  let attributes = Vector.with_capacity ~size:(Ast.Node.child_count item) in
  let after_declaration = ref false in
  Ast.Node.for_each_child_node
    item
    ~fn:(fun child ->
      if !after_declaration then
        (
          if node_kind_is child Kind.ATTRIBUTE_ITEM then
            Vector.push attributes ~value:child
        )
      else if same_ast_node child declaration then
        after_declaration := true);
  attributes

and render_item_attribute_suffixes = fun state attributes ->
  let length = Vector.length attributes in
  let rec loop index =
    if Int.(index < length) then (
      emit_space state;
      render_attribute_item state (Vector.get_unchecked attributes ~at:index);
      loop (Int.add index 1)
    )
  in
  loop 0

and signature_item_is_attribute = fun item ->
  match Ast.SignatureItem.view item with
  | Attribute _ -> true
  | _ -> false

and signature_item_is_extension = fun item ->
  match Ast.SignatureItem.view item with
  | Extension _ -> true
  | _ -> false

and structure_item_is_attribute = fun item ->
  match Ast.StructureItem.view item with
  | Attribute _ -> true
  | _ -> false

and structure_item_is_extension = fun item ->
  match Ast.StructureItem.view item with
  | Extension _ -> true
  | _ -> false

and structure_item_is_expr = fun item ->
  match Ast.StructureItem.view item with
  | Expr _ -> true
  | _ -> false

and signature_item_is_open = fun item ->
  match Ast.SignatureItem.view item with
  | Open _ -> true
  | _ -> false

and signature_item_is_type = fun item ->
  match Ast.SignatureItem.view item with
  | Type _ -> true
  | _ -> false

and signature_item_is_value = fun item ->
  match Ast.SignatureItem.view item with
  | Value _ -> true
  | _ -> false

and signature_item_is_variant_type = fun item ->
  match Ast.SignatureItem.view item with
  | Type (Ast.TypeDeclarationItem decl) ->
      let members = collect_type_members decl in
      let length = Vector.length members in
      let rec loop index =
        if Int.(index >= length) then
          false
        else
          match Ast.TypeDeclaration.Member.variant_type (Vector.get_unchecked members ~at:index) with
          | Some _ -> true
          | None -> loop (Int.add index 1)
      in
      loop 0
  | _ -> false

and signature_item_leading_docstring_count = fun item ->
  match Ast.Node.first_descendant_token item with
  | Some token ->
      let count = ref 0 in
      Ast.Token.for_each_leading_trivia_item
        token
        ~fn:(
          function
          | Ast.Token.Docstring _ -> count := Int.add !count 1
          | Ast.Token.Comment _
          | Ast.Token.Whitespace -> ()
        );
      !count
  | None -> 0

and signature_item_first_leading_docstring_is_section = fun item ->
  match Ast.Node.first_descendant_token item with
  | Some token ->
      let seen_docstring = ref false in
      let first_is_section = ref false in
      Ast.Token.for_each_leading_trivia_item
        token
        ~fn:(
          function
          | Ast.Token.Docstring docstring ->
              if not !seen_docstring then (
                seen_docstring := true;
                if docstring_is_section docstring then
                  first_is_section := true
              )
          | Ast.Token.Comment _
          | Ast.Token.Whitespace -> ()
        );
      !first_is_section
  | None -> false

and token_first_leading_docstring_is_indented = fun token ->
  let text = Ast.Token.leading_text token in
  let length = String.length text in
  let opener_at index =
    if Int.(index + 2 >= length) then
      false
    else if Char.equal (String.get_unchecked text ~at:index) '(' then
      if Char.equal (String.get_unchecked text ~at:(Int.add index 1)) '*' then
        Char.equal (String.get_unchecked text ~at:(Int.add index 2)) '*'
      else
        false
    else
      false
  in
  let rec indentation_before index count =
    if Int.(index < 0) then
      (false, count)
    else
      match String.get_unchecked text ~at:index with
      | '\n' -> (true, count)
      | '\r' -> indentation_before (Int.sub index 1) count
      | ' '
      | '\t' -> indentation_before (Int.sub index 1) (Int.add count 1)
      | _ -> (false, count)
  in
  let rec loop index =
    if Int.(index >= length) then
      false
    else if opener_at index then
      let (saw_line_start, indent) = indentation_before (Int.sub index 1) 0 in
      saw_line_start && Int.(indent > 0)
    else
      loop (Int.add index 1)
  in
  loop 0

and signature_item_first_leading_docstring_is_indented = fun item ->
  match Ast.Node.first_descendant_token item with
  | Some token -> token_first_leading_docstring_is_indented token
  | None -> false

and structure_item_is_open = fun item ->
  match Ast.StructureItem.view item with
  | Open _ -> true
  | _ -> false

and structure_item_is_module_path_alias = fun item ->
  match Ast.StructureItem.view item with
  | Module decl -> (
      match Ast.ModuleDeclaration.body decl with
      | Ast.ModuleDeclaration.Expr { body } -> (
          match Ast.ModuleExpr.view body with
          | Ast.ModuleExpr.Path _ -> not (module_declaration_has_ascribed_body decl)
          | _ -> false
        )
      | _ -> false
    )
  | _ -> false

and signature_items_join_tightly = fun left right ->
  (signature_item_is_open left && signature_item_is_open right)
  || (signature_item_is_type left
  && signature_item_is_type right
  && (Int.(signature_item_leading_docstring_count right <= 1)
  || signature_item_should_drop_initial_docstring left right))
  || (signature_item_is_type left
  && signature_item_is_value right
  && not (node_has_leading_comment right))

and structure_items_join_tightly = fun left right ->
  (structure_item_is_open left && structure_item_is_open right)
  || (structure_item_is_module_path_alias left
  && structure_item_is_module_path_alias right
  && not (node_has_leading_comment right))

and signature_item_should_drop_initial_docstring = fun previous item ->
  signature_item_is_variant_type previous && signature_item_first_leading_docstring_is_indented item

and signature_item_should_compact_leading_section = fun previous item ->
  signature_item_is_type previous
  && (not (signature_item_is_type item))
  && signature_item_first_leading_docstring_is_section item

and render_signature_item = fun
  ?(drop_initial_docstring = false)
  ?(compact_final_section_docstring = false)
  state
  item ->
  emit_top_level_leading ~drop_initial_docstring ~compact_final_section_docstring state item;
  let attribute_suffixes =
    match Ast.SignatureItem.declaration item with
    | Some declaration -> collect_item_attribute_suffixes item declaration
    | None -> Vector.with_capacity ~size:0
  in
  (
    match Ast.SignatureItem.view item with
    | Value decl -> render_value_declaration state decl
    | Type (Ast.TypeDeclarationItem decl) -> render_type_declaration state decl
    | Type (Ast.TypeExtensionItem decl) -> render_type_extension_declaration state decl
    | Module decl -> render_module_declaration state decl
    | ModuleType decl -> render_module_type_declaration state decl
    | Open decl -> render_open_declaration state decl
    | Include decl -> render_include_declaration state decl
    | External decl -> render_external_declaration state decl
    | Exception decl -> render_exception_declaration state decl
    | Attribute item -> render_attribute_item state item
    | Extension item -> render_extension_item state item
    | Error _
    | Unknown _ -> unsupported_node "unsupported signature item" item
  );
  render_item_attribute_suffixes state attribute_suffixes

and render_structure_item = fun state item ->
  emit_top_level_leading state item;
  let attribute_suffixes =
    match Ast.StructureItem.declaration item with
    | Some declaration -> collect_item_attribute_suffixes item declaration
    | None -> Vector.with_capacity ~size:0
  in
  (
    match Ast.StructureItem.view item with
    | Open decl -> render_open_declaration state decl
    | Type (Ast.TypeDeclarationItem decl) -> render_type_declaration state decl
    | Let decl -> render_let_declaration state decl
    | Module decl -> render_module_declaration state decl
    | ModuleType decl -> render_module_type_declaration state decl
    | Include decl -> render_include_declaration state decl
    | External decl -> render_external_declaration state decl
    | Exception decl -> render_exception_declaration state decl
    | Type (Ast.TypeExtensionItem decl) -> render_type_extension_declaration state decl
    | Attribute item -> render_attribute_item state item
    | Expr expr_item -> render_expr_item state expr_item
    | Extension item -> render_extension_item state item
    | Error _
    | Unknown _ -> unsupported_node "unsupported structure item" item
  );
  render_item_attribute_suffixes state attribute_suffixes

and render_expr_item = fun state item ->
  match Ast.ExprItem.expr item with
  | Some expr ->
      render_expr state expr;
      let trailing_tokens = collect_direct_tokens_after_child item expr in
      if Int.(Vector.length trailing_tokens > 0) then
        emit_token_vector_compact state trailing_tokens
  | None -> unsupported_node "expr item without expression" item

let render_interface = fun state interface ->
  render_signature_items
    state
    (collect_signature_items_from_node interface)

let render_implementation = fun state implementation ->
  render_structure_item_entries
    state
    (collect_structure_item_entries_from_node implementation)

let render_source_file = fun state source_file ->
  match Ast.SourceFile.view source_file with
  | Implementation implementation -> render_implementation state implementation
  | Interface interface -> render_interface state interface

let emit_source_file_trailing = fun state source_file ->
  match Ast.Node.first_child_token source_file ~kind:Kind.EOF with
  | Some token when Ast.Token.has_leading_comment token ->
      if state.wrote && not state.line_start then
        emit_line state;
      if token_has_leading_docstring (Some token) then
        emit_line state;
      emit_token_leading_comments_as_lines state token
  | Some _
  | None -> ()

let write = fun ~writer ?(width = 100) ?(buffer_size = 4_096) source_file ->
  let sink = { writer; flush_threshold = Int.max 1 buffer_size; error = None } in
  let state = {
    buffer = IO.Buffer.create ~size:(Int.max 0 buffer_size);
    sink;
    width;
    line_start = true;
    column = 0;
    indent = 0;
    pending_spaces = 0;
    wrote = false;
    line_count = 0;
    suppress_leading_token = None;
    last_leading_comment_kind = None;
    suppress_list_item_leading_comments = false;
    delimited_expr_depth = 0;
  }
  in
  try
    render_source_file state source_file;
    emit_source_file_trailing state source_file;
    if state.wrote && not state.line_start then
      emit_line state;
    match flush state with
    | Ok () -> (
        match sink.error with
        | Some error -> Error (Cannot_write error)
        | None ->
            IO.flush writer
            |> Result.map_err ~fn:(fun error -> Cannot_write error)
      )
    | Error error -> Error (Cannot_write error)
  with
  | Unsupported err -> Error (Cannot_format err)
