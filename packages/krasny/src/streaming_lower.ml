open Std
open Std.Collections

module Ast = Syn.Ast

module Kind = Syn.SyntaxKind

module Slice = IO.IoVec.IoSlice

type error = { message: string }

type write_error =
  | Cannot_format of error
  | Cannot_write of IO.error

exception Unsupported of error

let error_to_string = fun err -> err.message

let unsupported = fun message -> raise (Unsupported { message })

type sink = { writer: IO.Writer.t; flush_threshold: int; mutable error: IO.error option }

type state = {
  buffer: IO.Buffer.t;
  sink: sink;
  width: int;
  mutable line_start: bool;
  mutable column: int;
  mutable indent: int;
  mutable pending_spaces: int;
  mutable wrote: bool;
  mutable suppress_leading_token: int option;
  mutable delimited_expr_depth: int;
}

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Streaming_lower.append_subslice: " ^ IO.IoVec.error_message error)

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

let last_line_width = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index < 0) then
      length
    else
      if Char.equal (String.get_unchecked text ~at:index) '\n' then
        Int.sub (Int.sub length index) 1
      else loop (Int.sub index 1)
  in
  loop (Int.sub length 1)

let last_slice_line_width = fun slice ->
  let rec find_last_newline index last_newline =
    if Int.(index >= Slice.length slice) then
      last_newline
    else
      if Char.equal (Slice.get_unchecked slice ~at:index) '\n' then
        find_last_newline Int.(index + 1) index
      else find_last_newline Int.(index + 1) last_newline
  in
  let last_newline = find_last_newline 0 (-1) in
  if Int.(last_newline < 0) then
    Slice.length slice
  else Int.(Slice.length slice - last_newline - 1)

let write_indent = fun state ->
  let rec loop remaining =
    if Int.(remaining > 0) then
      (
        IO.Buffer.add_char state.buffer ' ';
        loop (Int.sub remaining 1)
      )
  in
  loop state.indent

let write_pending_spaces = fun state ->
  let count = state.pending_spaces in
  if Int.(count > 0) then
    (
      for _ = 1 to count do IO.Buffer.add_char state.buffer ' ' done;
      state.pending_spaces <- 0;
      flush_if_needed state;
      state.wrote <- true
    )

let write_string_segment = fun state value ~off ~len ->
  if Int.(len > 0) then
    (
      if state.line_start then
        (
          state.pending_spaces <- 0;
          write_indent state
        )
      else write_pending_spaces state;
      IO.Buffer.add_substring state.buffer value off len;
      flush_if_needed state;
      state.wrote <- true;
      state.line_start <- false
    )

let write_slice_segment = fun state value ~off ~len ->
  if Int.(len > 0) then
    (
      if state.line_start then
        (
          state.pending_spaces <- 0;
          write_indent state
        )
      else write_pending_spaces state;
      append_subslice_unchecked state.buffer value ~off ~len;
      flush_if_needed state;
      state.wrote <- true;
      state.line_start <- false
    )

let is_horizontal_whitespace = fun char -> Char.equal char ' ' || Char.equal char '\t'

let trim_whitespace_only_segment = fun value ~start ~stop ->
  let rec loop index =
    if Int.(index >= stop) then
      start
    else
      if is_horizontal_whitespace (String.get_unchecked value ~at:index) then
        loop (Int.add index 1)
      else stop
  in
  loop start

let emit_line = fun state ->
  state.pending_spaces <- 0;
  IO.Buffer.add_char state.buffer '\n';
  flush_if_needed state;
  state.wrote <- true;
  state.line_start <- true;
  state.column <- state.indent

let emit_text = fun state value ->
  let length = String.length value in
  if Int.(length > 0) then
    let rec loop segment_start index saw_newline =
      if Int.(index >= length) then
        (
          write_string_segment state value ~off:segment_start ~len:Int.(length - segment_start);
          state.column <- if saw_newline then
            Int.sub length segment_start
          else state.column + length
        )
      else
        if Char.equal (String.get_unchecked value ~at:index) '\n' then
          (
            let segment_end = trim_whitespace_only_segment value ~start:segment_start ~stop:index in
            write_string_segment state value ~off:segment_start ~len:Int.(segment_end - segment_start);
            state.pending_spaces <- 0;
            IO.Buffer.add_char state.buffer '\n';
            flush_if_needed state;
            state.wrote <- true;
            state.line_start <- true;
            loop Int.(index + 1) Int.(index + 1) true
          )
        else loop segment_start Int.(index + 1) saw_newline
    in
    loop 0 0 false

let count_newlines = fun text ->
  let length = String.length text in
  let rec loop index count =
    if Int.(index >= length) then
      count
    else
      if Char.equal (String.get_unchecked text ~at:index) '\n' then
        loop (Int.add index 1) (Int.add count 1)
      else loop (Int.add index 1) count
  in
  loop 0 0

let split_lines = fun text ->
  let length = String.length text in
  let lines = Vector.with_capacity ~size:(Int.add (count_newlines text) 1) in
  let rec loop segment_start index =
    if Int.(index >= length) then
      Vector.push lines ~value:(String.sub text ~offset:segment_start ~len:(Int.sub length segment_start))
    else
      if Char.equal (String.get_unchecked text ~at:index) '\n' then
        (
          Vector.push lines ~value:(String.sub text ~offset:segment_start ~len:(Int.sub index segment_start));
          loop (Int.add index 1) (Int.add index 1)
        )
      else loop segment_start (Int.add index 1)
  in
  loop 0 0;
  lines

let is_blank_line = fun line -> String.is_empty (String.trim line)

let leading_horizontal_width = fun line ->
  let length = String.length line in
  let rec loop index =
    if Int.(index >= length) then
      length
    else
      if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
        loop (Int.add index 1)
      else index
  in
  loop 0

let trim_trailing_horizontal = fun line ->
  let length = String.length line in
  let rec loop index =
    if Int.(index < 0) then
      0
    else
      if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
        loop (Int.sub index 1)
      else Int.add index 1
  in
  String.sub line ~offset:0 ~len:(loop (Int.sub length 1))

let strip_leading_width = fun line width ->
  let length = String.length line in
  let rec loop index remaining =
    if Int.(remaining <= 0 || index >= length) then
      index
    else
      if is_horizontal_whitespace (String.get_unchecked line ~at:index) then
        loop (Int.add index 1) (Int.sub remaining 1)
      else index
  in
  let offset = loop 0 width in String.sub line ~offset ~len:(Int.sub length offset)

let first_nonblank_line_index = fun lines ->
  let length = Vector.length lines in
  let rec loop index =
    if Int.(index >= length) then
      None
    else
      if is_blank_line (Vector.get_unchecked lines ~at:index) then
        loop (Int.add index 1)
      else Some index
  in
  loop 0

let last_nonblank_line_index = fun lines ->
  let rec loop index =
    if Int.(index < 0) then
      None
    else
      if is_blank_line (Vector.get_unchecked lines ~at:index) then
        loop (Int.sub index 1)
      else Some index
  in
  loop (Int.sub (Vector.length lines) 1)

let common_docstring_indent = fun lines ~start ~stop ->
  let rec loop index current =
    if Int.(index > stop) then
      current
    else
      let line = Vector.get_unchecked lines ~at:index in
      if is_blank_line line then
        loop (Int.add index 1) current
      else
        let indent = leading_horizontal_width line in
        let current =
          match current with
          | None -> Some indent
          | Some current -> Some (Int.min current indent)
        in
        loop (Int.add index 1) current
  in
  match loop start None with
  | Some indent -> indent
  | None -> 0

let delimited_trivia_closing = fun (trivia: Ast.Token.delimited_trivia) ->
  match trivia.closing with
  | Some closing -> closing
  | None -> ""

let normalize_inline_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  let body = String.trim docstring.content in
  let closing = delimited_trivia_closing docstring in
  if String.is_empty body then
    docstring.opening ^ " " ^ closing
  else docstring.opening ^ " " ^ body ^ " " ^ closing

let normalize_multiline_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  let lines = split_lines docstring.content in
  let line_count = Vector.length lines in
  if Int.(line_count <= 1) then
    normalize_inline_docstring docstring
  else
    match first_nonblank_line_index lines, last_nonblank_line_index lines with
    | (None, _) | (_, None) -> docstring.opening ^ "\n" ^ delimited_trivia_closing docstring
    | Some first_index, Some last_index ->
        let first_is_inline = Int.equal first_index 0 in
        let indent_start =
          if first_is_inline then
            Int.add first_index 1
          else first_index
        in
        let common_indent =
          if Int.(indent_start > last_index) then
            0
          else common_docstring_indent lines ~start:indent_start ~stop:last_index
        in
        let buffer = IO.Buffer.create ~size:(Int.add (String.length docstring.text) 8) in
        let add_body_line index =
          let raw_line = Vector.get_unchecked lines ~at:index in
          let line =
            trim_trailing_horizontal
              (
                if first_is_inline && Int.equal index first_index then
                  String.trim raw_line
                else strip_leading_width raw_line common_indent
              )
          in
          if String.is_empty line then
            IO.Buffer.add_char buffer '\n'
          else
            (
              IO.Buffer.add_string buffer "   ";
              IO.Buffer.add_string buffer line;
              IO.Buffer.add_char buffer '\n'
            )
        in
        IO.Buffer.add_string buffer docstring.opening;
        IO.Buffer.add_char buffer '\n';
        for index = first_index to last_index do add_body_line index done;
        IO.Buffer.add_string buffer (delimited_trivia_closing docstring);
        IO.Buffer.contents buffer

let normalize_docstring = fun (docstring: Ast.Token.delimited_trivia) ->
  if String.contains docstring.content "\n" then
    normalize_multiline_docstring docstring
  else normalize_inline_docstring docstring

let normalize_inline_comment = fun (comment: Ast.Token.delimited_trivia) ->
  let body = String.trim comment.content in
  let closing = delimited_trivia_closing comment in
  if String.is_empty body then
    comment.opening ^ " " ^ closing
  else comment.opening ^ " " ^ body ^ " " ^ closing

let normalize_multiline_comment = fun (comment: Ast.Token.delimited_trivia) ->
  let lines = split_lines comment.content in
  match first_nonblank_line_index lines, last_nonblank_line_index lines with
  | (None, _) | (_, None) -> comment.opening ^ "\n" ^ delimited_trivia_closing comment
  | Some first_index, Some last_index ->
      let first_is_inline = Int.equal first_index 0 in
      let body_start =
        if first_is_inline then
          Int.add first_index 1
        else first_index
      in
      let common_indent =
        if Int.(body_start > last_index) then
          0
        else common_docstring_indent lines ~start:body_start ~stop:last_index
      in
      let buffer = IO.Buffer.create ~size:(Int.add (String.length comment.text) 8) in
      IO.Buffer.add_string buffer comment.opening;
      if first_is_inline then
        (
          let first_line = Vector.get_unchecked lines ~at:first_index |> String.trim in
          if not (String.is_empty first_line) then
            (
              IO.Buffer.add_char buffer ' ';
              IO.Buffer.add_string buffer first_line
            )
        );
      IO.Buffer.add_char buffer '\n';
      for index = body_start to last_index do
        let raw_line = Vector.get_unchecked lines ~at:index in
        let line = strip_leading_width raw_line common_indent |> trim_trailing_horizontal in
        if String.is_empty line then
          IO.Buffer.add_char buffer '\n'
        else
          (
            IO.Buffer.add_string buffer "   ";
            IO.Buffer.add_string buffer line;
            IO.Buffer.add_char buffer '\n'
          )
      done;
      IO.Buffer.add_string buffer (delimited_trivia_closing comment);
      IO.Buffer.contents buffer

let normalize_comment = fun (comment: Ast.Token.delimited_trivia) ->
  if String.contains comment.content "\n" then
    normalize_multiline_comment comment
  else normalize_inline_comment comment

let emit_slice = fun state ~has_newline value ->
  let length = Slice.length value in
  if Int.(length > 0) then
    if has_newline then
      (
        if state.line_start then
          (
            state.pending_spaces <- 0;
            write_indent state
          )
        else write_pending_spaces state;
        append_subslice_unchecked state.buffer value ~off:0 ~len:length;
        flush_if_needed state;
        state.wrote <- true;
        state.line_start <- Char.equal (Slice.get_unchecked value ~at:Int.(length - 1)) '\n';
        state.column <- last_slice_line_width value
      )
    else
      (
        write_slice_segment state value ~off:0 ~len:length;
        state.column <- state.column + length
      )

let emit_space = fun state ->
  if state.line_start then
    state.column <- state.column + 1
  else
    (
      state.pending_spaces <- Int.add state.pending_spaces 1;
      state.column <- state.column + 1
    )

let emit_spaces = fun state count ->
  let rec loop remaining =
    if Int.(remaining > 0) then
      (
        emit_space state;
        loop (Int.sub remaining 1)
      )
  in
  loop count

let with_indent = fun state extra fn ->
  let previous = state.indent in
  state.indent <- Int.add previous extra;
  fn ();
  state.indent <- previous

let with_delimited_expr = fun state fn ->
  let previous = state.delimited_expr_depth in
  state.delimited_expr_depth <- Int.succ previous;
  fn ();
  state.delimited_expr_depth <- previous

let emit_token_leading_comments_as_lines = fun state token ->
  let emitted = ref false in
  Ast.Token.for_each_leading_trivia_item token ~fn:(
    function
    | Ast.Token.Comment comment ->
        if !emitted then
          emit_line state;
        emit_text state (normalize_comment comment);
        emitted := true
    | Ast.Token.Docstring docstring ->
        if !emitted then
          emit_line state;
        emit_text state (normalize_docstring docstring);
        emitted := true
    | Ast.Token.Whitespace -> ()
  );
  if !emitted then
    (
      emit_line state;
      state.suppress_leading_token <- Some token.Ast.id
    )

let emit_leading_trivia_before_token = fun state token ->
  match state.suppress_leading_token with
  | Some id when Int.equal id token.Ast.id -> state.suppress_leading_token <- None
  | _ ->
      if Ast.Token.has_leading_comment token then
        (
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

let emit_optional_token = fun state token ->
  match token with
  | Some token -> emit_token state token
  | None -> ()

let emit_keyword = emit_text

let emit_node_keyword = fun state node ~kind ~fallback ->
  match Ast.Node.first_child_token node ~kind with
  | Some token -> emit_token state token
  | None -> emit_keyword state fallback

let emit_top_level_leading = fun state node ->
  match Ast.Node.first_descendant_token node with
  | Some token ->
      if Ast.Token.has_leading_comment token then
        emit_token_leading_comments_as_lines state token
  | None -> ()

let emit_node_leading_comments_as_lines = fun state node ->
  match Ast.Node.first_descendant_token node with
  | Some token when Ast.Token.has_leading_comment token -> emit_token_leading_comments_as_lines state token
  | Some _ | None -> ()

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

let same_ast_node = fun (left: Ast.Node.t) (right: Ast.Node.t) -> Int.equal left.Ast.id right.Ast.id

let is_module_expr_kind = function
  | Kind.MODULE_EXPR | Kind.PATH_MODULE_EXPR | Kind.STRUCT_MODULE_EXPR | Kind.FUNCTOR_MODULE_EXPR | Kind.APPLY_MODULE_EXPR | Kind.CONSTRAINT_MODULE_EXPR | Kind.PAREN_MODULE_EXPR | Kind.OPAQUE_MODULE_EXPR -> true
  | _ -> false

let is_module_type_kind = function
  | Kind.MODULE_TYPE_EXPR | Kind.PATH_MODULE_TYPE | Kind.SIGNATURE_MODULE_TYPE | Kind.TYPEOF_MODULE_TYPE | Kind.FUNCTOR_MODULE_TYPE | Kind.WITH_MODULE_TYPE -> true
  | _ -> false

let first_child_node_matching = fun node ~matches ->
  let found = ref None in
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      match !found with
      | Some _ -> ()
      | None ->
          if matches (node_kind child) then
            found := Some child
  );
  !found

let first_descendant_node_matching = fun (node: Ast.Node.t) ~matches ->
  let found = ref None in
  let rec visit node = Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      match !found with
      | Some _ -> ()
      | None ->
          if matches (node_kind child) then
            found := Some child
          else visit child
  ) in
  visit node;
  !found

let node_child_token_kind_is = fun (node: Ast.Node.t) index kind ->
  match Ast.Node.child_at node index with
  | Some (Syn.SyntaxTree.Token id) -> token_kind_is (({ tree = node.Ast.tree; id } : Ast.Token.t)) kind
  | Some (Syn.SyntaxTree.Node _) | Some (Syn.SyntaxTree.Missing _) | None -> false

let node_child_index = fun (node: Ast.Node.t) (target: Ast.Node.t) ->
  let rec loop index =
    if Int.(index >= Ast.Node.child_count node) then
      None
    else
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Node id) when Int.equal id target.Ast.id -> Some index
      | Some _ | None -> loop (Int.add index 1)
  in
  loop 0

let collect_node_attribute_suffix_tokens_after_child = fun (node: Ast.Node.t) (child: Ast.Node.t) ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match node_child_index node child with
  | None -> tokens
  | Some child_index ->
      let first_suffix_index = Int.add child_index 1 in
      if node_child_token_kind_is node first_suffix_index Kind.LBRACKET && (node_child_token_kind_is node (Int.add first_suffix_index 1) Kind.AT || node_child_token_kind_is node (Int.add first_suffix_index 1) Kind.ATAT) then
        let rec loop index =
          if Int.(index < Ast.Node.child_count node) then
            (
              (
                match Ast.Node.child_at node index with
                | Some (Syn.SyntaxTree.Token id) -> Vector.push tokens ~value:(({ tree = node.Ast.tree; id } : Ast.Token.t))
                | Some (Syn.SyntaxTree.Node id) -> Ast.Node.for_each_token (({ tree = node.Ast.tree; id } : Ast.Node.t)) ~fn:(
                  fun token -> Vector.push tokens ~value:token
                )
                | Some (Syn.SyntaxTree.Missing _) | None -> ()
              );
              loop (Int.add index 1)
            )
        in
        loop first_suffix_index;
        tokens
      else tokens

let collect_wrapped_module_body_attribute_suffix_tokens = fun (node: Ast.Node.t) ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  match first_descendant_node_matching node ~matches:(
    fun kind -> Kind.(kind = MODULE_EXPR || kind = MODULE_TYPE_EXPR)
  ) with
  | None -> tokens
  | Some wrapper -> (
    match first_child_node_matching wrapper ~matches:(
      fun kind -> (is_module_expr_kind kind || is_module_type_kind kind) && not Kind.(kind = MODULE_EXPR || kind = MODULE_TYPE_EXPR)
    ) with
    | None -> tokens
    | Some body -> collect_node_attribute_suffix_tokens_after_child wrapper body
  )

let node_has_token_kind = fun node kind ->
  let found = ref false in
  Ast.Node.for_each_token node ~fn:(
    fun token ->
      if not !found && token_kind_is token kind then
        found := true
  );
  !found

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token node ~fn:(
    fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if token_kind_is token Kind.IDENT then
            found := Some token
  );
  !found

let last_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token node ~fn:(
    fun token ->
      if token_kind_is token Kind.IDENT then
        found := Some token
  );
  !found

let token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in Kind.(previous_kind = LBRACKET && current_kind = PIPE) || not (Kind.(current_kind = RPAREN) || Kind.(current_kind = RBRACKET) || Kind.(current_kind = RBRACE) || Kind.(current_kind = COLON) || Kind.(current_kind = COMMA) || Kind.(current_kind = SEMI) || Kind.(current_kind = DOT) || Kind.(previous_kind = LPAREN) || Kind.(previous_kind = LBRACKET) || Kind.(previous_kind = LBRACE) || Kind.(previous_kind = DOT) || Kind.(previous_kind = BACKTICK) || Kind.(previous_kind = QUOTE) || Kind.(previous_kind = AT) || Kind.(previous_kind = ATAT) || Kind.(previous_kind = TILDE) || Kind.(previous_kind = QUESTION))

let emit_token_stream = fun state node ->
  let previous = ref None in Ast.Node.for_each_token node ~fn:(
    fun token ->
      (
        match !previous with
        | Some previous when token_wants_space_before previous token -> emit_space state
        | Some _ | None -> ()
      );
      emit_token state token;
      previous := Some token
  )

let emit_token_vector_stream = fun state tokens ->
  let previous = ref None in Vector.for_each tokens ~fn:(
    fun token ->
      (
        match !previous with
        | Some previous when token_wants_space_before previous token -> emit_space state
        | Some _ | None -> ()
      );
      emit_token state token;
      previous := Some token
  )

let render_attribute_suffix_tokens = fun state tokens ->
  if Vector.length tokens > 0 then
    (
      emit_space state;
      emit_token_vector_stream state tokens
    )

let render_node_attribute_suffix_after_child = fun state node child -> render_attribute_suffix_tokens state (collect_node_attribute_suffix_tokens_after_child node child)

let render_wrapped_module_body_attribute_suffix = fun state node -> render_attribute_suffix_tokens state (collect_wrapped_module_body_attribute_suffix_tokens node)

let emit_token_vector_range_stream = fun state tokens ~start ~stop ->
  let previous = ref None in
  let rec loop index =
    if Int.(index < stop) then
      (
        let token = Vector.get_unchecked tokens ~at:index in
        (
          match !previous with
          | Some previous when token_wants_space_before previous token -> emit_space state
          | Some _ | None -> ()
        );
        emit_token state token;
        previous := Some token;
        loop (Int.add index 1)
      )
  in
  loop start

let emit_token_vector_range_compact = fun state tokens ~start ~stop ->
  let rec loop index =
    if Int.(index < stop) then
      (
        emit_token state (Vector.get_unchecked tokens ~at:index);
        loop (Int.add index 1)
      )
  in
  loop start

let shell_token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in not (Kind.(current_kind = RBRACKET) || Kind.(current_kind = BAR_RBRACKET) || Kind.(current_kind = DOT) || Kind.(current_kind = COLON) || Kind.(previous_kind = LBRACKET) || Kind.(previous_kind = LBRACKET_BAR) || Kind.(previous_kind = DOT) || Kind.(previous_kind = AT) || Kind.(previous_kind = ATAT) || Kind.(previous_kind = PERCENT) || Kind.(previous_kind = PERCENTGT) || Kind.(previous_kind = LTPERCENT))

let emit_shell_token_stream = fun state for_each_shell_token ->
  let previous = ref None in
  for_each_shell_token ~fn:(
    fun token ->
      (
        match !previous with
        | Some previous when shell_token_wants_space_before previous token -> emit_space state
        | Some _ | None -> ()
      );
      emit_token state token;
      previous := Some token
  );
  match !previous with
  | Some _ -> ()
  | None -> unsupported "attribute or extension without shell tokens"

let unsupported_node = fun label node -> unsupported (label ^ ": " ^ Kind.to_string (node_kind node))

let collect_child_exprs = fun (node: Ast.Node.t) ->
  let exprs = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      match Ast.Expr.cast child with
      | Some expr -> Vector.push exprs ~value:expr
      | None -> ()
  );
  exprs

let collect_child_patterns = fun (node: Ast.Node.t) ->
  let patterns = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      match Ast.Pattern.cast child with
      | Some pattern -> Vector.push patterns ~value:pattern
      | None -> ()
  );
  patterns

let rec collect_child_type_exprs = fun (type_expr: Ast.TypeExpr.t) ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_child_type type_expr ~fn:(
    fun item -> Vector.push items ~value:item
  );
  if Int.equal (Vector.length items) 1 && node_kind_is type_expr Kind.TYPE_EXPR then
    let only_child = Vector.get_unchecked items ~at:0 in
    match Ast.TypeExpr.view only_child with
    | Tuple _ -> collect_child_type_exprs only_child
    | _ -> items
  else items

let collect_record_fields = fun record ->
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record) in
  Ast.RecordExpr.for_each_field record ~fn:(
    fun field -> Vector.push fields ~value:field
  );
  fields

let collect_array_items = collect_child_exprs

let collect_type_members = fun decl ->
  let members = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeDeclaration.for_each_member decl ~fn:(
    fun member -> Vector.push members ~value:member
  );
  members

let collect_type_member_parameters = fun member ->
  let parameters = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  Ast.TypeDeclaration.Member.for_each_parameter member ~fn:(
    fun parameter -> Vector.push parameters ~value:parameter
  );
  parameters

let collect_type_member_attribute_items = fun member ->
  let attributes = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  Ast.TypeDeclaration.Member.for_each_child_node member ~fn:(
    fun child ->
      if node_kind_is child Kind.ATTRIBUTE_ITEM then
        Vector.push attributes ~value:child
  );
  attributes

let type_member_attribute_suffix_start_at = fun member close_index ->
  if not (Ast.TypeDeclaration.Member.child_token_kind_is member close_index Kind.RBRACKET) then
    None
  else
    let rec loop index depth =
      if Int.(index < 0) then
        None
      else
        if Ast.TypeDeclaration.Member.child_token_kind_is member index Kind.RBRACKET then
          loop (Int.sub index 1) (Int.add depth 1)
        else
          if Ast.TypeDeclaration.Member.child_token_kind_is member index Kind.LBRACKET then
            if Int.equal depth 1 then
              let next = Int.add index 1 in
              if Ast.TypeDeclaration.Member.child_token_kind_is member next Kind.AT || Ast.TypeDeclaration.Member.child_token_kind_is member next Kind.ATAT then
                Some index
              else None
            else loop (Int.sub index 1) (Int.sub depth 1)
          else loop (Int.sub index 1) depth
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
  if Int.(first_suffix_index < Ast.TypeDeclaration.Member.child_count member) && Ast.TypeDeclaration.Member.child_token_kind_is member first_suffix_index Kind.LBRACKET then
    Some first_suffix_index
  else None

let collect_type_member_attribute_suffix_tokens = fun member ->
  let tokens = Vector.with_capacity ~size:(Ast.TypeDeclaration.Member.child_count member) in
  match type_member_first_attribute_suffix_child_index member with
  | None -> tokens
  | Some first_suffix_index ->
      let rec loop index =
        if Int.(index < Ast.TypeDeclaration.Member.child_count member) then
          (
            (
              match Ast.TypeDeclaration.Member.child_at member index with
              | Some (Syn.SyntaxTree.Token _) -> (
                match Ast.TypeDeclaration.Member.child_token_at member index with
                | Some token -> Vector.push tokens ~value:token
                | None -> ()
              )
              | Some (Syn.SyntaxTree.Node _) -> (
                match Ast.TypeDeclaration.Member.child_node_at member index with
                | Some node -> Ast.Node.for_each_token node ~fn:(
                  fun token -> Vector.push tokens ~value:token
                )
                | None -> ()
              )
              | Some (Syn.SyntaxTree.Missing _) | None -> ()
            );
            loop (Int.add index 1)
          )
      in
      loop first_suffix_index;
      tokens

let collect_type_declaration_attribute_items = fun decl ->
  let attributes = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.Node.for_each_child_node decl ~fn:(
    fun child ->
      if node_kind_is child Kind.ATTRIBUTE_ITEM then
        Vector.push attributes ~value:child
  );
  attributes

let collect_type_extension_parameters = fun decl ->
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeExtensionDeclaration.for_each_parameter decl ~fn:(
    fun parameter -> Vector.push parameters ~value:parameter
  );
  parameters

let collect_record_type_fields = fun record_type ->
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record_type) in
  Ast.RecordType.for_each_field record_type ~fn:(
    fun field -> Vector.push fields ~value:field
  );
  fields

let collect_let_bindings = fun decl ->
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.LetDeclaration.for_each_binding decl ~fn:(
    fun binding -> Vector.push bindings ~value:binding
  );
  bindings

let collect_let_bindings_from_expr = fun expr ->
  let bindings = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Node.for_each_child_node expr ~fn:(
    fun child ->
      match Ast.LetBinding.cast child with
      | Some binding -> Vector.push bindings ~value:binding
      | None -> ()
  );
  bindings

let collect_binding_operator_clauses = fun expr ->
  let clauses = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.BindingOperatorExpr.for_each_clause expr ~fn:(
    fun clause -> Vector.push clauses ~value:clause
  );
  clauses

let collect_external_name_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_name_token decl ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  tokens

let collect_value_name_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ValueDeclaration.for_each_name_token decl ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  tokens

let collect_external_primitive_strings = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_primitive_string decl ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  tokens

let collect_external_attribute_tokens = fun decl ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ExternalDeclaration.for_each_attribute_token decl ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  tokens

let collect_child_tokens = fun node ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_token node ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  tokens

let collect_prefix_operator_tokens = fun node ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  let rec loop index =
    if Int.(index < Ast.Node.child_count node) then
      match Ast.Node.child_at node index with
      | Some (Syn.SyntaxTree.Token id) ->
          Vector.push tokens ~value:(({ tree = node.Ast.tree; id } : Ast.Token.t));
          loop (Int.add index 1)
      | Some (Syn.SyntaxTree.Node _) | Some (Syn.SyntaxTree.Missing _) | None -> ()
  in
  loop 0;
  tokens

let collect_child_tokens_of_kind = fun node kind ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_token node ~fn:(
    fun token ->
      if token_kind_is token kind then
        Vector.push tokens ~value:token
  );
  tokens

let token_is_keyword_operator_name = fun token ->
  match token_text token with
  | "asr" | "land" | "lor" | "lsl" | "lsr" | "lxor" | "mod" | "or" -> true
  | _ -> false

let token_vector_range_is_keyword_operator_name = fun tokens ~start ~stop ->
  if Int.equal (Int.sub stop start) 1 then
    token_is_keyword_operator_name (Vector.get_unchecked tokens ~at:start)
  else false

let token_vector_range_has_ident = fun tokens ~start ~stop ->
  let rec loop index =
    if Int.(index >= stop) then
      false
    else
      if token_kind_is (Vector.get_unchecked tokens ~at:index) Kind.IDENT then
        true
      else loop (Int.add index 1)
  in
  loop start

let token_vector_range_is_operator_name = fun tokens ~start ~stop ->
  if token_vector_range_has_ident tokens ~start ~stop then
    token_vector_range_is_keyword_operator_name tokens ~start ~stop
  else Int.(start < stop)

let token_vector_is_operator_name = fun tokens -> token_vector_range_is_operator_name tokens ~start:0 ~stop:(Vector.length tokens)

let token_vector_find_kind = fun tokens kind ->
  let length = Vector.length tokens in
  let rec loop index =
    if Int.(index >= length) then
      None
    else
      if token_kind_is (Vector.get_unchecked tokens ~at:index) kind then
        Some index
      else loop (Int.add index 1)
  in
  loop 0

let collect_match_cases = fun expr ->
  let cases = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  Ast.Expr.for_each_match_case expr ~fn:(
    fun case -> Vector.push cases ~value:case
  );
  cases

let collect_variant_constructors = fun variant_type ->
  let constructors = Vector.with_capacity ~size:(Ast.Node.child_count variant_type) in
  Ast.VariantType.for_each_constructor variant_type ~fn:(
    fun constructor -> Vector.push constructors ~value:constructor
  );
  constructors

let collect_record_pattern_fields = fun record ->
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record) in
  Ast.RecordPattern.for_each_field record ~fn:(
    fun field -> Vector.push fields ~value:field
  );
  fields

let collect_signature_items_from_module_type_decl = fun decl ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleTypeDeclaration.for_each_signature_item decl ~fn:(
    fun item -> Vector.push items ~value:item
  );
  items

let collect_signature_items_from_module_decl = fun decl ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleDeclaration.for_each_signature_item decl ~fn:(
    fun item -> Vector.push items ~value:item
  );
  items

let collect_structure_items_from_node = fun node ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      if node_kind_is child Kind.STRUCTURE_ITEM then
        Vector.push items ~value:child
  );
  items

let collect_signature_items_from_node = fun node ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      if node_kind_is child Kind.SIGNATURE_ITEM then
        Vector.push items ~value:child
  );
  items

let collect_module_declaration_members = fun decl ->
  let members = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.ModuleDeclaration.for_each_member decl ~fn:(
    fun member -> Vector.push members ~value:member
  );
  members

let emit_joined_vector = fun state values ~sep ~fn ->
  let length = Vector.length values in
  let rec loop index =
    if Int.(index < length) then
      (
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
  match token_vector_find_kind tokens Kind.LPAREN, token_vector_find_kind tokens Kind.MODULE_KW, token_vector_find_kind tokens Kind.RPAREN with
  | Some opening_index, Some module_index, Some closing_index when Int.equal module_index (Int.add opening_index 1) && Int.equal opening_index 0 && Int.equal closing_index (Int.sub length 1) ->
      emit_token_vector_stream state tokens;
      true
  | _ -> false

let render_bracketed_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  let length = Vector.length tokens in
  if Int.equal length 0 then
    false
  else
    let first = Vector.get_unchecked tokens ~at:0 in
    let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
    if (token_kind_is first Kind.LBRACKET && token_kind_is last Kind.RBRACKET) || (token_kind_is first Kind.LBRACKET_BAR && token_kind_is last Kind.BAR_RBRACKET) then
      (
        emit_token_vector_stream state tokens;
        true
      )
    else false

let render_dotdot_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  if Int.equal (Vector.length tokens) 1 then
    if token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.DOTDOT then
      (
        emit_token state (Vector.get_unchecked tokens ~at:0);
        true
      )
    else false
  else false

let render_gt_opaque_type = fun state node ->
  let tokens = collect_child_tokens node in
  if Int.equal (Vector.length tokens) 1 then
    if token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.GT then
      (
        emit_token state (Vector.get_unchecked tokens ~at:0);
        true
      )
    else false
  else false

let render_opaque_type = fun state node ->
  if render_first_class_module_type state node then
    true
  else
    if render_bracketed_opaque_type state node then
      true
    else
      if render_dotdot_opaque_type state node then
        true
      else render_gt_opaque_type state node

let render_path = fun state path -> Ast.Node.for_each_token path ~fn:(emit_token state)

let token_vector_is_parenthesized_operator_name = fun tokens ->
  let length = Vector.length tokens in
  if Int.(length <= 2) then
    false
  else
    let first = Vector.get_unchecked tokens ~at:0 in
    let last = Vector.get_unchecked tokens ~at:(Int.sub length 1) in
    if token_kind_is first Kind.LPAREN && token_kind_is last Kind.RPAREN then
      token_vector_range_is_operator_name tokens ~start:1 ~stop:(Int.sub length 1)
    else false

let render_parenthesized_operator_tokens = fun state tokens ->
  let length = Vector.length tokens in
  (
    emit_token state (Vector.get_unchecked tokens ~at:0);
    emit_space state;
    emit_token_vector_range_compact state tokens ~start:1 ~stop:(Int.sub length 1);
    emit_space state;
    emit_token state (Vector.get_unchecked tokens ~at:(Int.sub length 1))
  )

let render_pattern_path = fun state path ->
  let tokens = collect_child_tokens path in
  if token_vector_is_parenthesized_operator_name tokens then
    render_parenthesized_operator_tokens state tokens
  else render_path state path

let render_parenthesized_empty_pattern = fun state pattern ->
  let tokens = collect_child_tokens pattern in
  if token_vector_is_parenthesized_operator_name tokens then
    render_parenthesized_operator_tokens state tokens
  else emit_text state "()"

let pattern_is_operator_name = fun pattern ->
  match Ast.Pattern.view pattern with
  | Path { path } -> collect_child_tokens path |> token_vector_is_operator_name
  | _ -> false

let rec render_type_expr = fun state type_expr ->
  match Ast.TypeExpr.inner_without_attribute_suffix type_expr with
  | Some inner ->
      render_type_expr state inner;
      render_type_expr_attribute_suffix state type_expr
  | None -> (
    match Ast.TypeExpr.view type_expr with
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
        if not (render_opaque_type state node) then
          unsupported_node "unsupported type expression" type_expr
    | Error _ | Unknown _ -> unsupported_node "unsupported type expression" type_expr
  )
and render_type_expr_attribute_suffix = fun state type_expr ->
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_attribute_suffix_token type_expr ~fn:(
    fun token -> Vector.push tokens ~value:token
  );
  if Vector.length tokens > 0 then
    (
      emit_space state;
      emit_token_vector_stream state tokens
    )
and render_type_arrow_left = fun state type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Arrow _ ->
      emit_text state "(";
      render_type_expr state type_expr;
      emit_text state ")"
  | _ -> render_type_expr state type_expr
and render_type_apply_argument = fun state argument ->
  match Ast.TypeExpr.view argument with
  | Arrow _ | Poly _ | Tuple _ ->
      emit_text state "(";
      render_type_expr state argument;
      emit_text state ")"
  | _ -> render_type_expr state argument
and type_expr_flat_width = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
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
          loop (Int.add index 1) (Some token) (Int.add total (Int.add extra_space token_width))
      in
      loop 0 None 0
  | Var { name = Some name } -> Some (Int.add 1 (Slice.length (Ast.Token.slice name)))
  | Var { name = None } -> None
  | Wildcard -> Some 1
  | Arrow { left = Some left; right = Some right } -> (
    match type_expr_flat_width left, type_expr_flat_width right with
    | Some left_width, Some right_width ->
        let left_width =
          match Ast.TypeExpr.view left with
          | Arrow _ -> Int.add left_width 2
          | _ -> left_width
        in
        Some Int.(left_width + 4 + right_width)
    | _ -> None
  )
  | Arrow _ -> None
  | Apply { argument = Some argument; constructor = Some constructor } -> (
    match type_expr_flat_width argument, type_expr_flat_width constructor with
    | Some argument_width, Some constructor_width ->
        let argument_width =
          match Ast.TypeExpr.view argument with
          | Arrow _ | Poly _ | Tuple _ -> Int.add argument_width 2
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
  | Poly _ | Opaque _ | Error _ | Unknown _ -> None
and render_poly_type_expr = fun state type_expr body ->
  let names = Vector.with_capacity ~size:(Ast.Node.child_count type_expr) in
  Ast.TypeExpr.for_each_poly_type_name type_expr ~fn:(
    fun name -> Vector.push names ~value:name
  );
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
        if Int.(index < length) then
          (
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
    );
  render_type_expr state body
and render_type_tuple_separator = fun state separator ->
  match separator with
  | Ast.TypeExpr.Star ->
      emit_space state;
      emit_text state "*";
      emit_space state
  | Ast.TypeExpr.Comma ->
      emit_text state ",";
      emit_space state
  | Ast.TypeExpr.UnknownSeparator ->
      emit_space state;
      emit_text state "*";
      emit_space state
and render_tuple_type_expr = fun state type_expr separator ->
  let items = collect_child_type_exprs type_expr in
  let length = Vector.length items in
  if Int.(length < 2) then
    unsupported_node "incomplete tuple type" type_expr
  else emit_joined_vector state items ~sep:(
    fun () -> render_type_tuple_separator state separator
  ) ~fn:(render_type_expr state)
and type_tuple_separator_width = function
  | Ast.TypeExpr.Star | Ast.TypeExpr.UnknownSeparator -> 3
  | Ast.TypeExpr.Comma -> 2
and type_tuple_flat_width = fun type_expr ->
  let items = collect_child_type_exprs type_expr in
  let length = Vector.length items in
  if Int.(length < 2) then
    None
  else
    let separator_width =
      match Ast.TypeExpr.view type_expr with
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
              else Int.(total + separator_width + item_width)
            in
            loop (Int.add index 1) total
    in
    loop 0 0
and type_expr_starts_with_gt = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Opaque node ->
      let tokens = collect_child_tokens node in
      if Int.equal (Vector.length tokens) 1 then
        token_kind_is (Vector.get_unchecked tokens ~at:0) Kind.GT
      else false
  | Apply { argument = Some argument; _ } -> type_expr_starts_with_gt argument
  | _ -> false
and render_type_expr_after_colon = fun state type_expr ->
  if type_expr_starts_with_gt type_expr then
    render_type_expr state type_expr
  else
    (
      emit_space state;
      render_type_expr state type_expr
    )

let pattern_tuple_has_parens = fun pattern -> Option.is_some (Ast.Node.first_child_token pattern ~kind:Kind.LPAREN)

let collect_tuple_pattern_items = fun pattern ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  let rec push_items pattern = Ast.Pattern.for_each_child_pattern pattern ~fn:(
    fun child ->
      match Ast.Pattern.view child with
      | Tuple when not (pattern_tuple_has_parens child) -> push_items child
      | _ -> Vector.push items ~value:child
  ) in
  push_items pattern;
  items

let path_single_ident_token = fun path ->
  let found = ref None in
  let count = ref 0 in
  Ast.Path.for_each_ident path ~fn:(
    fun token ->
      count := Int.add !count 1;
      if Int.equal !count 1 then
        found := Some token
  );
  if Int.equal !count 1 then
    !found
  else None

let rec pattern_binding_ident_token = fun pattern ->
  match Ast.Pattern.view pattern with
  | Path { path } -> path_single_ident_token path
  | Parenthesized { inner = Some inner } | Constraint { pattern = Some inner; _ } | Attribute { inner = Some inner } -> pattern_binding_ident_token inner
  | _ -> None

let parameter_pattern_matches_label = fun label pattern ->
  match pattern_binding_ident_token pattern with
  | Some binding -> Ast.Token.text_equal label binding
  | None -> false

let split_typed_parameter_pattern = fun pattern ->
  match Ast.Pattern.view pattern with
  | Constraint { pattern = Some pattern; annotation = Some annotation } -> Some (pattern, annotation)
  | _ -> None

let parameter_pattern_requires_parens = fun pattern ->
  match Ast.Pattern.view pattern with
  | Alias _ -> true
  | _ -> false

let parameter_token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  if Kind.(previous_kind = COLON || current_kind = COLON) then
    false
  else token_wants_space_before previous token

let emit_parameter_token_stream = fun state parameter ->
  let previous = ref None in Ast.Node.for_each_token parameter ~fn:(
    fun token ->
      (
        match !previous with
        | Some previous when parameter_token_wants_space_before previous token -> emit_space state
        | Some _ | None -> ()
      );
      emit_token state token;
      previous := Some token
  )

let parameter_colon_has_leading_space = fun parameter ->
  let found = ref None in
  Ast.Node.for_each_token parameter ~fn:(
    fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Kind.(Ast.Token.kind token = COLON) then
            found := Some token
  );
  match !found with
  | Some colon -> Ast.Token.has_leading_whitespace colon
  | None -> false

let rec render_pattern = fun state pattern ->
  match Ast.Pattern.view pattern with
  | Wildcard -> emit_text state "_"
  | Path { path } -> render_pattern_path state path
  | Literal { token = Some token } ->
      (
        match Ast.Pattern.literal_sign_token pattern with
        | Some sign -> emit_token state sign
        | None -> ()
      );
      emit_token state token
  | Literal { token = None } -> unsupported_node "literal pattern without token" pattern
  | Apply { callee = Some callee; argument = Some argument } ->
      render_pattern state callee;
      emit_space state;
      render_pattern_atom state argument
  | Apply _ -> unsupported_node "incomplete apply pattern" pattern
  | Parenthesized { inner = Some inner } when pattern_is_operator_name inner ->
      emit_text state "(";
      emit_space state;
      render_pattern state inner;
      emit_space state;
      emit_text state ")"
  | Parenthesized { inner = Some inner } ->
      emit_text state "(";
      render_pattern state inner;
      emit_text state ")"
  | Parenthesized { inner = None } -> render_parenthesized_empty_pattern state pattern
  | Constraint { pattern = Some pattern; annotation = Some annotation } ->
      render_pattern state pattern;
      emit_text state ":";
      emit_space state;
      render_type_expr state annotation
  | Constraint _ -> unsupported_node "incomplete constraint pattern" pattern
  | Alias { pattern = Some pattern; alias = Some alias } ->
      render_pattern state pattern;
      emit_space state;
      emit_text state "as";
      emit_space state;
      render_pattern state alias
  | Alias _ -> unsupported_node "incomplete alias pattern" pattern
  | Or { left = Some left; right = Some right } ->
      render_pattern state left;
      emit_space state;
      emit_text state "|";
      emit_space state;
      render_pattern state right
  | Or _ -> unsupported_node "incomplete or pattern" pattern
  | Tuple ->
      let items = collect_tuple_pattern_items pattern in emit_joined_vector state items ~sep:(
        fun () ->
          emit_text state ",";
          emit_space state
      ) ~fn:(
        fun item ->
          if pattern_tuple_has_parens item then
            render_pattern_atom state item
          else render_pattern state item
      )
  | List ->
      let items = collect_child_patterns pattern in
      if Int.equal (Vector.length items) 0 then
        emit_text state "[]"
      else
        (
          emit_text state "[ ";
          emit_joined_vector state items ~sep:(
            fun () ->
              emit_text state ";";
              emit_space state
          ) ~fn:(render_pattern state);
          emit_space state;
          emit_text state "]"
        )
  | Array ->
      let items = collect_child_patterns pattern in
      emit_text state "[|";
      emit_joined_vector state items ~sep:(
        fun () ->
          emit_text state ";";
          emit_space state
      ) ~fn:(render_pattern state);
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
      render_pattern_atom state head;
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
  | LabeledParam parameter | OptionalParam parameter | OptionalParamDefault parameter -> render_parameter state parameter
  | LocallyAbstractType | FirstClassModule -> emit_token_stream state pattern
  | LocalOpen -> render_local_open_pattern state pattern
  | Extension | Attribute _ | Error _ | Unknown _ -> unsupported_node "unsupported pattern" pattern
and render_parameter_pattern = fun state parameter pattern ->
  let needs_parens = Ast.Parameter.has_explicit_pattern_parens parameter || parameter_pattern_requires_parens pattern in
  if needs_parens then
    (
      emit_text state "(";
      render_pattern state pattern;
      emit_text state ")"
    )
  else render_pattern state pattern
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
  | Labeled { label = Some label; pattern = None } ->
      emit_text state "~";
      emit_token state label
  | Labeled { label = Some label; pattern = Some pattern } -> render_named_parameter state ~sigil:"~" parameter label pattern
  | Labeled _ -> unsupported_node "labeled parameter without label" parameter
  | Optional { label = Some label; pattern = None } ->
      emit_text state "?";
      emit_token state label
  | Optional { label = Some label; pattern = Some pattern } -> render_named_parameter state ~sigil:"?" parameter label pattern
  | Optional _ -> unsupported_node "optional parameter without label" parameter
  | OptionalDefault _ -> emit_parameter_token_stream state parameter
  | Unknown _ -> unsupported_node "unsupported parameter" parameter
and loose_parameter_binding_annotation = fun parameter ->
  match Ast.Pattern.view parameter with
  | LabeledParam parameter -> (
    match Ast.Parameter.view parameter with
    | Labeled { pattern = Some annotation; _ } when (not (Ast.Parameter.has_explicit_pattern_parens parameter)) && parameter_colon_has_leading_space parameter -> Some annotation
    | _ -> None
  )
  | OptionalParam parameter -> (
    match Ast.Parameter.view parameter with
    | Optional { pattern = Some annotation; _ } when (not (Ast.Parameter.has_explicit_pattern_parens parameter)) && parameter_colon_has_leading_space parameter -> Some annotation
    | _ -> None
  )
  | _ -> None
and render_parameter_label_only = fun state parameter ->
  match Ast.Pattern.view parameter with
  | LabeledParam parameter -> (
    match Ast.Parameter.view parameter with
    | Labeled { label = Some label; _ } ->
        emit_text state "~";
        emit_token state label
    | _ -> emit_parameter_token_stream state parameter
  )
  | OptionalParam parameter -> (
    match Ast.Parameter.view parameter with
    | Optional { label = Some label; _ } ->
        emit_text state "?";
        emit_token state label
    | _ -> emit_parameter_token_stream state parameter
  )
  | _ -> render_pattern state parameter
and render_local_open_pattern = fun state pattern ->
  let path_tokens = Vector.with_capacity ~size:(Ast.Node.child_count pattern) in
  Ast.LocalOpenPattern.for_each_module_path_ident pattern ~fn:(
    fun token -> Vector.push path_tokens ~value:token
  );
  if Vector.length path_tokens = 0 then
    unsupported_node "local-open pattern without module path" pattern
  else emit_joined_vector state path_tokens ~sep:(
    fun () -> emit_text state "."
  ) ~fn:(emit_token state);
  emit_text state ".";
  match Ast.LocalOpenPattern.opening_token pattern, Ast.LocalOpenPattern.closing_token pattern, Ast.LocalOpenPattern.pattern pattern with
  | Some opening, Some closing, Some body when token_kind_is opening Kind.LPAREN ->
      emit_token state opening;
      render_pattern state body;
      emit_token state closing
  | _, _, Some body -> render_pattern state body
  | _ -> unsupported_node "local-open pattern without body" pattern
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
  else
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
and render_record_pattern = fun state pattern ->
  let fields = collect_record_pattern_fields pattern in
  let length = Vector.length fields in
  emit_text state "{";
  if Int.(length > 0) then
    (
      emit_space state;
      emit_joined_vector state fields ~sep:(
        fun () ->
          emit_text state ";";
          emit_space state
      ) ~fn:(
        fun field ->
          match field.Ast.RecordPattern.path with
          | Some path -> (
            render_path state path;
            match field.Ast.RecordPattern.pattern with
            | Some pattern ->
                emit_space state;
                emit_text state "=";
                emit_space state;
                render_pattern state pattern
            | None -> ()
          )
          | None -> render_pattern state field.Ast.RecordPattern.node
      );
      (
        match Ast.RecordPattern.open_wildcard pattern with
        | Some _ ->
            if Int.(length > 0) then
              (
                emit_text state ";";
                emit_space state
              );
            emit_text state "_"
        | None -> ()
      );
      emit_space state
    );
  emit_text state "}"
and render_pattern_atom = fun state pattern ->
  match Ast.Pattern.view pattern with
  | Apply _ | Or _ | Cons _ | Tuple | Alias _ | Constraint _ ->
      emit_text state "(";
      render_pattern state pattern;
      emit_text state ")"
  | _ -> render_pattern state pattern

let rec pattern_contains_type_annotation = fun pattern annotation ->
  match Ast.Pattern.view pattern with
  | Constraint { annotation = Some existing; _ } when same_ast_node existing annotation -> true
  | _ ->
      let found = ref false in
      Ast.Pattern.for_each_child_pattern pattern ~fn:(
        fun child ->
          if not !found && not (same_ast_node pattern child) then
            found := pattern_contains_type_annotation child annotation
      );
      !found

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
    match Ast.Expr.view expr with
    | Apply { callee = Some callee; argument = Some argument } when not (same_ast_node expr callee) && not (same_ast_node expr argument) ->
        Vector.push args ~value:argument;
        loop (Int.sub budget 1) callee
    | _ -> expr
  in
  let callee = loop 128 expr in
  if Int.(Vector.length args > 1) then
    Vector.reverse args;
  (callee, args)

let expr_classification_budget = 512

let rec expr_is_multiline = fun expr -> expr_is_multiline_with_budget expr_classification_budget expr
and expr_is_multiline_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    unsupported_node "cyclic expression while classifying multiline layout" expr;
  let next_budget = Int.sub budget 1 in
  match Ast.Expr.view expr with
  | If _ | Let _ | Match _ | Fun _ | Function _ | Try _ | LetModule _ | LetException _ | BindingOperator _ | Sequence _ | While _ | For _ -> true
  | Record | RecordUpdate -> not (record_expr_should_inline expr)
  | Parenthesized { inner = Some inner } when expr_first_token_text_is expr "begin" -> expr_is_multiline_with_budget next_budget inner
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) -> expr_is_multiline_with_budget next_budget inner
  | Array -> not (array_expr_should_inline expr)
  | List -> not (list_expr_should_inline expr)
  | Infix { left; right; _ } ->
      if option_expr_child_is_multiline_with_budget next_budget expr left then
        true
      else option_expr_child_is_multiline_with_budget next_budget expr right
  | Apply _ ->
      let _, args = collect_apply expr in vector_exists_expr_is_multiline_except_with_budget next_budget expr args
  | _ -> false
and expr_is_parenthesized_multiline = fun expr ->
  match Ast.Expr.view expr with
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) -> expr_is_multiline inner
  | _ -> false
and expr_is_tuple = fun expr ->
  match Ast.Expr.view expr with
  | Tuple -> true
  | _ -> false
and expr_can_capture_list_separator = fun expr ->
  match Ast.Expr.view expr with
  | If _ | Let _ | Match _ | Fun _ | Function _ | Try _ | BindingOperator _ | LetModule _ | LetException _ | Sequence _ | While _ | For _ -> true
  | Parenthesized _ -> false
  | Attribute { inner = Some inner } -> expr_can_capture_list_separator inner
  | _ -> false
and tuple_expr_needs_parens_in_list_item = fun expr ->
  if expr_is_tuple expr then
    let items = collect_tuple_expr_items expr in
    let length = Vector.length items in
    let rec loop index =
      if Int.(index >= length) then
        false
      else
        if expr_can_capture_list_separator (Vector.get_unchecked items ~at:index) then
          true
        else loop (Int.add index 1)
    in
    loop 0
  else false
and expr_tuple_has_parens = fun expr -> Option.is_some (Ast.Node.first_child_token expr ~kind:Kind.LPAREN)
and collect_tuple_expr_items = fun expr ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count expr) in
  let rec push_items expr = Ast.Expr.for_each_child_expr expr ~fn:(
    fun child ->
      match Ast.Expr.view child with
      | Tuple when not (expr_tuple_has_parens child) -> push_items child
      | _ -> Vector.push items ~value:child
  ) in
  push_items expr;
  items
and vector_exists_expr_is_multiline_except = fun parent exprs -> vector_exists_expr_is_multiline_except_with_budget expr_classification_budget parent exprs
and vector_exists_expr_is_multiline_except_with_budget = fun budget parent exprs ->
  let length = Vector.length exprs in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      let expr = Vector.get_unchecked exprs ~at:index in
      if same_ast_node parent expr then
        loop (Int.add index 1)
      else
        if expr_is_multiline_with_budget budget expr then
          true
        else loop (Int.add index 1)
  in
  loop 0
and option_expr_child_is_multiline = fun parent child -> option_expr_child_is_multiline_with_budget expr_classification_budget parent child
and option_expr_child_is_multiline_with_budget = fun budget parent child ->
  match child with
  | Some child ->
      if same_ast_node parent child then
        false
      else expr_is_multiline_with_budget budget child
  | None -> false
and expr_is_inline = fun expr -> expr_is_inline_with_budget expr_classification_budget expr
and expr_is_inline_with_budget = fun budget expr ->
  if Int.(budget <= 0) then
    unsupported_node "cyclic expression while classifying inline layout" expr;
  let next_budget = Int.sub budget 1 in
  match Ast.Expr.view expr with
  | Path _ | Literal _ | FieldAccess _ | Prefix _ | LabeledArg _ | OptionalArg _ -> true
  | FirstClassModule -> true
  | Attribute { inner = Some inner } ->
      if same_ast_node expr inner then
        false
      else expr_is_inline_with_budget next_budget inner
  | LocalOpen { body = Some body } ->
      if same_ast_node expr body then
        false
      else expr_is_inline_with_budget next_budget body
  | Infix { left = Some left; right = Some right; _ } ->
      if same_ast_node expr left || same_ast_node expr right then
        false
      else
        if not (expr_is_inline_with_budget next_budget left) then
          false
        else expr_is_inline_with_budget next_budget right
  | Apply _ ->
      let callee, args = collect_apply expr in
      if same_ast_node expr callee then
        false
      else
        if not (expr_is_inline_with_budget next_budget callee) then
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
              else
                if expr_is_inline_with_budget next_budget arg then
                  loop (Int.add index 1)
                else false
          in
          loop 0
  | Parenthesized { inner = Some inner } ->
      if same_ast_node expr inner then
        false
      else expr_is_inline_with_budget next_budget inner
  | Parenthesized { inner = None } -> true
  | Array -> array_expr_should_inline expr
  | List -> list_expr_should_inline expr
  | Record -> record_expr_should_inline expr
  | RecordUpdate -> record_expr_should_inline expr
  | Typed { expr = Some inner; _ } -> expr_is_inline_with_budget next_budget inner
  | _ -> false
and array_expr_should_inline = fun expr ->
  let items = collect_array_items expr in
  match Vector.length items with
  | 0 -> true
  | 1 -> expr_is_inline (Vector.get_unchecked items ~at:0)
  | _ -> false
and list_expr_should_inline = fun expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  if Int.(length > 3) then
    false
  else
    let rec loop index =
      if Int.(index >= length) then
        true
      else
        if expr_is_inline (Vector.get_unchecked items ~at:index) then
          loop (Int.add index 1)
        else false
    in
    loop 0
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
        match field.Ast.RecordExpr.value with
        | None -> loop (Int.add index 1)
        | Some value ->
            if expr_is_inline value then
              loop (Int.add index 1)
            else false
    in
    loop 0

let expr_can_follow_pipe_bare = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ | Fun _ | Function _ -> true
  | _ -> false

let expr_can_be_bare_infix_left_operand = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ -> true
  | _ -> false

let expr_can_be_bare_infix_right_operand = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ | Fun _ | Function _ | If _ | Let _ | Match _ | Try _ -> true
  | _ -> false

let expr_can_be_bare_pipe_left_operand = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ -> true
  | _ -> false

let expr_can_be_bare_caret_operand = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ -> true
  | _ -> false

let expr_is_infix_with_operator_text = fun expr expected ->
  match Ast.Expr.view expr with
  | Infix { operator = Some operator; _ } -> String.equal (token_text operator) expected
  | _ -> false

type infix_associativity =
  | Infix_left
  | Infix_right

let infix_operator_precedence = fun operator ->
  let length = String.length operator in
  if String.equal operator "@@" then
    1
  else
    if String.equal operator "|>" then
      2
    else
      if String.equal operator "||" || String.equal operator "or" then
        3
      else
        if String.equal operator "&&" || String.equal operator "&" then
          4
        else
          if String.equal operator "::" then
            6
          else
            if String.equal operator "@" || String.equal operator "^" then
              7
            else
              if String.equal operator "mod" || String.equal operator "land" || String.equal operator "lor" || String.equal operator "lxor" then
                9
              else
                if Int.(length > 0) then
                  (
                    match String.get_unchecked operator ~at:0 with
                    | '=' | '<' | '>' | '|' | '&' | '$' | '!' -> 5
                    | '+' | '-' -> 8
                    | '*' | '/' | '%' -> 9
                    | '@' | '^' -> 7
                    | _ -> 8
                  )
                else 8

let infix_operator_associativity = fun operator ->
  if String.equal operator "::" || String.starts_with ~prefix:"@" operator || String.starts_with ~prefix:"^" operator then
    Infix_right
  else Infix_left

let expr_infix_operator_text = fun expr ->
  match Ast.Expr.view expr with
  | Infix { operator = Some operator; _ } -> Some (token_text operator)
  | _ -> None

let infix_left_operand_can_be_bare = fun ~parent_operator left ->
  match expr_infix_operator_text left with
  | None -> false
  | Some child_operator ->
      let parent_precedence = infix_operator_precedence parent_operator in
      let child_precedence = infix_operator_precedence child_operator in
      if Int.(child_precedence > parent_precedence) then
        true
      else
        if Int.equal child_precedence parent_precedence then
          match infix_operator_associativity parent_operator with
          | Infix_left -> true
          | Infix_right -> false
        else false

let infix_right_operand_can_be_bare = fun ~parent_operator right ->
  match expr_infix_operator_text right with
  | None -> false
  | Some child_operator ->
      let parent_precedence = infix_operator_precedence parent_operator in
      let child_precedence = infix_operator_precedence child_operator in
      if Int.(child_precedence > parent_precedence) then
        true
      else
        if Int.equal child_precedence parent_precedence then
          match infix_operator_associativity parent_operator with
          | Infix_left -> false
          | Infix_right -> true
        else false

let rec render_expr = fun state expr ->
  emit_node_leading_trivia state expr;
  match Ast.Expr.view expr with
  | Path { path } -> render_path state path
  | Literal { token = Some token } -> emit_token state token
  | Literal { token = None } -> unsupported_node "literal expression without token" expr
  | FieldAccess { target = Some target; field = Some field } ->
      render_expr_atom state target;
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
  | Infix { left = Some left; operator = Some operator; right = Some right } -> render_infix_expr state left operator right
  | Infix _ -> unsupported_node "incomplete infix expression" expr
  | Prefix { operator = Some operator; operand = Some operand } ->
      let operator_tokens = collect_prefix_operator_tokens expr in
      let last_operator =
        if Int.equal (Vector.length operator_tokens) 0 then
          (
            emit_token state operator;
            operator
          )
        else
          (
            emit_token_vector_stream state operator_tokens;
            Vector.get_unchecked operator_tokens ~at:(Int.sub (Vector.length operator_tokens) 1)
          )
      in
      if token_text_is last_operator "not" then
        emit_space state;
      render_expr_atom state operand
  | Prefix _ -> unsupported_node "incomplete prefix expression" expr
  | Apply _ -> render_apply_expr state expr
  | LabeledArg { label = Some label; value = Some value } ->
      emit_text state "~";
      emit_token state label;
      emit_text state ":";
      render_expr_atom state value
  | LabeledArg { label = Some label; value = None } ->
      emit_text state "~";
      emit_token state label
  | LabeledArg { label = None; _ } -> unsupported_node "labeled argument without label" expr
  | OptionalArg { label = Some label; value = Some value } ->
      emit_text state "?";
      emit_token state label;
      emit_text state ":";
      render_expr_atom state value
  | OptionalArg { label = Some label; value = None } ->
      emit_text state "?";
      emit_token state label
  | OptionalArg { label = None; _ } -> unsupported_node "optional argument without label" expr
  | Parenthesized { inner = Some inner } when expr_first_token_text_is expr "begin" ->
      let render_inner () =
        with_delimited_expr state
          (
            fun () -> render_expr state inner
          )
      in
      emit_text state "begin";
      emit_line state;
      with_indent state 2 render_inner;
      emit_line state;
      emit_text state "end"
  | Parenthesized { inner = Some inner } when not (same_ast_node expr inner) -> render_parenthesized_expr state inner
  | Parenthesized { inner = Some inner } ->
      emit_text state "(";
      with_delimited_expr state
        (
          fun () -> render_expr state inner
        );
      emit_text state ")"
  | Parenthesized { inner = None } -> emit_text state "()"
  | If { condition = Some condition; then_branch = Some then_branch; else_branch } -> render_if_expr state expr condition then_branch else_branch
  | If _ -> unsupported_node "incomplete if expression" expr
  | Let { body = Some body; _ } -> render_let_expr state expr body
  | Let _ -> unsupported_node "let expression without body" expr
  | Fun { body = Some body } -> render_fun_expr state expr body
  | Fun { body = None } -> unsupported_node "fun expression without body" expr
  | Function { first_case = Some _ } -> render_function_expr state expr
  | Function { first_case = None } -> unsupported_node "function expression without cases" expr
  | Match { scrutinee = Some scrutinee; first_case = Some _ } -> render_match_expr state expr scrutinee
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
      render_expr state inner;
      emit_space state;
      emit_text state ":";
      render_type_expr_after_colon state annotation
  | Typed _ -> unsupported_node "incomplete typed expression" expr
  | Tuple ->
      let items = collect_tuple_expr_items expr in emit_joined_vector state items ~sep:(
        fun () ->
          emit_text state ",";
          emit_space state
      ) ~fn:(
        fun item ->
          if expr_is_tuple item then
            render_expr_atom state item
          else render_expr state item
      )
  | Sequence { left = Some left; right = Some right } ->
      if expr_is_tuple left then
        render_expr_atom state left
      else render_expr state left;
      emit_text state ";";
      emit_line state;
      if expr_is_tuple right then
        render_expr_atom state right
      else render_expr state right
  | Sequence { left = Some left; right = None } -> render_expr state left
  | Sequence _ -> unsupported_node "incomplete sequence expression" expr
  | ArrayIndex { target = Some target; index = Some index } ->
      render_expr_atom state target;
      emit_text state ".(";
      render_expr state index;
      emit_text state ")"
  | ArrayIndex _ -> unsupported_node "incomplete array index expression" expr
  | StringIndex { target = Some target; index = Some index } ->
      render_expr_atom state target;
      emit_text state ".[";
      render_expr state index;
      emit_text state "]"
  | StringIndex _ -> unsupported_node "incomplete string index expression" expr
  | PolyVariant { payload } -> render_poly_variant_expr state expr payload
  | BindingOperator _ -> render_binding_operator_expr state expr
  | LetModule { body = Some _ } -> render_let_module_expr state expr
  | LetModule { body = None } -> unsupported_node "let module expression without body" expr
  | FirstClassModule -> render_first_class_module_expr state expr
  | LetException { body = Some _ } -> render_let_exception_expr state expr
  | LetException { body = None } -> unsupported_node "let exception expression without body" expr
  | While { condition = Some condition; body = Some body } -> render_while_expr state condition body
  | While _ -> unsupported_node "incomplete while expression" expr
  | Assert { argument = Some argument } -> render_assert_expr state argument
  | Assert { argument = None } -> unsupported_node "assert expression without argument" expr
  | Attribute { inner = Some _ } -> render_attribute_expr state expr
  | Attribute { inner = None } -> unsupported_node "attribute expression without inner expression" expr
  | Extension -> render_extension_expr state expr
  | For { pattern = Some pattern; start_ = Some start_; stop = Some stop; body = Some body } -> render_for_expr state expr pattern start_ stop body
  | For _ -> unsupported_node "incomplete for expression" expr
  | Unreachable | Object | New | Lazy _ | MethodCall _ | Error _ | Unknown _ -> unsupported_node "unsupported expression" expr
and render_expr_atom = fun state expr ->
  match Ast.Expr.view expr with
  | Path _ | Literal _ | FieldAccess _ | Prefix _ | Parenthesized _ | Array | List | Record | ArrayIndex _ | StringIndex _ | FirstClassModule | Extension | LocalOpen _ | LabeledArg _ | PolyVariant { payload = None } | OptionalArg _ -> render_expr state expr
  | _ -> render_parenthesized_expr state expr
and render_parenthesized_expr = fun state expr ->
  let render_inner () =
    with_delimited_expr state
      (
        fun () -> render_expr state expr
      )
  in
  if expr_is_multiline expr then
    (
      emit_text state "(";
      emit_line state;
      with_indent state 2 render_inner;
      emit_line state;
      emit_text state ")"
    )
  else
    (
      emit_text state "(";
      render_inner ();
      emit_text state ")"
    )
and render_infix_left_operand = fun state ~operator_text left ->
  if String.equal operator_text "@@" then
    render_expr state left
  else
    if expr_can_be_bare_infix_left_operand left then
      render_expr state left
    else
      if String.equal operator_text "|>" && (expr_can_be_bare_pipe_left_operand left || expr_is_infix_with_operator_text left operator_text) then
        render_expr state left
      else
        if String.equal operator_text "^" && (expr_can_be_bare_caret_operand left || expr_is_infix_with_operator_text left operator_text) then
          render_expr state left
        else
          if String.equal operator_text "::" && expr_is_infix_with_operator_text left operator_text then
            render_expr state left
          else
            if infix_left_operand_can_be_bare ~parent_operator:operator_text left then
              render_expr state left
            else render_expr_atom state left
and render_infix_right_operand = fun state ~operator_text right ->
  if String.equal operator_text "@@" || expr_can_be_bare_infix_right_operand right || (String.equal operator_text "|>" && expr_can_follow_pipe_bare right) || (String.equal operator_text "^" && expr_can_be_bare_caret_operand right) || infix_right_operand_can_be_bare ~parent_operator:operator_text right then
    render_expr state right
  else render_expr_atom state right
and render_infix_expr = fun state left operator right ->
  let operator_text = token_text operator in
  render_infix_left_operand state ~operator_text left;
  emit_space state;
  emit_token state operator;
  emit_space state;
  render_infix_right_operand state ~operator_text right
and render_apply_flat = fun state callee args ->
  let callee, base_args = collect_apply callee in
  let render_arg arg =
    emit_space state;
    render_apply_argument state arg
  in
  render_expr_atom state callee;
  Vector.for_each base_args ~fn:render_arg;
  Vector.for_each args ~fn:render_arg
and render_infix_expr_with_right_apply = fun state left operator right args ->
  let operator_text = token_text operator in
  render_infix_left_operand state ~operator_text left;
  emit_space state;
  emit_token state operator;
  emit_space state;
  render_apply_flat state right args
and render_keyword_body_expr = fun state expr ->
  match Ast.Expr.view expr with
  | Tuple -> render_expr_atom state expr
  | _ -> render_expr state expr
and render_in_body_expr = fun state expr ->
  match Ast.Expr.view expr with
  | Tuple -> render_expr_atom state expr
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
and render_apply_argument = fun state arg ->
  let rec render_split_arg arg =
    match Ast.Expr.view arg with
    | PolyVariant { payload = Some payload } ->
        render_poly_variant_tag state arg;
        emit_space state;
        render_split_arg payload
    | LabeledArg { label = Some label; value = Some value } -> (
      match Ast.Expr.view value with
      | PolyVariant { payload = Some payload } ->
          emit_text state "~";
          emit_token state label;
          emit_text state ":";
          render_poly_variant_tag state value;
          emit_space state;
          render_split_arg payload
      | _ -> render_expr_atom state arg
    )
    | OptionalArg { label = Some label; value = Some value } -> (
      match Ast.Expr.view value with
      | PolyVariant { payload = Some payload } ->
          emit_text state "?";
          emit_token state label;
          emit_text state ":";
          render_poly_variant_tag state value;
          emit_space state;
          render_split_arg payload
      | _ -> render_expr_atom state arg
    )
    | _ -> render_expr_atom state arg
  in
  render_split_arg arg
and expr_is_constructor_like_callee = fun expr ->
  match Ast.Expr.view expr with
  | Path _ -> (
    match last_ident_token expr with
    | Some token -> token_text_starts_uppercase token
    | None -> false
  )
  | PolyVariant _ -> true
  | _ -> false
and render_apply_expr = fun state expr ->
  let callee, args = collect_apply expr in
  let arg_count = Vector.length args in
  let break_before_multiline_args =
    if vector_exists_expr_is_multiline_except expr args then
      not (Int.equal arg_count 1 && expr_is_constructor_like_callee callee)
    else false
  in
  if same_ast_node expr callee then
    unsupported_node "apply expression resolved to itself" expr;
  match Ast.Expr.view callee with
  | Infix { left = Some left; operator = Some operator; right = Some right } -> render_infix_expr_with_right_apply state left operator right args
  | _ ->
      render_expr_atom state callee;
      let rec loop index breaking =
        if Int.(index < arg_count) then
          (
            let arg = Vector.get_unchecked args ~at:index in
            let should_break =
              if not break_before_multiline_args then
                false
              else
                if breaking then
                  true
                else expr_is_multiline arg
            in
            if should_break then
              (
                emit_line state;
                with_indent state 2
                  (
                    fun () -> render_apply_argument state arg
                  );
                loop (Int.add index 1) true
              )
            else
              (
                emit_space state;
                render_apply_argument state arg;
                loop (Int.add index 1) breaking
              )
          )
      in
      loop 0 false
and render_if_expr = fun state expr condition then_branch else_branch ->
  emit_node_keyword state expr ~kind:Kind.IF_KW ~fallback:"if";
  emit_space state;
  render_expr state condition;
  emit_space state;
  emit_node_keyword state expr ~kind:Kind.THEN_KW ~fallback:"then";
  emit_line state;
  with_indent state 2
    (
      fun () -> render_keyword_body_expr state then_branch
    );
  (
    match else_branch with
    | None -> ()
    | Some branch ->
        emit_line state;
        emit_node_keyword state expr ~kind:Kind.ELSE_KW ~fallback:"else";
        if expr_is_multiline branch then
          (
            emit_line state;
            with_indent state 2
              (
                fun () -> render_keyword_body_expr state branch
              )
          )
        else
          (
            emit_space state;
            render_keyword_body_expr state branch
          )
  )
and render_fun_expr = fun state expr body ->
  let params = collect_child_patterns expr in
  let render_body () =
    match Ast.Expr.view body with
    | Tuple -> render_expr_atom state body
    | _ -> render_expr state body
  in
  emit_text state "fun";
  Vector.for_each params ~fn:(
    fun param ->
      emit_space state;
      render_pattern state param
  );
  emit_space state;
  emit_text state "->";
  if expr_is_multiline body then
    (
      emit_line state;
      with_indent state 2 render_body
    )
  else
    (
      emit_space state;
      render_body ()
    )
and render_function_expr = fun state expr ->
  let inline = not state.line_start in
  emit_text state "function";
  emit_line state;
  let cases = collect_match_cases expr in
  if inline then
    with_indent state 2
      (
        fun () -> render_match_cases state cases
      )
  else render_match_cases state cases
and render_match_expr = fun state expr scrutinee ->
  emit_text state "match";
  emit_space state;
  render_expr state scrutinee;
  emit_space state;
  emit_text state "with";
  emit_line state;
  let cases = collect_match_cases expr in render_match_cases state cases
and render_try_expr = fun state expr body ->
  emit_text state "try";
  if expr_is_multiline body then
    (
      emit_line state;
      with_indent state 2
        (
          fun () -> render_expr state body
        );
      emit_line state
    )
  else
    (
      emit_space state;
      render_expr state body;
      emit_space state
    );
  emit_text state "with";
  emit_line state;
  let cases = collect_match_cases expr in render_match_cases state cases
and render_assert_expr = fun state argument ->
  emit_text state "assert";
  emit_space state;
  render_expr state argument
and render_attribute_expr = fun state expr ->
  match Ast.AttributeExpr.cast expr with
  | Some attribute -> (
    match Ast.AttributeExpr.inner attribute with
    | Some inner ->
        render_expr state inner;
        emit_space state;
        emit_shell_token_stream state
          (
            fun ~fn -> Ast.AttributeExpr.for_each_shell_token attribute ~fn
          )
    | None -> unsupported_node "attribute expression without inner expression" expr
  )
  | None -> unsupported_node "unsupported attribute expression" expr
and render_extension_expr = fun state expr ->
  match Ast.ExtensionExpr.cast expr with
  | Some extension ->
      emit_shell_token_stream state
        (
          fun ~fn -> Ast.ExtensionExpr.for_each_shell_token extension ~fn
        )
  | None -> unsupported_node "unsupported extension expression" expr
and render_let_exception_expr = fun state expr ->
  match Ast.LetExceptionExpr.cast expr with
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
          Ast.LetExceptionExpr.for_each_payload_token let_exception ~fn:(
            fun token -> Vector.push payload_tokens ~value:token
          );
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
and loop_body_should_break = fun body ->
  match Ast.Expr.view body with
  | Assign _ | If _ | Let _ | Match _ | Try _ | Sequence _ | LetModule _ | BindingOperator _ | While _ | For _ -> true
  | _ -> expr_is_multiline body
and render_loop_body = fun state body ->
  if loop_body_should_break body then
    (
      emit_line state;
      with_indent state 2
        (
          fun () -> render_expr state body
        );
      emit_line state
    )
  else
    (
      emit_space state;
      render_expr state body;
      emit_space state
    )
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
and render_match_cases = fun state cases ->
  let length = Vector.length cases in
  let rec loop index =
    if Int.(index < length) then
      (
        render_match_case state (Vector.get_unchecked cases ~at:index);
        if Int.(index < Int.sub length 1) then
          emit_line state;
        loop (Int.add index 1)
      )
  in
  loop 0
and render_match_case = fun state case ->
  let view = Ast.MatchCase.view case in
  emit_text state "|";
  emit_space state;
  (
    match view.pattern with
    | Some pattern -> render_pattern state pattern
    | None -> unsupported_node "match case without pattern" case
  );
  (
    match view.guard with
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
    match view.body with
    | Some body when expr_is_parenthesized_multiline body ->
        emit_space state;
        render_expr state body
    | Some body when expr_is_tuple body && Int.(state.delimited_expr_depth > 0) && expr_has_leading_comment body ->
        emit_line state;
        with_indent state 4
          (
            fun () ->
              emit_node_leading_comments_as_lines state body;
              render_expr_atom state body
          )
    | Some body when expr_is_tuple body && Int.(state.delimited_expr_depth > 0) ->
        emit_space state;
        render_expr_atom state body
    | Some body when expr_is_tuple body && expr_has_leading_comment body ->
        emit_line state;
        with_indent state 4
          (
            fun () ->
              emit_node_leading_comments_as_lines state body;
              render_expr state body
          )
    | Some body when expr_is_tuple body ->
        emit_space state;
        render_expr state body
    | Some body when expr_is_multiline body ->
        emit_line state;
        with_indent state 4
          (
            fun () -> render_expr state body
          )
    | Some body ->
        emit_space state;
        render_expr state body
    | None -> unsupported_node "match case without body" case
  )
and render_local_open_expr = fun state expr ->
  let render_delimited_body opening body closing =
    if token_kind_is opening Kind.LPAREN then
      (
        emit_token state opening;
        let spaced =
          match Ast.Expr.view body with
          | Path { path } ->
              let tokens = collect_child_tokens path in token_vector_is_operator_name tokens
          | _ -> false
        in
        if spaced then
          emit_space state;
        render_expr state body;
        if spaced then
          emit_space state;
        emit_token state closing
      )
    else render_expr state body
  in
  match Ast.LocalOpenExpr.view expr with
  | LetOpen { module_path = Some module_path; body = Some body; _ } ->
      emit_text state "let";
      emit_space state;
      emit_text state "open";
      emit_space state;
      render_path state module_path;
      emit_space state;
      emit_text state "in";
      emit_line state;
      render_expr state body
  | LetOpen _ -> unsupported_node "incomplete let-open expression" expr
  | Delimited { module_path = Some module_path; dot_token = Some dot_token; opening_token = Some opening; body = Some body; closing_token = Some closing } ->
      render_path state module_path;
      emit_token state dot_token;
      render_delimited_body opening body closing
  | Delimited { module_path = None; dot_token = Some dot_token; opening_token = Some opening; body = Some body; closing_token = Some closing } -> (
    let exprs = collect_child_exprs expr in
    if Int.equal (Vector.length exprs) 2 then
      (
        render_expr state (Vector.get_unchecked exprs ~at:0);
        emit_token state dot_token;
        render_delimited_body opening body closing
      )
    else unsupported_node "incomplete delimited local-open expression" expr
  )
  | Delimited _ -> unsupported_node "incomplete delimited local-open expression" expr
and token_vector_range_is_path = fun tokens ~start ~stop ->
  let rec loop index saw_ident expect_ident =
    if Int.(index >= stop) then
      saw_ident && not expect_ident
    else
      let token = Vector.get_unchecked tokens ~at:index in
      if token_kind_is token Kind.IDENT && expect_ident then
        loop (Int.add index 1) true false
      else
        if token_kind_is token Kind.DOT && saw_ident && not expect_ident then
          loop (Int.add index 1) saw_ident true
        else false
  in
  if Int.(start < stop) then
    loop start false true
  else false
and render_token_path_range = fun state node tokens ~start ~stop message ->
  if not (token_vector_range_is_path tokens ~start ~stop) then
    unsupported_node message node
  else
    let rec loop index =
      if Int.(index < stop) then
        (
          emit_token state (Vector.get_unchecked tokens ~at:index);
          loop (Int.add index 1)
        )
    in
    loop start
and render_parenthesized_val_module_body = fun state node ->
  let tokens = collect_child_tokens node in
  match token_vector_find_kind tokens Kind.LPAREN, token_vector_find_kind tokens Kind.VAL_KW, token_vector_find_kind tokens Kind.COLON, token_vector_find_kind tokens Kind.RPAREN with
  | Some opening_index, Some val_index, None, Some closing_index ->
      emit_token state (Vector.get_unchecked tokens ~at:opening_index);
      emit_token state (Vector.get_unchecked tokens ~at:val_index);
      emit_space state;
      render_token_path_range state node tokens ~start:(Int.add val_index 1) ~stop:closing_index "first-class module unpack without value path";
      emit_token state (Vector.get_unchecked tokens ~at:closing_index)
  | Some opening_index, Some val_index, Some colon_index, Some closing_index ->
      emit_token state (Vector.get_unchecked tokens ~at:opening_index);
      emit_token state (Vector.get_unchecked tokens ~at:val_index);
      emit_space state;
      render_token_path_range state node tokens ~start:(Int.add val_index 1) ~stop:colon_index "first-class module unpack without value path";
      emit_space state;
      emit_token state (Vector.get_unchecked tokens ~at:colon_index);
      emit_space state;
      emit_token_vector_range_stream state tokens ~start:(Int.add colon_index 1) ~stop:closing_index;
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
  Ast.Node.for_each_child_node node ~fn:(
    fun child ->
      match module_expr_specific_node child with
      | Some expr -> Vector.push exprs ~value:expr
      | None -> ()
  );
  exprs
and render_signature_body = fun state items ~end_token ->
  let has_terminal_trivia = token_has_leading_comment end_token in
  if Int.equal (Vector.length items) 0 then
    (
      emit_text state "sig";
      if has_terminal_trivia then
        (
          emit_line state;
          (
            match end_token with
            | Some token ->
                with_indent state 2
                  (
                    fun () -> emit_token_leading_comments_as_lines state token
                  )
            | None -> ()
          )
        )
      else emit_space state;
      emit_text state "end"
    )
  else
    (
      emit_text state "sig";
      emit_line state;
      with_indent state 2
        (
          fun () ->
            render_signature_items state items;
            match end_token with
            | Some token when has_terminal_trivia ->
                emit_line state;
                emit_line state;
                emit_token_leading_comments_as_lines state token
            | _ -> ()
        );
      if not has_terminal_trivia then
        emit_line state;
      emit_text state "end"
    )
and render_signature_module_type_node = fun state node ->
  let items = collect_signature_items_from_node node in render_signature_body state items ~end_token:(Ast.Node.first_child_token node ~kind:Kind.END_KW)
and render_module_typeof_node = fun state node ->
  emit_text state "module type of";
  emit_space state;
  let tokens = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_token node ~fn:(
    fun token ->
      if token_kind_is token Kind.IDENT || token_kind_is token Kind.DOT then
        Vector.push tokens ~value:token
  );
  if Int.equal (Vector.length tokens) 0 then
    unsupported_node "module type-of without path" node
  else emit_token_vector_stream state tokens
and render_module_type_constraint_node = fun state node ->
  match Ast.ModuleTypeConstraint.cast node with
  | None -> unsupported_node "unsupported module type constraint" node
  | Some constraint_ -> (
    match Ast.ModuleTypeConstraint.view constraint_ with
    | Type { path = Some path; operator = Some operator; body = Some body } ->
        emit_text state "type";
        emit_space state;
        render_path state path;
        emit_space state;
        emit_token state operator;
        emit_space state;
        render_type_expr state body
    | Type _ -> unsupported_node "incomplete module type constraint" node
    | Module { path = Some path; body = Some body } ->
        emit_text state "module";
        emit_space state;
        render_path state path;
        emit_space state;
        emit_text state "=";
        emit_space state;
        (
          match module_type_specific_node body with
          | Some module_type -> render_module_type_node state module_type
          | None -> unsupported_node "module constraint without module type body" body
        )
    | Module _ -> unsupported_node "incomplete module constraint" node
    | Unknown _ -> unsupported_node "unsupported module type constraint" node
  )
and render_with_module_type_node = fun state node ->
  let rendered_base = ref false in
  let pending_connector = ref None in
  Ast.Node.for_each_child node ~fn:(
    fun child ->
      match child with
      | Syn.SyntaxTree.Token id ->
          let token: Ast.Token.t = { tree = node.Ast.tree; id } in
          if token_kind_is token Kind.WITH_KW || token_kind_is token Kind.AND_KW then
            pending_connector := Some token
      | Syn.SyntaxTree.Node id ->
          let child: Ast.Node.t = { tree = node.Ast.tree; id } in
          if is_module_type_kind (node_kind child) && not !rendered_base then
            (
              render_module_type_node state child;
              rendered_base := true
            )
          else
            (
              match Ast.ModuleTypeConstraint.cast child with
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
      | Syn.SyntaxTree.Missing _ -> ()
  );
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
  if Int.equal (Vector.length items) 0 then
    (
      emit_text state "struct";
      emit_space state;
      emit_text state "end"
    )
  else
    (
      emit_text state "struct";
      emit_line state;
      with_indent state 2
        (
          fun () -> render_structure_items state items
        );
      emit_line state;
      emit_text state "end"
    )
and render_module_expr_atom_node = fun state node ->
  match module_expr_specific_node node with
  | Some node when node_kind_is node Kind.PATH_MODULE_EXPR || node_kind_is node Kind.STRUCT_MODULE_EXPR || node_kind_is node Kind.PAREN_MODULE_EXPR -> render_module_expr_node state node
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
    | Path -> render_module_body_path state (Ast.LetModuleExpr.for_each_module_body_path_ident module_expr)
    | EmptyStruct ->
        emit_text state "struct";
        emit_space state;
        emit_text state "end"
    | Unsupported -> unsupported_node "unsupported let module body" module_expr
  )
and render_let_module_expr = fun state expr ->
  let module_expr =
    match Ast.LetModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported_node "unsupported let module expression" expr
  in
  match Ast.LetModuleExpr.let_token module_expr, Ast.LetModuleExpr.module_token module_expr, Ast.LetModuleExpr.name module_expr, Ast.LetModuleExpr.equals_token module_expr, Ast.LetModuleExpr.in_token module_expr, Ast.LetModuleExpr.body module_expr with
  | Some let_token, Some module_token, Some name, Some equals_token, Some in_token, Some body ->
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
  for_each_path_ident ~fn:(
    fun ident -> Vector.push segments ~value:ident
  );
  if Vector.length segments = 0 then
    unsupported_node message expr
  else emit_joined_vector state segments ~sep:(
    fun () -> emit_text state "."
  ) ~fn:(emit_token state)
and render_first_class_module_ascription_tokens = fun state expr colon_token ->
  let tokens = collect_child_tokens expr in
  match token_vector_find_kind tokens Kind.COLON, token_vector_find_kind tokens Kind.RPAREN with
  | Some colon_index, Some closing_index when Int.(Int.add colon_index 1 < closing_index) ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      emit_token_vector_range_stream state tokens ~start:(Int.add colon_index 1) ~stop:closing_index
  | _ -> unsupported_node "unsupported first-class module ascription" expr
and render_first_class_module_ascription = fun state expr ->
  match Ast.FirstClassModuleExpr.colon_token expr, Ast.FirstClassModuleExpr.ascription expr with
  | None, Ast.FirstClassModuleExpr.NoAscription -> ()
  | Some colon_token, Ast.FirstClassModuleExpr.PathAscription ->
      emit_space state;
      emit_token state colon_token;
      emit_space state;
      render_first_class_module_path state expr (Ast.FirstClassModuleExpr.for_each_ascription_path_ident expr) "first-class module expression without module type path"
  | Some colon_token, Ast.FirstClassModuleExpr.UnsupportedAscription -> render_first_class_module_ascription_tokens state expr colon_token
  | (Some _, Ast.FirstClassModuleExpr.NoAscription) | (None, Ast.FirstClassModuleExpr.PathAscription) | (None, Ast.FirstClassModuleExpr.UnsupportedAscription) -> unsupported_node "unsupported first-class module ascription" expr
and render_first_class_module_expr = fun state expr ->
  let module_expr =
    match Ast.FirstClassModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported_node "unsupported first-class module expression" expr
  in
  match Ast.FirstClassModuleExpr.opening_token module_expr, Ast.FirstClassModuleExpr.closing_token module_expr with
  | Some opening_token, Some closing_token -> (
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
      match Ast.FirstClassModuleExpr.module_token module_expr, Ast.FirstClassModuleExpr.module_path module_expr with
      | Some module_token, Ast.FirstClassModuleExpr.ModulePath ->
          emit_token state module_token;
          emit_space state;
          render_first_class_module_path state module_expr (Ast.FirstClassModuleExpr.for_each_module_path_ident module_expr) "first-class module expression without module path";
          render_first_class_module_ascription state module_expr;
          emit_token state closing_token
      | _ -> unsupported_node "unsupported first-class module expression" expr
    )
  )
  | _ -> unsupported_node "incomplete first-class module expression" expr
and render_binding_operator_clause = fun state (clause: Ast.BindingOperatorExpr.clause) ->
  match clause.keyword, clause.operator with
  | Some keyword, Some operator ->
      emit_token state keyword;
      emit_token state operator;
      emit_space state;
      render_let_binding_tail state clause.binding
  | _ -> unsupported "incomplete binding operator clause"
and binding_operator_clause_is_multiline = fun (clause: Ast.BindingOperatorExpr.clause) ->
  match Ast.LetBinding.body clause.binding with
  | Some body -> expr_is_multiline body
  | None -> false
and render_binding_operator_expr = fun state expr ->
  let view =
    match Ast.BindingOperatorExpr.cast expr with
    | Some view -> view
    | None -> unsupported_node "unsupported binding operator expression" expr
  in
  let clauses = collect_binding_operator_clauses view in
  match Vector.length clauses, Ast.BindingOperatorExpr.in_token view, Ast.BindingOperatorExpr.body view with
  | 0, _, _ -> unsupported_node "binding operator expression without binding" expr
  | _, None, _ -> unsupported_node "binding operator expression without in" expr
  | _, _, None -> unsupported_node "binding operator expression without body" expr
  | length, Some in_token, Some body ->
      let multiline =
        if Int.(length > 1) then
          true
        else
          if expr_is_multiline body then
            true
          else
            let rec loop index =
              if Int.(index >= length) then
                false
              else
                if binding_operator_clause_is_multiline (Vector.get_unchecked clauses ~at:index) then
                  true
                else loop (Int.add index 1)
            in
            loop 0
      in
      if multiline then
        (
          let rec loop index =
            if Int.(index < length) then
              (
                render_binding_operator_clause state (Vector.get_unchecked clauses ~at:index);
                if Int.(index < Int.sub length 1) then
                  emit_line state;
                loop (Int.add index 1)
              )
          in
          loop 0;
          emit_line state;
          emit_token state in_token;
          emit_line state;
          render_keyword_body_expr state body
        )
      else
        (
          render_binding_operator_clause state (Vector.get_unchecked clauses ~at:0);
          emit_space state;
          emit_token state in_token;
          emit_space state;
          render_keyword_body_expr state body
        )
and let_binding_body_renders_inline = fun body ->
  match Ast.Expr.view body with
  | Fun { body = Some body } -> not (expr_is_multiline body)
  | Record | RecordUpdate -> record_expr_should_inline body
  | Function _ -> false
  | _ -> not (expr_is_multiline body)
and render_let_expr = fun state expr body ->
  let bindings = collect_let_bindings_from_expr expr in
  let rec_token = Ast.Node.first_child_token expr ~kind:Kind.REC_KW in
  let and_tokens = collect_child_tokens_of_kind expr Kind.AND_KW in
  let length = Vector.length bindings in
  let render_binding_head index binding =
    (
      if Int.equal index 0 then
        emit_text state "let"
      else
        (
          let and_index = Int.sub index 1 in
          let and_token =
            if Int.(and_index < Vector.length and_tokens) then
              Some (Vector.get_unchecked and_tokens ~at:and_index)
            else None
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
    render_let_binding_tail state binding
  in
  if Int.equal length 1 then
    (
      let binding = Vector.get_unchecked bindings ~at:0 in
      render_binding_head 0 binding;
      match Ast.LetBinding.body binding with
      | Some binding_body when let_binding_body_renders_inline binding_body ->
          emit_space state;
          emit_text state "in";
          if expr_is_multiline body then
            (
              emit_line state;
              render_in_body_expr state body
            )
          else
            (
              emit_space state;
              render_in_body_expr state body
            )
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
            render_binding_head index (Vector.get_unchecked bindings ~at:index);
            if Int.(index < Int.sub length 1) then
              emit_line state;
            loop (Int.add index 1)
          )
      in
      loop 0;
      emit_line state;
      emit_text state "in";
      emit_line state;
      render_in_body_expr state body
    )
and render_let_binding_tail = fun state binding ->
  let view = Ast.LetBinding.view binding in
  let annotation = Ast.LetBinding.type_annotation binding in
  let parameters = Vector.with_capacity ~size:(Ast.Node.child_count binding) in
  Ast.LetBinding.for_each_parameter binding ~fn:(
    fun param -> Vector.push parameters ~value:param
  );
  let parameter_count = Vector.length parameters in
  let loose_binding_annotation =
    if Int.(parameter_count > 0) then
      (
        let last_index = Int.sub parameter_count 1 in
        let last = Vector.get_unchecked parameters ~at:last_index in
        match loose_parameter_binding_annotation last with
        | Some annotation -> Some (last_index, annotation)
        | None -> None
      )
    else None
  in
  let pattern_contains_annotation annotation =
    match view.pattern with
    | Some pattern -> pattern_contains_type_annotation pattern annotation
    | None -> false
  in
  (
    match view.pattern with
    | Some pattern -> render_pattern state pattern
    | None -> unsupported_node "let binding without pattern" binding
  );
  let rec render_parameters index =
    if Int.(index < parameter_count) then
      (
        let parameter = Vector.get_unchecked parameters ~at:index in
        emit_space state;
        (
          match loose_binding_annotation with
          | Some (loose_index, _) when Int.equal index loose_index -> render_parameter_label_only state parameter
          | Some _ | None -> render_pattern state parameter
        );
        render_parameters (Int.add index 1)
      )
  in
  render_parameters 0;
  (
    match annotation, loose_binding_annotation with
    | Some annotation, Some _ ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        render_type_expr state annotation
    | None, Some (_, annotation) ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        emit_token_stream state annotation
    | Some annotation, None when not (pattern_contains_annotation annotation) ->
        emit_text state ":";
        emit_space state;
        render_type_expr state annotation
    | (Some _, None) | (None, None) -> ()
  );
  emit_space state;
  emit_text state "=";
  match view.body with
  | Some ({ Ast.id = _; _ } as body) when (
    match Ast.Expr.view body with
    | Record | Fun _ | Function _ -> true
    | _ -> false
  ) ->
      emit_space state;
      render_expr state body
  | Some body when expr_is_multiline body ->
      emit_line state;
      with_indent state 2
        (
          fun () -> render_expr state body
        )
  | Some body ->
      emit_space state;
      render_expr state body
  | None -> unsupported_node "let binding without body" binding
and render_record_expr = fun state ~inline expr ->
  let base = Ast.RecordExpr.base expr in
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  let render_inline () =
    emit_text state "{";
    emit_space state;
    (
      match base with
      | Some base ->
          render_expr state base;
          emit_space state;
          emit_text state "with";
          emit_space state
      | None -> ()
    );
    emit_joined_vector state fields ~sep:(
      fun () ->
        emit_text state ";";
        emit_space state
    ) ~fn:(render_record_field state ~inline:true);
    emit_space state;
    emit_text state "}"
  in
  if Option.is_none base && Int.equal length 0 then
    emit_text state "{}"
  else
    if Option.is_some base && Int.equal length 0 then
      unsupported_node "record update without fields" expr
    else
      if inline then
        render_inline ()
      else
        if record_expr_should_inline expr then
          render_inline ()
        else
          (
            emit_text state "{";
            emit_line state;
            with_indent state 2
              (
                fun () ->
                  (
                    match base with
                    | Some base ->
                        render_expr state base;
                        emit_space state;
                        emit_text state "with";
                        emit_line state
                    | None -> ()
                  );
                  let rec loop index =
                    if Int.(index < length) then
                      (
                        render_record_field state ~inline:false (Vector.get_unchecked fields ~at:index);
                        if Int.(index < Int.sub length 1) then
                          (
                            emit_text state ";";
                            emit_line state
                          );
                        loop (Int.add index 1)
                      )
                  in
                  loop 0
              );
            emit_line state;
            emit_text state "}"
          )
and render_record_field = fun state ~inline field ->
  (
    match field.Ast.RecordExpr.path with
    | Some path -> render_path state path
    | None -> unsupported_node "record field without path" field.node
  );
  match field.Ast.RecordExpr.value with
  | None -> ()
  | Some value ->
      emit_space state;
      emit_text state "=";
      let multiline_value =
        if inline then
          false
        else expr_is_multiline value
      in
      if multiline_value then
        (
          emit_space state;
          if expr_is_tuple value then
            render_expr_atom state value
          else render_expr state value
        )
      else
        (
          emit_space state;
          if expr_is_tuple value then
            render_expr_atom state value
          else render_expr state value
        )
and render_array_expr = fun state expr ->
  let items = collect_array_items expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    emit_text state "[||]"
  else
    if Int.equal length 1 then
      (
        emit_text state "[|";
        let item = Vector.get_unchecked items ~at:0 in
        (
          match Ast.Expr.view item with
          | Record -> render_record_expr state ~inline:true item
          | _ -> render_expr state item
        );
        emit_text state "|]"
      )
    else
      (
        emit_text state "[|";
        emit_line state;
        with_indent state 2
          (
            fun () ->
              let rec loop index =
                if Int.(index < length) then
                  (
                    let item = Vector.get_unchecked items ~at:index in
                    (
                      match Ast.Expr.view item with
                      | Record -> render_record_expr state ~inline:true item
                      | _ -> render_expr state item
                    );
                    emit_text state ";";
                    if Int.(index < Int.sub length 1) then
                      emit_line state;
                    loop (Int.add index 1)
                  )
              in
              loop 0
          );
        emit_line state;
        emit_text state "|]"
      )
and render_list_expr = fun state expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    emit_text state "[]"
  else
    if list_expr_should_inline expr then
      (
        emit_text state "[ ";
        emit_joined_vector state items ~sep:(
          fun () ->
            emit_text state ";";
            emit_space state
        ) ~fn:(
          fun item ->
            if tuple_expr_needs_parens_in_list_item item then
              render_expr_atom state item
            else render_expr state item
        );
        emit_space state;
        emit_text state "]"
      )
    else
      (
        emit_text state "[";
        emit_line state;
        with_indent state 2
          (
            fun () ->
              let rec loop index =
                if Int.(index < length) then
                  (
                    let item = Vector.get_unchecked items ~at:index in
                    if tuple_expr_needs_parens_in_list_item item then
                      render_expr_atom state item
                    else render_expr state item;
                    emit_text state ";";
                    if Int.(index < Int.sub length 1) then
                      emit_line state;
                    loop (Int.add index 1)
                  )
              in
              loop 0
          );
        emit_line state;
        emit_text state "]"
      )
and render_open_declaration = fun state decl ->
  emit_text state "open";
  emit_space state;
  let first = ref true in Ast.OpenDeclaration.for_each_path_ident decl ~fn:(
    fun ident ->
      if !first then
        first := false
      else emit_text state ".";
      emit_token state ident
  )
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
  (
    match Ast.RecordField.mutable_token field with
    | Some token ->
        emit_token state token;
        emit_space state
    | None -> ()
  );
  (
    match Ast.RecordField.name field with
    | Some name -> emit_token state name
    | None -> unsupported_node "record type field without name" field
  );
  emit_text state ":";
  emit_space state;
  (
    match Ast.RecordField.type_annotation field with
    | Some annotation -> render_type_expr state annotation
    | None -> unsupported_node "record type field without annotation" field
  );
  if not last then
    emit_text state ";"
and record_type_field_flat_width = fun field ->
  match Ast.RecordField.name field, Ast.RecordField.type_annotation field with
  | Some name, Some annotation -> (
    match type_expr_flat_width annotation with
    | Some annotation_width ->
        let mutable_width =
          match Ast.RecordField.mutable_token field with
          | Some token -> Int.add (Slice.length (Ast.Token.slice token)) 1
          | None -> 0
        in
        Some Int.(mutable_width + Slice.length (Ast.Token.slice name) + 2 + annotation_width)
    | None -> None
  )
  | _ -> None
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
              else 2
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
    else
      if node_has_leading_comment (Vector.get_unchecked fields ~at:index) then
        true
      else loop (Int.add index 1)
  in
  loop 0
and record_type_should_inline = fun state record_type fields ->
  if record_type_has_terminal_trivia record_type || record_type_fields_have_leading_comment fields then
    false
  else
    match record_type_flat_width fields with
    | Some width -> Int.(width <= state.width - state.column)
    | None -> false
and render_record_type_closing = fun state ~leading_trivia_indent record_type ->
  match Ast.RecordType.closing_token record_type with
  | Some closing ->
      if Ast.Token.has_leading_comment closing && Int.(leading_trivia_indent > 0) then
        (
          with_indent state leading_trivia_indent
            (
              fun () -> emit_token_leading_comments_as_lines state closing
            );
          if state.line_start then
            state.column <- state.indent
        )
      else emit_token_leading_comments_as_lines state closing;
      emit_token state closing
  | None -> emit_text state "}"
and render_record_type_inline = fun state record_type fields ->
  let length = Vector.length fields in
  if Int.equal length 0 then
    (
      emit_text state "{";
      render_record_type_closing state ~leading_trivia_indent:0 record_type
    )
  else
    (
      emit_text state "{";
      emit_space state;
      emit_joined_vector state fields ~sep:(
        fun () ->
          emit_text state ";";
          emit_space state
      ) ~fn:(render_record_type_field state ~last:true);
      emit_space state;
      render_record_type_closing state ~leading_trivia_indent:0 record_type
    )
and render_record_type = fun state record_type ->
  let fields = collect_record_type_fields record_type in
  (
    match Ast.RecordType.private_token record_type with
    | Some token ->
        emit_token state token;
        emit_space state
    | None -> ()
  );
  if record_type_should_inline state record_type fields then
    render_record_type_inline state record_type fields
  else
    (
      emit_text state "{";
      if Vector.length fields > 0 then
        (
          emit_line state;
          with_indent state 2
            (
              fun () ->
                let length = Vector.length fields in
                let rec loop index =
                  if Int.(index < length) then
                    (
                      render_record_type_field state ~last:false (Vector.get_unchecked fields ~at:index);
                      if Int.(index < Int.sub length 1) then
                        emit_line state;
                      loop (Int.add index 1)
                    )
                in
                loop 0
            );
          emit_line state
        );
      render_record_type_closing state ~leading_trivia_indent:2 record_type
    )
and render_external_declaration = fun state decl ->
  let name_tokens = collect_external_name_tokens decl in
  let primitive_strings = collect_external_primitive_strings decl in
  let attribute_tokens = collect_external_attribute_tokens decl in
  emit_text state "external";
  emit_space state;
  if Vector.length name_tokens = 0 then
    unsupported_node "external declaration without name" decl
  else emit_token_vector_stream state name_tokens;
  (
    match Ast.ExternalDeclaration.colon_token decl, Ast.ExternalDeclaration.type_annotation decl with
    | Some colon_token, Some annotation ->
        emit_token state colon_token;
        emit_space state;
        render_type_expr state annotation
    | _ -> unsupported_node "incomplete external declaration" decl
  );
  (
    match Ast.Node.first_child_token decl ~kind:Kind.EQ with
    | Some equals_token ->
        emit_space state;
        emit_token state equals_token;
        emit_space state
    | None -> unsupported_node "external declaration without primitive separator" decl
  );
  if Vector.length primitive_strings = 0 then
    unsupported_node "external declaration without primitive strings" decl
  else emit_joined_vector state primitive_strings ~sep:(
    fun () -> emit_space state
  ) ~fn:(emit_token state);
  if Vector.length attribute_tokens > 0 then
    (
      emit_space state;
      emit_token_vector_stream state attribute_tokens
    )
and render_exception_payload = fun state payload ->
  match payload with
  | Ast.ExceptionDeclaration.TypeExpr type_expr -> render_type_expr state type_expr
  | Ast.ExceptionDeclaration.Record record_type -> render_record_type state record_type
and render_exception_rhs = fun state rhs ->
  match rhs with
  | Ast.ExceptionDeclaration.Bare -> ()
  | Ast.ExceptionDeclaration.Alias { equals_token = Some equals_token; path = Some path } ->
      emit_space state;
      emit_token state equals_token;
      emit_space state;
      render_path state path
  | Ast.ExceptionDeclaration.Alias { equals_token = None; path = Some path } ->
      emit_space state;
      render_path state path
  | Ast.ExceptionDeclaration.Alias { equals_token = Some equals_token; path = None } ->
      emit_space state;
      emit_token state equals_token
  | Ast.ExceptionDeclaration.Alias { equals_token = None; path = None } -> ()
  | Ast.ExceptionDeclaration.Payload { of_token = Some of_token; payload = Some payload } ->
      emit_space state;
      emit_token state of_token;
      emit_space state;
      render_exception_payload state payload
  | Ast.ExceptionDeclaration.Payload { of_token = None; payload = Some payload } ->
      emit_space state;
      render_exception_payload state payload
  | Ast.ExceptionDeclaration.Payload { of_token = Some of_token; payload = None } ->
      emit_space state;
      emit_token state of_token
  | Ast.ExceptionDeclaration.Payload { of_token = None; payload = None } -> ()
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
and render_variant_constructor = fun state ~private_token constructor ->
  (
    match private_token with
    | Some token ->
        emit_token state token;
        emit_space state
    | None -> ()
  );
  (
    match Ast.VariantConstructor.pipe_token constructor with
    | Some pipe_token ->
        emit_token_leading_comments_as_lines state pipe_token;
        emit_token state pipe_token
    | None -> emit_text state "|"
  );
  emit_space state;
  (
    match Ast.VariantConstructor.name constructor with
    | Some name -> emit_token state name
    | None -> unsupported_node "variant constructor without name" constructor
  );
  (
    match Ast.VariantConstructor.colon_token constructor, Ast.VariantConstructor.record_payload constructor, Ast.VariantConstructor.payload_type constructor with
    | Some colon_token, Some record_type, _ ->
        emit_space state;
        emit_token state colon_token;
        emit_space state;
        render_record_type state record_type;
        (
          match Ast.Node.first_child_token constructor ~kind:Kind.ARROW, Ast.VariantConstructor.result_type constructor with
          | Some arrow_token, Some result ->
              emit_space state;
              emit_token state arrow_token;
              emit_space state;
              render_type_expr state result
          | None, Some result ->
              emit_space state;
              emit_text state "->";
              emit_space state;
              render_type_expr state result
          | Some arrow_token, None ->
              emit_space state;
              emit_token state arrow_token
          | None, None -> ()
        )
    | _, Some record_type, _ ->
        emit_space state;
        emit_text state "of";
        emit_space state;
        render_record_type state record_type
    | _, None, Some payload ->
        emit_space state;
        emit_text state "of";
        emit_space state;
        render_type_expr state payload
    | _, None, None -> ()
  );
  (
    match Ast.VariantConstructor.colon_token constructor, Ast.VariantConstructor.record_payload constructor, Ast.VariantConstructor.result_type constructor with
    | Some _, Some _, _ -> ()
    | _, _, Some result ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        render_type_expr state result
    | _, _, None -> ()
  )
and render_variant_type = fun state ~private_token variant_type ->
  let constructors = collect_variant_constructors variant_type in
  let length = Vector.length constructors in
  if Int.(length > 0) then
    (
      emit_line state;
      with_indent state 2
        (
          fun () ->
            let rec loop index =
              if Int.(index < length) then
                (
                  let constructor_private_token =
                    if Int.equal index 0 then
                      private_token
                    else None
                  in
                  render_variant_constructor state ~private_token:constructor_private_token (Vector.get_unchecked constructors ~at:index);
                  if Int.(index < Int.sub length 1) then
                    emit_line state;
                  loop (Int.add index 1)
                )
            in
            loop 0
        )
    )
and render_type_parameter = fun state parameter ->
  match parameter with
  | Ast.TypeDeclaration.Named { name; quote; variance; injective } ->
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
      if Int.(length > 4) then
        (
          emit_line state;
          with_indent state 2
            (
              fun () -> emit_joined_vector state parameters ~sep:(
                fun () ->
                  emit_text state ",";
                  emit_line state
              ) ~fn:(render_type_parameter state)
            );
          emit_line state
        )
      else emit_joined_vector state parameters ~sep:(
        fun () ->
          emit_text state ",";
          emit_space state
      ) ~fn:(render_type_parameter state);
      emit_text state ")";
      emit_space state
and type_member_has_equals = fun member ->
  let found = ref false in
  Ast.TypeDeclaration.Member.for_each_child_token member ~fn:(
    fun token ->
      if token_kind_is token Kind.EQ then
        found := true
  );
  !found
and type_member_private_token = fun member ->
  let saw_equals = ref false in
  let found = ref None in
  Ast.TypeDeclaration.Member.for_each_child_token member ~fn:(
    fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if !saw_equals then
            (
              if token_kind_is token Kind.PRIVATE_KW then
                found := Some token
            )
          else
            if token_kind_is token Kind.EQ then
              saw_equals := true
  );
  (
    match !found with
    | Some _ -> ()
    | None -> Ast.TypeDeclaration.Member.for_each_child_node member ~fn:(
      fun node ->
        match !found with
        | Some _ -> ()
        | None -> found := Ast.Node.first_child_token node ~kind:Kind.PRIVATE_KW
    )
  );
  !found
and type_member_is_alias_extensible = fun member ->
  let equals_count = ref 0 in
  let saw_dotdot_after_alias = ref false in
  Ast.TypeDeclaration.Member.for_each_child_token member ~fn:(
    fun token ->
      if token_kind_is token Kind.EQ then
        equals_count := Int.add !equals_count 1
      else
        if Int.((!equals_count) >= 2) && token_kind_is token Kind.DOTDOT then
          saw_dotdot_after_alias := true
  );
  !saw_dotdot_after_alias
and render_alias_extensible_suffix = fun state member ->
  if type_member_is_alias_extensible member then
    (
      emit_space state;
      emit_text state "=";
      emit_space state;
      emit_text state ".."
    )
and type_member_representation_equals_token = fun member ->
  let equals_seen = ref 0 in
  let found = ref None in
  Ast.TypeDeclaration.Member.for_each_child_token member ~fn:(
    fun token ->
      if Option.is_none !found && token_kind_is token Kind.EQ then
        (
          equals_seen := Int.add !equals_seen 1;
          if Int.equal !equals_seen 2 then
            found := Some token
        )
  );
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
  Ast.TypeDeclaration.Member.for_each_child member ~fn:(
    function
    | Syn.SyntaxTree.Token id ->
        let token: Ast.Token.t = { tree; id } in
        if !collecting then
          Vector.push tokens ~value:token
        else
          if token_kind_is token Kind.EQ then
            collecting := true
    | Syn.SyntaxTree.Node id ->
        if !collecting then
          let node: Ast.Node.t = { tree; id } in Ast.Node.for_each_token node ~fn:(
            fun token -> Vector.push tokens ~value:token
          )
    | Syn.SyntaxTree.Missing _ -> ()
  );
  tokens
and render_private_alias_type_member_body = fun state member ->
  match type_member_private_token member with
  | None -> false
  | Some _ ->
      let body_tokens = collect_type_member_body_tokens member in
      if Int.equal (Vector.length body_tokens) 0 then
        false
      else
        (
          emit_space state;
          emit_text state "=";
          emit_space state;
          emit_token_vector_stream state body_tokens;
          true
        )
and render_type_attribute_items = fun state attributes ->
  let length = Vector.length attributes in
  if Int.(length > 0) then
    (
      emit_space state;
      let rec loop index =
        if Int.(index < length) then
          (
            if Int.(index > 0) then
              emit_space state;
            render_attribute_item state (Vector.get_unchecked attributes ~at:index);
            loop (Int.add index 1)
          )
      in
      loop 0
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
    match Ast.TypeDeclaration.Member.variant_type member, Ast.TypeDeclaration.Member.record_type member, Ast.TypeDeclaration.Member.manifest member with
    | Some variant_type, _, Some manifest ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest;
        render_type_member_representation_separator state member;
        render_variant_type state ~private_token:(type_member_private_token member) variant_type
    | Some variant_type, _, None ->
        emit_space state;
        emit_text state "=";
        render_variant_type state ~private_token:(type_member_private_token member) variant_type
    | None, Some record_type, Some manifest ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest;
        render_type_member_representation_separator state member;
        emit_space state;
        render_record_type state record_type
    | None, Some record_type, None ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_record_type state record_type
    | None, None, Some manifest ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest;
        render_alias_extensible_suffix state member
    | None, None, None when type_member_has_equals member ->
        if not (render_private_alias_type_member_body state member) then
          unsupported "unsupported type declaration body"
    | None, None, None -> ()
  );
  let suffix_tokens = collect_type_member_attribute_suffix_tokens member in
  if Int.(Vector.length suffix_tokens > 0) then
    (
      emit_space state;
      emit_token_vector_stream state suffix_tokens
    );
  render_type_attribute_items state (collect_type_member_attribute_items member)
and render_type_declaration = fun state decl ->
  let members = collect_type_members decl in
  let length = Vector.length members in
  let single_pseudo_member = Int.equal length 1 && Int.equal (Ast.TypeDeclaration.Member.start_index (Vector.get_unchecked members ~at:0)) 0 && Int.equal (Ast.TypeDeclaration.Member.stop_index (Vector.get_unchecked members ~at:0)) (Ast.Node.child_count decl) in
  let rec loop index =
    if Int.(index < length) then
      (
        if Int.(index > 0) then
          emit_line state;
        render_type_member state (Vector.get_unchecked members ~at:index);
        loop (Int.add index 1)
      )
  in
  loop 0;
  if not single_pseudo_member then
    render_type_attribute_items state (collect_type_declaration_attribute_items decl)
and render_type_extension_declaration = fun state decl ->
  (
    match Ast.TypeExtensionDeclaration.keyword_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state "type"
  );
  emit_space state;
  render_type_parameters state (collect_type_extension_parameters decl);
  let name_tokens = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeExtensionDeclaration.for_each_name_ident decl ~fn:(
    fun token -> Vector.push name_tokens ~value:token
  );
  if Vector.length name_tokens = 0 then
    unsupported_node "type extension declaration without name" decl
  else emit_joined_vector state name_tokens ~sep:(
    fun () -> emit_text state "."
  ) ~fn:(emit_token state);
  emit_space state;
  (
    match Ast.TypeExtensionDeclaration.plus_token decl, Ast.TypeExtensionDeclaration.equals_token decl with
    | Some plus_token, Some equals_token ->
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
  let first = ref true in for_each_path_ident ~fn:(
    fun ident ->
      if !first then
        first := false
      else emit_text state ".";
      emit_token state ident
  )
and render_module_declaration_separator = fun state decl ~default ->
  emit_space state;
  (
    match Ast.ModuleDeclaration.separator_token decl with
    | Some token -> emit_token state token
    | None -> emit_text state default
  );
  emit_space state
and render_module_typeof_body = fun state decl ->
  render_module_declaration_separator state decl ~default:":";
  emit_text state "module type of";
  emit_space state;
  render_module_body_path state (Ast.ModuleDeclaration.for_each_typeof_body_path_ident decl)
and render_value_declaration = fun state decl ->
  let name_tokens = collect_value_name_tokens decl in
  emit_text state "val";
  emit_space state;
  if Vector.length name_tokens = 0 then
    unsupported_node "value declaration without name" decl
  else
    if token_vector_is_parenthesized_operator_name name_tokens then
      render_parenthesized_operator_tokens state name_tokens
    else emit_token_vector_stream state name_tokens;
  emit_text state ":";
  emit_space state;
  (
    match Ast.ValueDeclaration.type_annotation decl with
    | Some annotation -> render_type_expr state annotation
    | None -> unsupported_node "value declaration without annotation" decl
  )
and render_let_declaration = fun state decl ->
  let bindings = collect_let_bindings decl in
  let rec_token = Ast.LetDeclaration.rec_token decl in
  let and_tokens = collect_child_tokens_of_kind decl Kind.AND_KW in
  let length = Vector.length bindings in
  let rec loop index =
    if Int.(index < length) then
      (
        (
          if Int.equal index 0 then
            emit_text state "let"
          else
            (
              let and_index = Int.sub index 1 in
              let and_token =
                if Int.(and_index < Vector.length and_tokens) then
                  Some (Vector.get_unchecked and_tokens ~at:and_index)
                else None
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
        render_let_binding_tail state (Vector.get_unchecked bindings ~at:index);
        if Int.(index < Int.sub length 1) then
          emit_line state;
        loop (Int.add index 1)
      )
  in
  loop 0
and render_module_declaration_member = fun state member ->
  let previous_token = ref None in
  let previous_node = ref false in
  let emit_member_token token =
    (
      match !previous_token, !previous_node with
      | _, true -> emit_space state
      | Some previous, false when token_wants_space_before previous token -> emit_space state
      | (Some _, false) | (None, false) -> ()
    );
    emit_token state token;
    previous_token := Some token;
    previous_node := false
  in
  let emit_member_node render node =
    (
      match !previous_token, !previous_node with
      | (Some _, _) | (None, true) -> emit_space state
      | None, false -> ()
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
      if Int.(index < length) then
        (
          if Int.(index > 0) then
            emit_line state;
          render_module_declaration_member state (Vector.get_unchecked members ~at:index);
          loop (Int.add index 1)
        )
    in
    loop 0
and module_declaration_has_ascribed_body = fun decl ->
  let found = ref false in
  Ast.ModuleDeclaration.for_each_member decl ~fn:(
    fun member ->
      match Ast.ModuleDeclaration.Member.module_type member, Ast.ModuleDeclaration.Member.module_expr member with
      | Some _, Some _ -> found := true
      | _ -> ()
  );
  !found
and module_declaration_has_head_parameter = fun decl ->
  let found = ref false in
  Ast.ModuleDeclaration.for_each_member decl ~fn:(
    fun member ->
      let rec loop index =
        if not !found && Int.(index < Ast.ModuleDeclaration.Member.child_count member) then
          (
            match Ast.ModuleDeclaration.Member.child_token_at member index with
            | Some token when token_kind_is token Kind.EQ || token_kind_is token Kind.COLON -> ()
            | Some token when token_kind_is token Kind.LPAREN -> found := true
            | Some _ | None -> loop (Int.add index 1)
          )
      in
      loop 0
  );
  !found
and render_module_declaration = fun state decl ->
  if module_declaration_has_ascribed_body decl || module_declaration_has_head_parameter decl then
    render_module_declaration_members state decl
  else
    match Ast.ModuleDeclaration.body decl with
    | Unsupported when not (Ast.ModuleDeclaration.has_typeof_body decl) -> render_module_declaration_members state decl
    | body ->
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
        (
          match body with
          | Path ->
              render_module_declaration_separator state decl ~default:"=";
              render_module_body_path state (Ast.ModuleDeclaration.for_each_body_path_ident decl);
              render_wrapped_module_body_attribute_suffix state decl
          | EmptyStruct ->
              emit_space state;
              emit_text state "=";
              emit_space state;
              emit_text state "struct";
              emit_space state;
              emit_text state "end";
              render_wrapped_module_body_attribute_suffix state decl
          | Struct ->
              emit_space state;
              emit_text state "=";
              emit_space state;
              emit_text state "struct";
              emit_line state;
              with_indent state 2
                (
                  fun () ->
                    let first = ref true in Ast.ModuleDeclaration.for_each_structure_item decl ~fn:(
                      fun item ->
                        if !first then
                          first := false
                        else
                          (
                            emit_line state;
                            emit_line state
                          );
                        render_structure_item state item
                    )
                );
              emit_line state;
              emit_text state "end";
              render_wrapped_module_body_attribute_suffix state decl
          | EmptySig ->
              emit_space state;
              emit_text state ":";
              emit_space state;
              render_signature_body state (Vector.with_capacity ~size:0) ~end_token:(Ast.ModuleDeclaration.end_token decl);
              render_wrapped_module_body_attribute_suffix state decl
          | Sig ->
              emit_space state;
              emit_text state ":";
              emit_space state;
              let items = collect_signature_items_from_module_decl decl in
              render_signature_body state items ~end_token:(Ast.ModuleDeclaration.end_token decl);
              render_wrapped_module_body_attribute_suffix state decl
          | Unsupported ->
              if Ast.ModuleDeclaration.has_typeof_body decl then
                render_module_typeof_body state decl
              else render_module_declaration_members state decl
        )
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
    | Path ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_module_body_path state (Ast.ModuleTypeDeclaration.for_each_body_path_ident decl);
        render_wrapped_module_body_attribute_suffix state decl
    | EmptySig ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_signature_body state (Vector.with_capacity ~size:0) ~end_token:(Ast.ModuleTypeDeclaration.end_token decl);
        render_wrapped_module_body_attribute_suffix state decl
    | Sig ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        let items = collect_signature_items_from_module_type_decl decl in
        render_signature_body state items ~end_token:(Ast.ModuleTypeDeclaration.end_token decl);
        render_wrapped_module_body_attribute_suffix state decl
    | With | Unsupported -> unsupported_node "unsupported module type declaration body" decl
  )
and render_structure_items = fun state items ->
  let length = Vector.length items in
  let rec loop index =
    if Int.(index < length) then
      (
        let item = Vector.get_unchecked items ~at:index in
        render_structure_item state item;
        if Int.(index < Int.sub length 1) then
          if structure_item_is_attribute (Vector.get_unchecked items ~at:(Int.add index 1)) then
            emit_space state
          else
            if structure_items_join_tightly item (Vector.get_unchecked items ~at:(Int.add index 1)) then
              emit_line state
            else
              (
                emit_line state;
                emit_line state
              );
        loop (Int.add index 1)
      )
  in
  loop 0
and render_signature_items = fun state items ->
  let length = Vector.length items in
  let rec loop index =
    if Int.(index < length) then
      (
        let item = Vector.get_unchecked items ~at:index in
        render_signature_item state item;
        if Int.(index < Int.sub length 1) then
          if signature_item_is_attribute (Vector.get_unchecked items ~at:(Int.add index 1)) then
            emit_space state
          else
            if signature_items_join_tightly item (Vector.get_unchecked items ~at:(Int.add index 1)) then
              emit_line state
            else
              (
                emit_line state;
                emit_line state
              );
        loop (Int.add index 1)
      )
  in
  loop 0
and render_attribute_item = fun state item -> emit_token_stream state item
and collect_item_attribute_suffixes = fun item declaration ->
  let attributes = Vector.with_capacity ~size:(Ast.Node.child_count item) in
  let after_declaration = ref false in
  Ast.Node.for_each_child_node item ~fn:(
    fun child ->
      if !after_declaration then
        (
          if node_kind_is child Kind.ATTRIBUTE_ITEM then
            Vector.push attributes ~value:child
        )
      else
        if same_ast_node child declaration then
          after_declaration := true
  );
  attributes
and render_item_attribute_suffixes = fun state attributes ->
  let length = Vector.length attributes in
  let rec loop index =
    if Int.(index < length) then
      (
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
and structure_item_is_attribute = fun item ->
  match Ast.StructureItem.view item with
  | Attribute _ -> true
  | _ -> false
and signature_item_is_open = fun item ->
  match Ast.SignatureItem.view item with
  | Open _ -> true
  | _ -> false
and structure_item_is_open = fun item ->
  match Ast.StructureItem.view item with
  | Open _ -> true
  | _ -> false
and signature_items_join_tightly = fun left right -> signature_item_is_open left && signature_item_is_open right
and structure_items_join_tightly = fun left right -> structure_item_is_open left && structure_item_is_open right
and render_signature_item = fun state item ->
  emit_top_level_leading state item;
  let attribute_suffixes =
    match Ast.SignatureItem.declaration item with
    | Some declaration -> collect_item_attribute_suffixes item declaration
    | None -> Vector.with_capacity ~size:0
  in
  (
    match Ast.SignatureItem.view item with
    | Value decl -> render_value_declaration state decl
    | Type decl -> render_type_declaration state decl
    | TypeExtension decl -> render_type_extension_declaration state decl
    | Module decl -> render_module_declaration state decl
    | ModuleType decl -> render_module_type_declaration state decl
    | Open decl -> render_open_declaration state decl
    | Include decl -> render_include_declaration state decl
    | External decl -> render_external_declaration state decl
    | Exception decl -> render_exception_declaration state decl
    | Attribute item -> render_attribute_item state item
    | Class _ | Extension _ | Error _ | Unknown _ -> unsupported_node "unsupported signature item" item
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
    | Type decl -> render_type_declaration state decl
    | Let decl -> render_let_declaration state decl
    | Module decl -> render_module_declaration state decl
    | ModuleType decl -> render_module_type_declaration state decl
    | Include decl -> render_include_declaration state decl
    | External decl -> render_external_declaration state decl
    | Exception decl -> render_exception_declaration state decl
    | TypeExtension decl -> render_type_extension_declaration state decl
    | Attribute item -> render_attribute_item state item
    | Expr expr_item -> (
      match Ast.ExprItem.expr expr_item with
      | Some expr -> render_expr state expr
      | None -> unsupported_node "expr item without expression" item
    )
    | Class _ | Extension _ | Error _ | Unknown _ -> unsupported_node "unsupported structure item" item
  );
  render_item_attribute_suffixes state attribute_suffixes

let render_interface = fun state interface -> render_signature_items state (collect_signature_items_from_node interface)

let render_implementation = fun state implementation -> render_structure_items state (collect_structure_items_from_node implementation)

let render_source_file = fun state source_file ->
  match Ast.SourceFile.view source_file with
  | Empty -> ()
  | Implementation implementation -> render_implementation state implementation
  | Interface interface -> render_interface state interface

let emit_source_file_trailing = fun state source_file ->
  match Ast.Node.first_child_token source_file ~kind:Kind.EOF with
  | Some token when Ast.Token.has_leading_comment token -> emit_token_leading_comments_as_lines state token
  | Some _ | None -> ()

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
    suppress_leading_token = None;
    delimited_expr_depth = 0
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
      | None -> IO.flush writer |> Result.map_err ~fn:(
        fun error -> Cannot_write error
      )
    )
    | Error error -> Error (Cannot_write error)
  with
  | Unsupported err -> Error (Cannot_format err)
