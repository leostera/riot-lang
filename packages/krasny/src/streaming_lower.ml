open Std
open Std.Collections
module Ast = Syn.Ast2
module Kind = Syn.SyntaxKind2
module Slice = IO.IoVec.IoSlice

type error = {
  message: string;
}

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

type state = {
  buffer: IO.Buffer.t;
  sink: sink;
  mutable line_start: bool;
  mutable column: int;
  mutable indent: int;
  mutable wrote: bool;
}

let append_subslice_unchecked = fun buffer slice ~off ~len ->
  match IO.Buffer.append_subslice buffer slice ~off ~len with
  | Ok () -> ()
  | Error error -> panic ("Streaming_lower.append_subslice: " ^ Kernel.IO.Error.message error)

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
    if Int.compare index 0 < 0 then
      length
    else if Char.equal (String.get_unchecked text ~at:index) '\n' then
      Int.sub (Int.sub length index) 1
    else
      loop (Int.sub index 1)
  in
  loop (Int.sub length 1)

let last_slice_line_width = fun slice ->
  let rec find_last_newline index last_newline =
    if Int.(index >= Slice.length slice) then
      last_newline
    else if Char.equal (Slice.get_unchecked slice ~at:index) '\n' then
      find_last_newline Int.(index + 1) index
    else
      find_last_newline Int.(index + 1) last_newline
  in
  let last_newline = find_last_newline 0 (-1) in
  if Int.(last_newline < 0) then
    Slice.length slice
  else
    Int.(Slice.length slice - last_newline - 1)

let write_indent = fun state ->
  let rec loop remaining =
    if Int.compare remaining 0 > 0 then
      (
        IO.Buffer.add_char state.buffer ' ';
        loop (Int.sub remaining 1)
      )
  in
  loop state.indent

let write_string_segment = fun state value ~off ~len ->
  if state.line_start && Int.compare len 0 > 0 then
    write_indent state;
  IO.Buffer.add_substring state.buffer value off len;
  flush_if_needed state;
  if Int.compare len 0 > 0 then
    state.wrote <- true;
  state.line_start <- state.line_start && Int.equal len 0

let write_slice_segment = fun state value ~off ~len ->
  if state.line_start && Int.compare len 0 > 0 then
    write_indent state;
  append_subslice_unchecked state.buffer value ~off ~len;
  flush_if_needed state;
  if Int.compare len 0 > 0 then
    state.wrote <- true;
  state.line_start <- state.line_start && Int.equal len 0

let emit_line = fun state ->
  IO.Buffer.add_char state.buffer '\n';
  flush_if_needed state;
  state.wrote <- true;
  state.line_start <- true;
  state.column <- state.indent

let emit_text = fun state value ->
  let length = String.length value in
  if Int.compare length 0 > 0 then
    let rec loop segment_start index saw_newline =
      if Int.(index >= length) then
        (
          write_string_segment state value ~off:segment_start ~len:Int.(length - segment_start);
          state.column <- if saw_newline then
            Int.sub length segment_start
          else
            state.column + length
        )
      else if Char.equal (String.get_unchecked value ~at:index) '\n' then
        (
          write_string_segment state value ~off:segment_start ~len:Int.(index - segment_start);
          IO.Buffer.add_char state.buffer '\n';
          flush_if_needed state;
          state.wrote <- true;
          state.line_start <- true;
          loop Int.(index + 1) Int.(index + 1) true
        )
      else
        loop segment_start Int.(index + 1) saw_newline
    in
    loop 0 0 false

let emit_raw_text = fun state value ->
  let length = String.length value in
  if Int.compare length 0 > 0 then
    (
      if state.line_start then
        write_indent state;
      IO.Buffer.add_string state.buffer value;
      flush_if_needed state;
      state.wrote <- true;
      state.line_start <- Char.equal (String.get_unchecked value ~at:Int.(length - 1)) '\n';
      state.column <- if String.contains value "\n" then
        last_line_width value
      else
        state.column + length
    )

let emit_slice = fun state ~has_newline value ->
  let length = Slice.length value in
  if Int.compare length 0 > 0 then
    if has_newline then
      (
        if state.line_start then
          write_indent state;
        append_subslice_unchecked state.buffer value ~off:0 ~len:length;
        flush_if_needed state;
        state.wrote <- true;
        state.line_start <- Char.equal (Slice.get_unchecked value ~at:Int.(length - 1)) '\n';
        state.column <- last_slice_line_width value
      )
    else (
      write_slice_segment state value ~off:0 ~len:length;
      state.column <- state.column + length
    )

let emit_space = fun state ->
  if state.line_start then
    state.column <- state.column + 1
  else (
    IO.Buffer.add_char state.buffer ' ';
    flush_if_needed state;
    state.wrote <- true;
    state.column <- state.column + 1
  )

let emit_spaces = fun state count ->
  let rec loop remaining =
    if Int.compare remaining 0 > 0 then
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

let emit_token = fun state token ->
  emit_slice state ~has_newline:(Ast.Token.has_newline token) (Ast.Token.slice token)

let emit_optional_token = fun state token ->
  match token with
  | Some token -> emit_token state token
  | None -> ()

let emit_top_level_leading = fun state node ->
  match Ast.Node.first_descendant_token node with
  | Some token ->
      let leading = Ast.Token.leading_text token in
      if not (String.is_empty (String.trim leading)) then
        emit_raw_text state leading
  | None -> ()

let emit_keyword = emit_text

let token_text = Ast.Token.text

let token_text_is = Ast.Token.text_is

let node_kind = Ast.Node.kind

let token_kind_is = fun token kind -> Kind.(Ast.Token.kind token = kind)

let node_has_token_kind = fun node kind ->
  let found = ref false in
  Ast.Node.for_each_token node
    ~fn:(fun token ->
      if not !found && token_kind_is token kind then
        found := true);
  !found

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if token_kind_is token Kind.IDENT then
            found := Some token);
  !found

let token_wants_space_before = fun previous token ->
  let current_kind = Ast.Token.kind token in
  let previous_kind = Ast.Token.kind previous in
  not
    (Kind.(current_kind = RPAREN)
    || Kind.(current_kind = RBRACKET)
    || Kind.(current_kind = RBRACE)
    || Kind.(current_kind = COMMA)
    || Kind.(current_kind = SEMI)
    || Kind.(current_kind = DOT)
    || Kind.(previous_kind = LPAREN)
    || Kind.(previous_kind = LBRACKET)
    || Kind.(previous_kind = LBRACE)
    || Kind.(previous_kind = DOT)
    || Kind.(previous_kind = TILDE)
    || Kind.(previous_kind = QUESTION))

let emit_token_stream = fun state node ->
  let previous = ref None in
  Ast.Node.for_each_token node
    ~fn:(fun token ->
      (
        match !previous with
        | Some previous when token_wants_space_before previous token -> emit_space state
        | Some _
        | None -> ()
      );
      emit_token state token;
      previous := Some token)

let unsupported_node = fun label node -> unsupported (label ^ ": " ^ Kind.to_string (node_kind node))

let collect_child_exprs = fun (node: Ast.Node.t) ->
  let exprs = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.Expr.cast child with
      | Some expr -> Vector.push exprs ~value:expr
      | None -> ());
  exprs

let collect_child_patterns = fun (node: Ast.Node.t) ->
  let patterns = Vector.with_capacity ~size:(Ast.Node.child_count node) in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.Pattern.cast child with
      | Some pattern -> Vector.push patterns ~value:pattern
      | None -> ());
  patterns

let collect_record_fields = fun record ->
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record) in
  Ast.RecordExpr.for_each_field record ~fn:(fun field -> Vector.push fields ~value:field);
  fields

let collect_array_items = collect_child_exprs

let collect_type_members = fun decl ->
  let members = Vector.with_capacity ~size:(Ast.Node.child_count decl) in
  Ast.TypeDeclaration.for_each_member decl ~fn:(fun member -> Vector.push members ~value:member);
  members

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
  Ast.Node.for_each_child_node expr
    ~fn:(fun child ->
      match Ast.LetBinding.cast child with
      | Some binding -> Vector.push bindings ~value:binding
      | None -> ());
  bindings

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
  let fields = Vector.with_capacity ~size:(Ast.Node.child_count record) in
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

let emit_joined_vector = fun state values ~sep ~fn ->
  let length = Vector.length values in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        if Int.compare index 0 > 0 then
          sep ();
        fn (Vector.get_unchecked values ~at:index);
        loop (Int.add index 1)
      )
  in
  loop 0

let render_path = fun state path -> Ast.Node.for_each_token path ~fn:(emit_token state)

let rec render_type_expr = fun state type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Path { path } ->
      render_path state path
  | Var { name=Some name } ->
      emit_text state "'";
      emit_token state name
  | Var { name=None } ->
      unsupported_node "type variable without name" type_expr
  | Wildcard ->
      emit_text state "_"
  | Arrow { left=Some left; right=Some right } ->
      render_type_arrow_left state left;
      emit_space state;
      emit_text state "->";
      emit_space state;
      render_type_expr state right
  | Arrow _ ->
      unsupported_node "incomplete arrow type" type_expr
  | Apply { argument=Some argument; constructor=Some constructor } ->
      render_type_expr state argument;
      emit_space state;
      render_type_expr state constructor
  | Apply _ ->
      unsupported_node "incomplete apply type" type_expr
  | Parenthesized { inner=Some inner } ->
      emit_text state "(";
      render_type_expr state inner;
      emit_text state ")"
  | Parenthesized { inner=None } ->
      emit_text state "()"
  | Tuple { left=Some left; right=Some right; separator } ->
      render_type_expr state left;
      emit_space state;
      (
        match separator with
        | Star -> emit_text state "*"
        | Comma -> emit_text state ","
        | UnknownSeparator -> emit_text state "*"
      );
      emit_space state;
      render_type_expr state right
  | Tuple _ ->
      unsupported_node "incomplete tuple type" type_expr
  | Poly { body=Some body } ->
      render_type_expr state body
  | Poly { body=None } ->
      unsupported_node "poly type without body" type_expr
  | Labeled { optional_token; label=Some label; annotation=Some annotation } ->
      emit_optional_token state optional_token;
      emit_token state label;
      emit_text state ":";
      render_type_expr state annotation
  | Labeled _ ->
      unsupported_node "incomplete labeled type" type_expr
  | Opaque _
  | Error _
  | Unknown _ ->
      unsupported_node "unsupported type expression" type_expr

and render_type_arrow_left = fun state type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Arrow _ ->
      emit_text state "(";
      render_type_expr state type_expr;
      emit_text state ")"
  | _ -> render_type_expr state type_expr

let rec render_pattern = fun state pattern ->
  match Ast.Pattern.view pattern with
  | Wildcard ->
      emit_text state "_"
  | Path { path } ->
      render_path state path
  | Literal { token=Some token } ->
      emit_token state token
  | Literal { token=None } ->
      unsupported_node "literal pattern without token" pattern
  | Apply { callee=Some callee; argument=Some argument } ->
      render_pattern state callee;
      emit_space state;
      render_pattern_atom state argument
  | Apply _ ->
      unsupported_node "incomplete apply pattern" pattern
  | Parenthesized { inner=Some inner } ->
      emit_text state "(";
      render_pattern state inner;
      emit_text state ")"
  | Parenthesized { inner=None } ->
      emit_text state "()"
  | Constraint { pattern=Some pattern; annotation=Some annotation } ->
      render_pattern state pattern;
      emit_text state ":";
      emit_space state;
      render_type_expr state annotation
  | Constraint _ ->
      unsupported_node "incomplete constraint pattern" pattern
  | Alias { pattern=Some pattern; alias=Some alias } ->
      render_pattern state pattern;
      emit_space state;
      emit_text state "as";
      emit_space state;
      render_pattern state alias
  | Alias _ ->
      unsupported_node "incomplete alias pattern" pattern
  | Or { left=Some left; right=Some right } ->
      render_pattern state left;
      emit_space state;
      emit_text state "|";
      emit_space state;
      render_pattern state right
  | Or _ ->
      unsupported_node "incomplete or pattern" pattern
  | Tuple ->
      let items = collect_child_patterns pattern in
      emit_joined_vector state items
        ~sep:(fun () ->
          emit_text state ",";
          emit_space state)
        ~fn:(render_pattern state)
  | List ->
      let items = collect_child_patterns pattern in
      if Int.equal (Vector.length items) 0 then
        emit_text state "[]"
      else (
        emit_text state "[ ";
        emit_joined_vector state items
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
      emit_joined_vector state items
        ~sep:(fun () ->
          emit_text state ";";
          emit_space state)
        ~fn:(render_pattern state);
      emit_text state "|]"
  | Record ->
      render_record_pattern state pattern
  | PolyVariant ->
      render_poly_variant_pattern state pattern
  | Interval { left=Some left; right=Some right } ->
      render_pattern state left;
      emit_space state;
      emit_text state "..";
      emit_space state;
      render_pattern state right
  | Interval _ ->
      unsupported_node "incomplete interval pattern" pattern
  | Cons { head=Some head; tail=Some tail } ->
      render_pattern_atom state head;
      emit_space state;
      emit_text state "::";
      emit_space state;
      render_pattern state tail
  | Cons _ ->
      unsupported_node "incomplete cons pattern" pattern
  | Lazy { pattern=Some pattern } ->
      emit_text state "lazy";
      emit_space state;
      render_pattern state pattern
  | Lazy _ ->
      unsupported_node "lazy pattern without payload" pattern
  | Exception { pattern=Some pattern } ->
      emit_text state "exception";
      emit_space state;
      render_pattern state pattern
  | Exception _ ->
      unsupported_node "exception pattern without payload" pattern
  | LabeledParam _
  | OptionalParam _
  | OptionalParamDefault _ ->
      emit_token_stream state pattern
  | LocallyAbstractType
  | FirstClassModule ->
      emit_token_stream state pattern
  | Extension
  | Attribute _
  | LocalOpen
  | Error _
  | Unknown _ ->
      unsupported_node "unsupported pattern" pattern

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
  | 0 ->
      ()
  | 1 ->
      emit_space state;
      render_pattern state (Vector.get_unchecked children ~at:0)
  | _ ->
      unsupported_node "polymorphic variant pattern with multiple payloads" pattern

and render_record_pattern = fun state pattern ->
  let fields = collect_record_pattern_fields pattern in
  let length = Vector.length fields in
  emit_text state "{";
  if Int.compare length 0 > 0 then
    (
      emit_space state;
      emit_joined_vector state fields
        ~sep:(fun () ->
          emit_text state ";";
          emit_space state)
        ~fn:(fun field ->
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
          | None -> render_pattern state field.Ast.RecordPattern.node);
      (
        match Ast.RecordPattern.open_wildcard pattern with
        | Some _ ->
            if Int.compare length 0 > 0 then
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
  | Apply _
  | Or _
  | Cons _
  | Alias _
  | Constraint _ ->
      emit_text state "(";
      render_pattern state pattern;
      emit_text state ")"
  | _ -> render_pattern state pattern

let expr_first_token_text_is = fun expr expected ->
  match Ast.Node.first_descendant_token expr with
  | Some token -> token_text_is token expected
  | None -> false

let collect_apply = fun expr ->
  let args = Vector.with_capacity ~size:4 in
  let rec loop expr =
    match Ast.Expr.view expr with
    | Apply { callee=Some callee; argument=Some argument } ->
        Vector.push args ~value:argument;
        loop callee
    | _ -> expr
  in
  let callee = loop expr in
  Vector.reverse args;
  (callee, args)

let rec expr_is_multiline = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Fun _
  | Function _
  | Try _
  | While _
  | For _ -> true
  | Record -> not (record_expr_should_inline expr)
  | Parenthesized { inner=Some inner } when expr_first_token_text_is expr "begin" -> expr_is_multiline
    inner
  | Array -> not (array_expr_should_inline expr)
  | List -> not (list_expr_should_inline expr)
  | _ -> false

and expr_is_inline = fun expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | FieldAccess _
  | Prefix _
  | LabeledArg _
  | OptionalArg _ ->
      true
  | LocalOpen { body=Some body } ->
      expr_is_inline body
  | Infix { left=Some left; right=Some right; _ } ->
      expr_is_inline left && expr_is_inline right
  | Apply _ ->
      let callee, args = collect_apply expr in
      expr_is_inline callee && (
        let inline = ref true in
        Vector.for_each args ~fn:(fun arg -> inline := !inline && expr_is_inline arg);
        !inline
      )
  | Parenthesized { inner=Some inner } ->
      expr_is_inline inner
  | Parenthesized { inner=None } ->
      true
  | Array ->
      array_expr_should_inline expr
  | List ->
      list_expr_should_inline expr
  | Record ->
      record_expr_should_inline expr
  | Typed { expr=Some inner; _ } ->
      expr_is_inline inner
  | _ ->
      false

and array_expr_should_inline = fun expr ->
  let items = collect_array_items expr in
  match Vector.length items with
  | 0 -> true
  | 1 -> expr_is_inline (Vector.get_unchecked items ~at:0)
  | _ -> false

and list_expr_should_inline = fun expr ->
  let items = collect_child_exprs expr in
  let length = Vector.length items in
  Int.compare length 3 <= 0 && (
    let inline = ref true in
    Vector.for_each items ~fn:(fun item -> inline := !inline && expr_is_inline item);
    !inline
  )

and record_expr_should_inline = fun expr ->
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  Int.compare length 3 <= 0 && (
    let inline = ref true in
    Vector.for_each fields
      ~fn:(fun field ->
        inline := !inline && (
          match field.Ast.RecordExpr.value with
          | None -> true
          | Some value -> expr_is_inline value
        ));
    !inline
  )

let rec render_expr = fun state expr ->
  match Ast.Expr.view expr with
  | Path { path } ->
      render_path state path
  | Literal { token=Some token } ->
      emit_token state token
  | Literal { token=None } ->
      unsupported_node "literal expression without token" expr
  | FieldAccess { target=Some target; field=Some field } ->
      render_expr_atom state target;
      emit_text state ".";
      emit_token state field
  | FieldAccess _ ->
      unsupported_node "incomplete field access" expr
  | Assign { target=Some target; value=Some value } ->
      render_expr_atom state target;
      emit_space state;
      emit_text state ":=";
      emit_space state;
      render_expr state value
  | Assign _ ->
      unsupported_node "incomplete assign expression" expr
  | Infix { left=Some left; operator=Some operator; right=Some right } ->
      render_expr_atom state left;
      emit_space state;
      emit_token state operator;
      emit_space state;
      render_expr_atom state right
  | Infix _ ->
      unsupported_node "incomplete infix expression" expr
  | Prefix { operator=Some operator; operand=Some operand } ->
      emit_token state operator;
      if token_text_is operator "not" then
        emit_space state;
      render_expr_atom state operand
  | Prefix _ ->
      unsupported_node "incomplete prefix expression" expr
  | Apply _ ->
      render_apply_expr state expr
  | LabeledArg { label=Some label; value=Some value } ->
      emit_text state "~";
      emit_token state label;
      emit_text state ":";
      render_expr_atom state value
  | LabeledArg { label=Some label; value=None } ->
      emit_text state "~";
      emit_token state label
  | LabeledArg { label=None; _ } ->
      unsupported_node "labeled argument without label" expr
  | OptionalArg { label=Some label; value=Some value } ->
      emit_text state "?";
      emit_token state label;
      emit_text state ":";
      render_expr_atom state value
  | OptionalArg { label=Some label; value=None } ->
      emit_text state "?";
      emit_token state label
  | OptionalArg { label=None; _ } ->
      unsupported_node "optional argument without label" expr
  | Parenthesized { inner=Some inner } when expr_first_token_text_is expr "begin" ->
      emit_text state "begin";
      emit_line state;
      with_indent state 2 (fun () -> render_expr state inner);
      emit_line state;
      emit_text state "end"
  | Parenthesized { inner=Some inner } ->
      emit_text state "(";
      render_expr state inner;
      emit_text state ")"
  | Parenthesized { inner=None } ->
      emit_text state "()"
  | If { condition=Some condition; then_branch=Some then_branch; else_branch } ->
      render_if_expr state condition then_branch else_branch
  | If _ ->
      unsupported_node "incomplete if expression" expr
  | Let { body=Some body; _ } ->
      render_let_expr state expr body
  | Let _ ->
      unsupported_node "let expression without body" expr
  | Fun { body=Some body } ->
      render_fun_expr state expr body
  | Fun { body=None } ->
      unsupported_node "fun expression without body" expr
  | Function { first_case=Some _ } ->
      render_function_expr state expr
  | Function { first_case=None } ->
      unsupported_node "function expression without cases" expr
  | Match { scrutinee=Some scrutinee; first_case=Some _ } ->
      render_match_expr state expr scrutinee
  | Match _ ->
      unsupported_node "match expression without scrutinee or cases" expr
  | Try { body=Some body; first_case=Some _ } ->
      render_try_expr state expr body
  | Try _ ->
      unsupported_node "try expression without body or cases" expr
  | LocalOpen { body=Some _ } ->
      render_local_open_expr state expr
  | LocalOpen { body=None } ->
      unsupported_node "local open expression without body" expr
  | List ->
      render_list_expr state expr
  | Array ->
      render_array_expr state expr
  | Record ->
      render_record_expr state ~inline:false expr
  | Typed { expr=Some inner; annotation=Some annotation } ->
      render_expr state inner;
      emit_text state ":";
      emit_space state;
      render_type_expr state annotation
  | Typed _ ->
      unsupported_node "incomplete typed expression" expr
  | Tuple ->
      let items = collect_child_exprs expr in
      emit_joined_vector state items
        ~sep:(fun () ->
          emit_text state ",";
          emit_space state)
        ~fn:(render_expr state)
  | Sequence { left=Some left; right=Some right } ->
      render_expr state left;
      emit_text state ";";
      emit_line state;
      render_expr state right
  | Sequence _ ->
      unsupported_node "incomplete sequence expression" expr
  | ArrayIndex { target=Some target; index=Some index } ->
      render_expr_atom state target;
      emit_text state ".(";
      render_expr state index;
      emit_text state ")"
  | ArrayIndex _ ->
      unsupported_node "incomplete array index expression" expr
  | StringIndex { target=Some target; index=Some index } ->
      render_expr_atom state target;
      emit_text state ".[";
      render_expr state index;
      emit_text state "]"
  | StringIndex _ ->
      unsupported_node "incomplete string index expression" expr
  | PolyVariant { payload } ->
      render_poly_variant_expr state expr payload
  | RecordUpdate
  | LetModule _
  | LetException _
  | BindingOperator _
  | FirstClassModule
  | Extension
  | Unreachable
  | Object
  | New
  | While _
  | For _
  | Assert _
  | Lazy _
  | Attribute _
  | MethodCall _
  | Error _
  | Unknown _ ->
      unsupported_node "unsupported expression" expr

and render_expr_atom = fun state expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | FieldAccess _
  | Parenthesized _
  | Array
  | List
  | Record
  | LocalOpen _
  | LabeledArg _
  | PolyVariant { payload=None }
  | OptionalArg _ -> render_expr state expr
  | _ ->
      emit_text state "(";
      render_expr state expr;
      emit_text state ")"

and render_poly_variant_expr = fun state expr payload ->
  (
    match first_ident_token expr with
    | Some tag ->
        emit_text state "`";
        emit_token state tag
    | None -> unsupported_node "polymorphic variant expression without tag" expr
  );
  match payload with
  | Some payload ->
      emit_space state;
      render_expr state payload
  | None -> ()

and render_apply_expr = fun state expr ->
  let callee, args = collect_apply expr in
  render_expr_atom state callee;
  Vector.for_each args
    ~fn:(fun arg ->
      emit_space state;
      render_expr_atom state arg)

and render_if_expr = fun state condition then_branch else_branch ->
  emit_text state "if";
  emit_space state;
  render_expr state condition;
  emit_space state;
  emit_text state "then";
  emit_line state;
  with_indent state 2 (fun () -> render_expr state then_branch);
  (
    match else_branch with
    | None -> ()
    | Some branch ->
        emit_line state;
        emit_text state "else";
        if expr_is_multiline branch then
          (
            emit_line state;
            with_indent state 2 (fun () -> render_expr state branch)
          )
        else (
          emit_space state;
          render_expr state branch
        )
  )

and render_fun_expr = fun state expr body ->
  let params = collect_child_patterns expr in
  emit_text state "fun";
  Vector.for_each params
    ~fn:(fun param ->
      emit_space state;
      render_pattern state param);
  emit_space state;
  emit_text state "->";
  if expr_is_multiline body then
    (
      emit_line state;
      with_indent state 2 (fun () -> render_expr state body)
    )
  else (
    emit_space state;
    render_expr state body
  )

and render_function_expr = fun state expr ->
  let inline = not state.line_start in
  emit_text state "function";
  emit_line state;
  let cases = collect_match_cases expr in
  if inline then
    with_indent state 2 (fun () -> render_match_cases state cases)
  else
    render_match_cases state cases

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
  if expr_is_multiline body then
    (
      emit_line state;
      with_indent state 2 (fun () -> render_expr state body);
      emit_line state
    )
  else (
    emit_space state;
    render_expr state body;
    emit_space state
  );
  emit_text state "with";
  emit_line state;
  let cases = collect_match_cases expr in
  render_match_cases state cases

and render_match_cases = fun state cases ->
  let length = Vector.length cases in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        render_match_case state (Vector.get_unchecked cases ~at:index);
        if Int.compare index (Int.sub length 1) < 0 then
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
    | Some body when expr_is_multiline body ->
        emit_line state;
        with_indent state 2 (fun () -> render_expr state body)
    | Some body ->
        emit_space state;
        render_expr state body
    | None ->
        unsupported_node "match case without body" case
  )

and render_local_open_expr = fun state expr ->
  match Ast.LocalOpenExpr.view expr with
  | LetOpen { module_path=Some module_path; body=Some body; _ } ->
      emit_text state "let";
      emit_space state;
      emit_text state "open";
      emit_space state;
      render_path state module_path;
      emit_space state;
      emit_text state "in";
      emit_line state;
      render_expr state body
  | LetOpen _ ->
      unsupported_node "incomplete let-open expression" expr
  | Delimited { module_path=Some module_path; body=Some body; _ } ->
      render_path state module_path;
      emit_text state ".";
      render_expr state body
  | Delimited _ ->
      unsupported_node "incomplete delimited local-open expression" expr

and render_let_expr = fun state expr body ->
  let bindings = collect_let_bindings_from_expr expr in
  let rec_token = Ast.Node.first_child_token expr ~kind:Kind.REC_KW in
  let length = Vector.length bindings in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        let binding = Vector.get_unchecked bindings ~at:index in
        if Int.equal index 0 then
          emit_text state "let"
        else
          emit_text state "and";
        if Int.equal index 0 then
          (
            match rec_token with
            | Some token ->
                emit_space state;
                emit_token state token
            | None -> ()
          );
        emit_space state;
        render_let_binding_tail state binding;
        if Int.compare index (Int.sub length 1) < 0 then
          emit_line state;
        loop (Int.add index 1)
      )
  in
  loop 0;
  emit_line state;
  emit_text state "in";
  emit_line state;
  render_expr state body

and render_let_binding_tail = fun state binding ->
  let view = Ast.LetBinding.view binding in
  (
    match view.pattern with
    | Some pattern -> render_pattern state pattern
    | None -> unsupported_node "let binding without pattern" binding
  );
  Ast.LetBinding.for_each_parameter binding
    ~fn:(fun param ->
      emit_space state;
      render_pattern state param);
  (
    match Ast.LetBinding.type_annotation binding with
    | Some annotation ->
        emit_text state ":";
        emit_space state;
        render_type_expr state annotation
    | None -> ()
  );
  emit_space state;
  emit_text state "=";
  match view.body with
  | Some ({ Ast.id=_; _ } as body) when (
    match Ast.Expr.view body with
    | Record
    | Fun _
    | Function _ -> true
    | _ -> false
  ) ->
      emit_space state;
      render_expr state body
  | Some body when expr_is_multiline body ->
      emit_line state;
      with_indent state 2 (fun () -> render_expr state body)
  | Some body ->
      emit_space state;
      render_expr state body
  | None ->
      unsupported_node "let binding without body" binding

and render_record_expr = fun state ~inline expr ->
  let fields = collect_record_fields expr in
  let length = Vector.length fields in
  if Int.equal length 0 then
    emit_text state "{}"
  else if inline || record_expr_should_inline expr then
    (
      emit_text state "{";
      emit_space state;
      emit_joined_vector state fields
        ~sep:(fun () ->
          emit_text state ";";
          emit_space state)
        ~fn:(render_record_field state ~inline:true);
      emit_space state;
      emit_text state "}"
    )
  else (
    emit_text state "{";
    emit_line state;
    with_indent state 2
      (fun () ->
        let rec loop index =
          if Int.compare index length < 0 then
            (
              render_record_field state ~inline:false (Vector.get_unchecked fields ~at:index);
              if Int.compare index (Int.sub length 1) < 0 then
                (
                  emit_text state ";";
                  emit_line state
                );
              loop (Int.add index 1)
            )
        in
        loop 0);
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
      if (not inline) && expr_is_multiline value then
        (
          emit_space state;
          render_expr state value
        )
      else (
        emit_space state;
        render_expr state value
      )

and render_array_expr = fun state expr ->
  let items = collect_array_items expr in
  let length = Vector.length items in
  if Int.equal length 0 then
    emit_text state "[||]"
  else if Int.equal length 1 then
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
  else (
    emit_text state "[|";
    emit_line state;
    with_indent state 2
      (fun () ->
        let rec loop index =
          if Int.compare index length < 0 then
            (
              let item = Vector.get_unchecked items ~at:index in
              (
                match Ast.Expr.view item with
                | Record -> render_record_expr state ~inline:true item
                | _ -> render_expr state item
              );
              emit_text state ";";
              if Int.compare index (Int.sub length 1) < 0 then
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
  else if list_expr_should_inline expr then
    (
      emit_text state "[ ";
      emit_joined_vector state items
        ~sep:(fun () ->
          emit_text state ";";
          emit_space state)
        ~fn:(render_expr state);
      emit_space state;
      emit_text state "]"
    )
  else (
    emit_text state "[";
    emit_line state;
    with_indent state 2
      (fun () ->
        let rec loop index =
          if Int.compare index length < 0 then
            (
              render_expr state (Vector.get_unchecked items ~at:index);
              emit_text state ";";
              if Int.compare index (Int.sub length 1) < 0 then
                emit_line state;
              loop (Int.add index 1)
            )
        in
        loop 0);
    emit_line state;
    emit_text state "]"
  )

let render_open_declaration = fun state decl ->
  emit_text state "open";
  emit_space state;
  let first = ref true in
  Ast.OpenDeclaration.for_each_path_ident decl
    ~fn:(fun ident ->
      if !first then
        first := false
      else
        emit_text state ".";
      emit_token state ident)

let render_record_type_field = fun state ~last field ->
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

let render_record_type = fun state record_type ->
  let fields = collect_record_type_fields record_type in
  emit_text state "{";
  if Vector.length fields > 0 then
    (
      emit_line state;
      with_indent state 2
        (fun () ->
          let length = Vector.length fields in
          let rec loop index =
            if Int.compare index length < 0 then
              (
                render_record_type_field
                  state
                  ~last:(Int.equal index (Int.sub length 1))
                  (Vector.get_unchecked fields ~at:index);
                if Int.compare index (Int.sub length 1) < 0 then
                  emit_line state;
                loop (Int.add index 1)
              )
          in
          loop 0);
      emit_line state
    );
  emit_text state "}"

let render_variant_constructor = fun state constructor ->
  emit_text state "|";
  emit_space state;
  (
    match Ast.VariantConstructor.name constructor with
    | Some name -> emit_token state name
    | None -> unsupported_node "variant constructor without name" constructor
  );
  (
    match Ast.VariantConstructor.record_payload constructor, Ast.VariantConstructor.payload_type constructor with
    | Some record_type, _ ->
        emit_space state;
        emit_text state "of";
        emit_space state;
        render_record_type state record_type
    | None, Some payload ->
        emit_space state;
        emit_text state "of";
        emit_space state;
        render_type_expr state payload
    | None, None ->
        ()
  );
  (
    match Ast.VariantConstructor.result_type constructor with
    | Some result ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        render_type_expr state result
    | None -> ()
  )

let render_variant_type = fun state variant_type ->
  let constructors = collect_variant_constructors variant_type in
  let length = Vector.length constructors in
  if Int.compare length 0 > 0 then
    (
      emit_line state;
      with_indent state 2
        (fun () ->
          let rec loop index =
            if Int.compare index length < 0 then
              (
                render_variant_constructor state (Vector.get_unchecked constructors ~at:index);
                if Int.compare index (Int.sub length 1) < 0 then
                  emit_line state;
                loop (Int.add index 1)
              )
          in
          loop 0)
    )

let render_type_member = fun state member ->
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
  (
    match Ast.TypeDeclaration.Member.name member with
    | Some name -> emit_token state name
    | None -> unsupported "type declaration member without name"
  );
  (
    match Ast.TypeDeclaration.Member.variant_type member, Ast.TypeDeclaration.Member.record_type member, Ast.TypeDeclaration.Member.manifest
      member with
    | Some variant_type, _, _ ->
        emit_space state;
        emit_text state "=";
        render_variant_type state variant_type
    | None, Some record_type, _ ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_record_type state record_type
    | None, None, Some manifest ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_type_expr state manifest
    | None, None, None ->
        ()
  )

let render_type_declaration = fun state decl ->
  let members = collect_type_members decl in
  let length = Vector.length members in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        if Int.compare index 0 > 0 then
          emit_line state;
        render_type_member state (Vector.get_unchecked members ~at:index);
        loop (Int.add index 1)
      )
  in
  loop 0

let render_module_body_path = fun state for_each_path_ident ->
  let first = ref true in
  for_each_path_ident
    ~fn:(fun ident ->
      if !first then
        first := false
      else
        emit_text state ".";
      emit_token state ident)

let render_value_declaration = fun state decl ->
  emit_text state "val";
  emit_space state;
  (
    match Ast.ValueDeclaration.name decl with
    | Some name -> emit_token state name
    | None -> unsupported_node "value declaration without name" decl
  );
  emit_text state ":";
  emit_space state;
  (
    match Ast.ValueDeclaration.type_annotation decl with
    | Some annotation -> render_type_expr state annotation
    | None -> unsupported_node "value declaration without annotation" decl
  )

let render_let_declaration = fun state decl ->
  let bindings = collect_let_bindings decl in
  let rec_token = Ast.LetDeclaration.rec_token decl in
  let length = Vector.length bindings in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        if Int.equal index 0 then
          emit_text state "let"
        else
          emit_text state "and";
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
        if Int.compare index (Int.sub length 1) < 0 then
          emit_line state;
        loop (Int.add index 1)
      )
  in
  loop 0

let rec render_module_declaration = fun state decl ->
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
    match Ast.ModuleDeclaration.body decl with
    | Path ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_module_body_path state (Ast.ModuleDeclaration.for_each_body_path_ident decl)
    | EmptyStruct ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        emit_text state "struct";
        emit_space state;
        emit_text state "end"
    | Struct ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        emit_text state "struct";
        emit_line state;
        with_indent state 2
          (fun () ->
            let first = ref true in
            Ast.ModuleDeclaration.for_each_structure_item decl
              ~fn:(fun item ->
                if !first then
                  first := false
                else (
                  emit_line state;
                  emit_line state
                );
                render_structure_item state item));
        emit_line state;
        emit_text state "end"
    | EmptySig ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        emit_text state "sig";
        emit_space state;
        emit_text state "end"
    | Sig ->
        emit_space state;
        emit_text state ":";
        emit_space state;
        emit_text state "sig";
        emit_line state;
        with_indent state 2
          (fun () ->
            let items = collect_signature_items_from_module_decl decl in
            render_signature_items state items);
        emit_line state;
        emit_text state "end"
    | Unsupported ->
        unsupported_node "unsupported module declaration body" decl
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
    | Abstract ->
        ()
    | Path ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        render_module_body_path state (Ast.ModuleTypeDeclaration.for_each_body_path_ident decl)
    | EmptySig ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        emit_text state "sig";
        emit_space state;
        emit_text state "end"
    | Sig ->
        emit_space state;
        emit_text state "=";
        emit_space state;
        emit_text state "sig";
        emit_line state;
        with_indent state 2
          (fun () ->
            let items = collect_signature_items_from_module_type_decl decl in
            render_signature_items state items);
        emit_line state;
        emit_text state "end"
    | With
    | Unsupported ->
        unsupported_node "unsupported module type declaration body" decl
  )

and render_signature_items = fun state items ->
  let length = Vector.length items in
  let rec loop index =
    if Int.compare index length < 0 then
      (
        render_signature_item state (Vector.get_unchecked items ~at:index);
        if Int.compare index (Int.sub length 1) < 0 then
          (
            emit_line state;
            emit_line state
          );
        loop (Int.add index 1)
      )
  in
  loop 0

and render_signature_item = fun state item ->
  match Ast.SignatureItem.view item with
  | Value decl -> render_value_declaration state decl
  | Type decl -> render_type_declaration state decl
  | Module decl -> render_module_declaration state decl
  | ModuleType decl -> render_module_type_declaration state decl
  | Open decl -> render_open_declaration state decl
  | Include _
  | External _
  | Exception _
  | TypeExtension _
  | Class _
  | Extension _
  | Attribute _
  | Error _
  | Unknown _ -> unsupported_node "unsupported signature item" item

and render_structure_item = fun state item ->
  emit_top_level_leading state item;
  match Ast.StructureItem.view item with
  | Open decl ->
      render_open_declaration state decl
  | Type decl ->
      render_type_declaration state decl
  | Let decl ->
      render_let_declaration state decl
  | Module decl ->
      render_module_declaration state decl
  | ModuleType decl ->
      render_module_type_declaration state decl
  | Expr expr_item -> (
      match Ast.ExprItem.expr expr_item with
      | Some expr -> render_expr state expr
      | None -> unsupported_node "expr item without expression" item
    )
  | TypeExtension _
  | Include _
  | External _
  | Exception _
  | Class _
  | Extension _
  | Attribute _
  | Error _
  | Unknown _ ->
      unsupported_node "unsupported structure item" item

let render_implementation = fun state implementation ->
  let first = ref true in
  Ast.Implementation.for_each_item implementation
    ~fn:(fun item ->
      if !first then
        first := false
      else (
        emit_line state;
        emit_line state
      );
      render_structure_item state item)

let render_source_file = fun state source_file ->
  match Ast.SourceFile.view source_file with
  | Empty -> ()
  | Implementation implementation -> render_implementation state implementation
  | Interface _ -> unsupported "streaming interface formatting is not implemented yet"

let write = fun ~writer ?(width = 100) ?(buffer_size = 4_096) source_file ->
  ignore width;
  let sink = { writer; flush_threshold = Int.max 1 buffer_size; error = None } in
  let state = {
    buffer = IO.Buffer.create ~size:(Int.max 0 buffer_size);
    sink;
    line_start = true;
    column = 0;
    indent = 0;
    wrote = false;
  }
  in
  try
    render_source_file state source_file;
    if state.wrote && not state.line_start then
      emit_line state;
    match flush state with
    | Ok () -> (
        match sink.error with
        | Some error -> Error (Cannot_write error)
        | None -> IO.flush writer |> Result.map_err ~fn:(fun error -> Cannot_write error)
      )
    | Error error -> Error (Cannot_write error)
  with
  | Unsupported err -> Error (Cannot_format err)
