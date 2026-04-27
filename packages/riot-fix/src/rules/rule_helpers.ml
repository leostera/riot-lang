open Std
open Std.Collections

module Ast = Syn.Ast

let span_of_token = fun token ->
  Syn.Ceibo.Span.make
    ~start:(Ast.Token.span_start token)
    ~end_:(Ast.Token.span_end token)

let span_of_node = fun node ->
  Syn.Ceibo.Span.make
    ~start:(Ast.Node.span_start node)
    ~end_:(Ast.Node.span_end node)

let vector_to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let diagnostics_for_root = fun root -> Vector.with_capacity ~size:(Ast.Node.child_count root)

let push_diagnostic = fun diagnostics diagnostic -> Vector.push diagnostics ~value:diagnostic

let diagnostic = fun ~rule_id ~message ~span ?suggestion ?fix () ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message })
    ~span
    ?suggestion
    ?fix
    ()

let replace_token_fix = fun ~title ~token ~text ->
  Fix.make
    ~title
    ~operations:[ Fix.replace_token_with_text ~target:token ~text; ]

let diagnostic_for_token = fun ~rule_id ~message ~token ?suggestion ?fix () ->
  diagnostic
    ~rule_id
    ~message
    ~span:(span_of_token token)
    ?suggestion
    ?fix
    ()

let is_upper = fun ch -> ch >= 'A' && ch <= 'Z'

let is_lower = fun ch -> ch >= 'a' && ch <= 'z'

let is_digit = fun ch -> ch >= '0' && ch <= '9'

let is_letter = fun ch -> is_upper ch || is_lower ch

let is_ident_char = fun ch -> is_letter ch || is_digit ch || Char.equal ch '_' || Char.equal ch '\''

let to_snake_case = fun text ->
  let pieces = Vector.with_capacity ~size:(String.length text * 2) in
  let prev_was_lower_or_digit = ref false in
  String.for_each
    text
    ~fn:(fun ch ->
      if is_upper ch then (
        if !prev_was_lower_or_digit then
          Vector.push pieces ~value:"_";
        Vector.push pieces ~value:(String.make ~len:1 ~char:(Char.lowercase_ascii ch));
        prev_was_lower_or_digit := false
      ) else if Char.equal ch '-' then (
        Vector.push pieces ~value:"_";
        prev_was_lower_or_digit := false
      ) else (
        Vector.push pieces ~value:(String.make ~len:1 ~char:ch);
        prev_was_lower_or_digit := is_lower ch || is_digit ch
      ));
  Vector.to_array pieces
  |> Array.to_list
  |> String.concat ""

let to_class_case = fun text ->
  let pieces = Vector.with_capacity ~size:(String.length text) in
  let capitalize_next = ref true in
  String.for_each
    text
    ~fn:(fun ch ->
      if Char.equal ch '_' || Char.equal ch '-' then
        capitalize_next := true
      else
        (
          let ch =
            if !capitalize_next then
              Char.uppercase_ascii ch
            else
              ch
          in
          Vector.push pieces ~value:(String.make ~len:1 ~char:ch);
          capitalize_next := false
        ));
  Vector.to_array pieces
  |> Array.to_list
  |> String.concat ""

let should_be_snake_case = fun text -> not (String.equal text (to_snake_case text))

let should_be_class_case = fun text -> not (String.equal text (to_class_case text))

let path_last_ident = fun path -> Ast.Path.last_ident path

let first_child_expr = fun expr ->
  let found = ref None in
  Ast.Expr.for_each_child_expr
    expr
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None -> found := Some child);
  !found

let first_child_pattern = fun pattern ->
  let found = ref None in
  Ast.Pattern.for_each_child_pattern
    pattern
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None -> found := Some child);
  !found

let first_child_type_expr = fun type_expr ->
  let found = ref None in
  Ast.TypeExpr.for_each_child_type
    type_expr
    ~fn:(fun child ->
      match !found with
      | Some _ -> ()
      | None -> found := Some child);
  !found

let rec unwrap_expr = fun expr ->
  match Ast.AttributeExpr.cast expr with
  | Some attribute -> (
      match Ast.AttributeExpr.inner attribute with
      | Some inner -> unwrap_expr inner
      | None -> expr
    )
  | None ->
      if Syn.SyntaxKind.(Ast.Node.kind expr = PAREN_EXPR) then
        match first_child_expr expr with
        | Some inner -> unwrap_expr inner
        | None -> expr
      else
        expr

let rec unwrap_pattern = fun pattern ->
  match Ast.AttributePattern.cast pattern with
  | Some attribute -> (
      match Ast.AttributePattern.inner attribute with
      | Some inner -> unwrap_pattern inner
      | None -> pattern
    )
  | None ->
      if Syn.SyntaxKind.(Ast.Node.kind pattern = PAREN_PATTERN) then
        match first_child_pattern pattern with
        | Some inner -> unwrap_pattern inner
        | None -> pattern
      else
        pattern

let rec unwrap_type_expr = fun type_expr ->
  if Syn.SyntaxKind.(Ast.Node.kind type_expr = PAREN_TYPE) then
    match first_child_type_expr type_expr with
    | Some inner -> unwrap_type_expr inner
    | None -> type_expr
  else
    type_expr

type parameter_kind =
  | LabeledParameter
  | OptionalParameter

let parameter_kind = fun pattern ->
  match Ast.Node.kind pattern with
  | Syn.SyntaxKind.LABELED_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM_DEFAULT -> (
      let parameter =
        Ast.Parameter.cast pattern
        |> Option.expect ~msg:"expected syntactic parameter view"
      in
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; _ } -> None
      | Ast.Parameter.Param { label = Ast.Parameter.Labeled _; _ } -> Some LabeledParameter
      | Ast.Parameter.Param { label = Ast.Parameter.Optional _; _ } -> Some OptionalParameter
      | Ast.Parameter.Unknown _ -> None
    )
  | _ -> None

let pattern_name_token = fun pattern ->
  match Ast.Pattern.view (unwrap_pattern pattern) with
  | Ast.Pattern.Ident { path } -> path_last_ident path
  | Ast.Pattern.Constraint { pattern; _ }
  | Ast.Pattern.Alias { pattern; _ } -> (
      match Ast.Pattern.view pattern with
      | Ast.Pattern.Ident { path } -> path_last_ident path
      | _ -> None
    )
  | _ -> None

let binding_name_token = fun binding ->
  Ast.LetBinding.pattern binding
  |> Option.and_then ~fn:pattern_name_token

let binding_has_parameters = fun binding ->
  let found = ref false in
  Ast.LetBinding.for_each_parameter binding ~fn:(fun _ -> found := true);
  !found

let expr_is_fun = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Fun _ -> true
  | _ -> false

let binding_is_function = fun binding ->
  binding_has_parameters binding || match Ast.LetBinding.body binding with
  | Some body -> expr_is_fun body
  | None -> false

let rec parameter_name_token = fun pattern ->
  match Ast.Node.kind pattern with
  | Syn.SyntaxKind.LABELED_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM
  | Syn.SyntaxKind.OPTIONAL_PARAM_DEFAULT -> (
      let parameter =
        Ast.Parameter.cast pattern
        |> Option.expect ~msg:"expected syntactic parameter view"
      in
      match Ast.Parameter.view parameter with
      | Ast.Parameter.Param { label = Ast.Parameter.NoLabel; pattern = Some pattern } ->
          parameter_name_token pattern
      | Ast.Parameter.Param { label = Ast.Parameter.Labeled { name = Some label }; _ }
      | Ast.Parameter.Param { label = Ast.Parameter.Optional { name = Some label; _ }; _ } -> Some label
      | Ast.Parameter.Param { pattern = Some pattern; _ } -> pattern_name_token pattern
      | _ -> None
    )
  | _ -> (
      match Ast.Pattern.view pattern with
      | Ast.Pattern.Ident { path } -> path_last_ident path
      | Ast.Pattern.Constraint { pattern; _ } -> parameter_name_token pattern
      | _ -> None
    )

let for_each_let_binding = fun root ~fn ->
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_let_binding =
        Some (fun visitor binding ->
          fn binding;
          (visitor, Syn.Visitor.Continue));
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor -> ignore (Syn.Visitor.visit_node visitor root)

let for_each_type_declaration = fun root ~fn ->
  let hooks =
    {
      Syn.Visitor.empty_hooks with
      enter_type_declaration =
        Some (fun visitor declaration ->
          fn declaration;
          (visitor, Syn.Visitor.Continue));
    }
  in
  Syn.Visitor.make ~ctx:() ~hooks
  |> fun visitor -> ignore (Syn.Visitor.visit_node visitor root)

let source_span = fun source start stop -> String.sub source ~offset:start ~len:(stop - start)

let span_text = fun source span -> source_span source span.Syn.Ceibo.Span.start span.end_

let node_source = fun ctx node ->
  source_span
    ctx.Rule.source
    (Ast.Node.span_start node)
    (Ast.Node.span_end node)

let token_source = fun ctx token ->
  source_span
    ctx.Rule.source
    (Ast.Token.span_start token)
    (Ast.Token.span_end token)

let token_starts_with = fun token prefix -> String.starts_with ~prefix (Ast.Token.text token)
