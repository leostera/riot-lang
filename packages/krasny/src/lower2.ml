open Std
open Std.Collections
module Ast = Syn.Ast2
module Doc = Doc
module Kind = Syn.SyntaxKind2

type error = {
  message: string;
}

exception Unsupported of error

let error_to_string = fun err -> err.message

let unsupported = fun message -> raise (Unsupported { message })

let token_doc = fun token -> Doc.text (Ast.Token.text token)

let optional_token_doc = fun token ->
  match token with
  | Some token -> token_doc token
  | None -> Doc.empty

let path_doc = fun path ->
  let segments = ref [] in
  Ast.Path.for_each_ident path ~fn:(fun token -> segments := token_doc token :: !segments);
  Doc.join (Doc.text ".") (List.reverse !segments)

let open_path_doc = fun decl ->
  let segments = ref [] in
  Ast.OpenDeclaration.for_each_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  Doc.join (Doc.text ".") (List.reverse !segments)

let include_path_doc = fun decl ->
  let segments = ref [] in
  Ast.IncludeDeclaration.for_each_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "include declaration without target"
  | segments -> Doc.join (Doc.text ".") segments

let child_expr_docs = fun expr ->
  let docs = ref [] in
  Ast.Expr.for_each_child_expr expr ~fn:(fun child -> docs := child :: !docs);
  List.reverse !docs

let child_pattern_docs = fun pattern ->
  let docs = ref [] in
  Ast.Pattern.for_each_child_pattern pattern ~fn:(fun child -> docs := child :: !docs);
  List.reverse !docs

let direct_pattern_docs = fun node ->
  let docs = ref [] in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.Pattern.cast child with
      | Some pattern -> docs := pattern :: !docs
      | None -> ());
  List.reverse !docs

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Kind.(Ast.Token.kind token = IDENT) then
            found := Some token);
  !found

let let_binding_nodes = fun node ->
  let bindings = ref [] in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.LetBinding.cast child with
      | Some binding -> bindings := binding :: !bindings
      | None -> ());
  List.reverse !bindings

type let_binding_parts = {
  pattern: Ast.pattern option;
  parameters: Ast.pattern list;
  annotation: Ast.type_expr option;
  body: Ast.expr option;
}

let let_binding_parts = fun binding ->
  let pattern = ref None in
  let parameters = ref [] in
  let annotation = ref None in
  let body = ref None in
  Ast.Node.for_each_child_node binding
    ~fn:(fun child ->
      match Ast.Pattern.cast child with
      | Some child_pattern -> (
          match !pattern with
          | None -> pattern := Some child_pattern
          | Some _ -> parameters := child_pattern :: !parameters
        )
      | None -> (
          match Ast.TypeExpr.cast child with
          | Some type_expr -> (
              match !annotation with
              | None -> annotation := Some type_expr
              | Some _ -> ()
            )
          | None -> (
              match Ast.Expr.cast child with
              | Some expr -> (
                  match !body with
                  | None -> body := Some expr
                  | Some _ -> ()
                )
              | None -> ()
            )
        ));
  {
    pattern = !pattern;
    parameters = List.reverse !parameters;
    annotation = !annotation;
    body = !body
  }

let rec type_expr_doc = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Path { path } -> path_doc path
  | Var { name=Some name } -> Doc.concat [ Doc.text "'"; token_doc name ]
  | Var { name=None } -> unsupported "type variable without name"
  | Wildcard -> Doc.text "_"
  | Arrow { left=Some left; right=Some right } -> Doc.concat
    [ type_expr_doc left; Doc.space; Doc.arrow; Doc.space; type_expr_doc right ]
  | Arrow _ -> unsupported "incomplete arrow type expression"
  | Tuple { left=Some left; right=Some right } -> Doc.concat
    [ type_expr_doc left; Doc.space; Doc.text "*"; Doc.space; type_expr_doc right ]
  | Tuple _ -> unsupported "incomplete tuple type expression"
  | Apply { argument=Some argument; constructor=Some constructor } -> Doc.concat
    [ type_expr_doc argument; Doc.space; type_expr_doc constructor ]
  | Apply _ -> unsupported "incomplete type application"
  | Parenthesized { inner=Some inner } -> Doc.concat [ Doc.lparen; type_expr_doc inner; Doc.rparen ]
  | Parenthesized { inner=None } -> Doc.concat [ Doc.lparen; Doc.rparen ]
  | Opaque _
  | Error _
  | Unknown _ -> unsupported "unsupported type expression"

let rec pattern_doc = fun pattern ->
  match Ast.Pattern.view pattern with
  | Wildcard ->
      Doc.text "_"
  | Path { path } ->
      path_doc path
  | Literal { token=Some token } ->
      token_doc token
  | Literal { token=None } ->
      unsupported "literal pattern without token"
  | Parenthesized { inner=Some inner } ->
      Doc.concat [ Doc.lparen; pattern_doc inner; Doc.rparen ]
  | Parenthesized { inner=None } ->
      Doc.concat [ Doc.lparen; Doc.rparen ]
  | Tuple ->
      child_pattern_docs pattern
      |> List.map ~fn:pattern_doc
      |> Doc.join (Doc.concat [ Doc.comma; Doc.space ])
  | List ->
      child_pattern_docs pattern
      |> List.map ~fn:pattern_doc
      |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])
      |> fun items -> Doc.concat [ Doc.lbracket; items; Doc.rbracket ]
  | Array ->
      child_pattern_docs pattern
      |> List.map ~fn:pattern_doc
      |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])
      |> fun items -> Doc.concat [ Doc.text "[|"; items; Doc.text "|]" ]
  | Record ->
      record_pattern_doc pattern
  | Cons { head=Some head; tail=Some tail } ->
      Doc.concat [ pattern_doc head; Doc.space; Doc.text "::"; Doc.space; pattern_doc tail ]
  | Cons _ ->
      unsupported "incomplete cons pattern"
  | Constraint { pattern=Some pattern; annotation=Some annotation } ->
      Doc.concat [ pattern_doc pattern; Doc.text ":"; Doc.space; type_expr_doc annotation ]
  | Constraint _ ->
      unsupported "incomplete typed pattern"
  | Alias { pattern=Some pattern; alias=Some alias } ->
      Doc.concat [ pattern_doc pattern; Doc.space; Doc.text "as"; Doc.space; pattern_doc alias ]
  | Alias _ ->
      unsupported "incomplete alias pattern"
  | Apply { callee=Some callee; argument=Some argument } ->
      Doc.concat [ pattern_doc callee; Doc.space; pattern_doc argument ]
  | Apply _ ->
      unsupported "incomplete apply pattern"
  | Or { left=Some left; right=Some right } ->
      Doc.concat [ pattern_doc left; Doc.space; Doc.bar; Doc.space; pattern_doc right ]
  | Or _ ->
      unsupported "incomplete or pattern"
  | PolyVariant ->
      let head =
        match first_ident_token pattern with
        | Some tag -> Doc.concat [ Doc.text "`"; token_doc tag ]
        | None -> unsupported "polymorphic variant pattern without tag"
      in
      (
        match child_pattern_docs pattern with
        | [] -> head
        | [ payload ] -> Doc.concat [ head; Doc.space; pattern_doc payload ]
        | _ -> unsupported "polymorphic variant pattern with multiple payloads"
      )
  | LabeledParam parameter ->
      parameter_doc parameter
  | OptionalParam parameter ->
      parameter_doc parameter
  | OptionalParamDefault parameter ->
      parameter_doc parameter
  | Interval { left=Some left; right=Some right } ->
      Doc.concat [ pattern_doc left; Doc.space; Doc.text ".."; Doc.space; pattern_doc right ]
  | Interval _ ->
      unsupported "incomplete interval pattern"
  | Lazy { pattern=Some pattern } ->
      Doc.concat [ Doc.text "lazy"; Doc.space; pattern_doc pattern ]
  | Lazy _ ->
      unsupported "lazy pattern without payload"
  | Exception { pattern=Some pattern } ->
      Doc.concat [ Doc.text "exception"; Doc.space; pattern_doc pattern ]
  | Exception _ ->
      unsupported "exception pattern without payload"
  | LocalOpen ->
      local_open_pattern_doc pattern
  | Extension
  | Attribute _
  | LocallyAbstractType
  | FirstClassModule
  | Error _
  | Unknown _ ->
      unsupported "unsupported pattern"

and local_open_pattern_path_doc = fun pattern ->
  let segments = ref [] in
  Ast.LocalOpenPattern.for_each_module_path_ident
    pattern
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "local open pattern without module path"
  | segments -> Doc.join (Doc.text ".") segments

and local_open_pattern_doc = fun pattern ->
  match Ast.LocalOpenPattern.cast pattern with
  | None -> unsupported "unsupported local open pattern"
  | Some local_open -> (
      match Ast.LocalOpenPattern.dot_token local_open, Ast.LocalOpenPattern.opening_token local_open, Ast.LocalOpenPattern.pattern
        local_open, Ast.LocalOpenPattern.closing_token local_open with
      | Some dot_token, Some opening_token, Some inner, Some closing_token -> Doc.concat
        [
          local_open_pattern_path_doc local_open;
          token_doc dot_token;
          token_doc opening_token;
          pattern_doc inner;
          token_doc closing_token;
        ]
      | _ -> unsupported "incomplete local open pattern"
    )

and record_pattern_field_doc = fun (field: Ast.RecordPattern.field) ->
  match field.path with
  | Some path -> (
      match field.pattern with
      | Some pattern -> Doc.concat
        [ path_doc path; Doc.space; Doc.equal; Doc.space; pattern_doc pattern ]
      | None -> path_doc path
    )
  | None -> unsupported "unsupported record pattern field"

and record_pattern_doc = fun pattern ->
  let fields = ref [] in
  Ast.RecordPattern.for_each_field
    pattern
    ~fn:(fun field -> fields := record_pattern_field_doc field :: !fields);
  let fields =
    (
      match Ast.RecordPattern.open_wildcard pattern with
      | Some wildcard -> token_doc wildcard :: !fields
      | None -> !fields
    )
    |> List.reverse
  in
  match fields with
  | [] -> Doc.concat [ Doc.lbrace; Doc.rbrace ]
  | fields -> Doc.concat
    [
      Doc.lbrace;
      Doc.space;
      Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields;
      Doc.space;
      Doc.rbrace;
    ]

and parameter_doc = fun parameter ->
  match Ast.Parameter.view parameter with
  | Labeled { label=Some label; pattern=None } -> Doc.concat [ Doc.text "~"; token_doc label ]
  | Labeled { label=Some label; pattern=Some pattern } -> Doc.concat
    [ Doc.text "~"; token_doc label; Doc.text ":"; pattern_doc pattern ]
  | Labeled _ -> unsupported "labeled parameter without label"
  | Optional { label=Some label; pattern=None } -> Doc.concat [ Doc.text "?"; token_doc label ]
  | Optional { label=Some label; pattern=Some pattern } -> Doc.concat
    [ Doc.text "?"; token_doc label; Doc.text ":"; pattern_doc pattern ]
  | Optional _ -> unsupported "optional parameter without label"
  | OptionalDefault { label=Some label; pattern=Some pattern; default=Some default } -> Doc.concat
    [
      Doc.text "?";
      token_doc label;
      Doc.text ":(";
      pattern_doc pattern;
      Doc.space;
      Doc.equal;
      Doc.space;
      expr_doc default;
      Doc.rparen;
    ]
  | OptionalDefault _ -> unsupported "incomplete optional parameter default"
  | Unknown _ -> unsupported "unsupported parameter"

and match_case_doc = fun match_case ->
  let view = Ast.MatchCase.view match_case in
  match view.pattern, view.body with
  | Some pattern, Some body ->
      let guard =
        match view.guard with
        | Some guard -> Doc.concat [ Doc.space; Doc.text "when"; Doc.space; expr_doc guard ]
        | None -> Doc.empty
      in
      Doc.concat
        [
          Doc.bar;
          Doc.space;
          pattern_doc pattern;
          guard;
          Doc.space;
          Doc.arrow;
          Doc.space;
          expr_doc body
        ]
  | _ -> unsupported "incomplete match case"

and expr_apply_callee_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Path _
  | FieldAccess _
  | MethodCall _
  | Apply _
  | Parenthesized _ -> expr_doc_with_view expr view
  | _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]

and expr_apply_argument_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Path _
  | Literal _
  | Parenthesized _
  | List
  | Array
  | PolyVariant _
  | LabeledArg _
  | OptionalArg _
  | Record
  | RecordUpdate -> expr_doc_with_view expr view
  | _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]

and expr_infix_operand_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Tuple
  | Sequence _
  | Let _
  | LocalOpen _
  | LetModule _
  | LetException _
  | BindingOperator _
  | If _
  | Match _
  | Fun _
  | Function _
  | Try _
  | While _
  | For _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]
  | _ -> expr_doc_with_view expr view

and expr_doc = fun expr -> expr_doc_with_view expr (Ast.Expr.view expr)

and expr_doc_with_view = fun expr (view: Ast.Expr.view) ->
  match view with
  | Path { path } ->
      path_doc path
  | Literal { token=Some token } ->
      token_doc token
  | Literal { token=None } ->
      unsupported "literal expression without token"
  | Parenthesized { inner=Some inner } ->
      Doc.concat [ Doc.lparen; expr_doc inner; Doc.rparen ]
  | Parenthesized { inner=None } ->
      Doc.concat [ Doc.lparen; Doc.rparen ]
  | Infix { left=Some left; operator=Some operator; right=Some right } ->
      Doc.concat
        [
          expr_infix_operand_doc left;
          Doc.space;
          token_doc operator;
          Doc.space;
          expr_infix_operand_doc right;
        ]
  | Infix _ ->
      unsupported "incomplete infix expression"
  | Prefix { operator=Some operator; operand=Some operand } ->
      Doc.concat [ token_doc operator; expr_doc operand ]
  | Prefix _ ->
      unsupported "incomplete prefix expression"
  | Apply { callee=Some callee; argument=Some argument } ->
      Doc.concat [ expr_apply_callee_doc callee; Doc.space; expr_apply_argument_doc argument ]
  | Apply _ ->
      unsupported "incomplete apply expression"
  | Typed { expr=Some expr; annotation=Some annotation } ->
      Doc.concat [ expr_doc expr; Doc.text ":"; Doc.space; type_expr_doc annotation ]
  | Typed _ ->
      unsupported "incomplete typed expression"
  | If { condition=Some condition; then_branch=Some then_branch; else_branch=Some else_branch } ->
      Doc.concat
        [
          Doc.text "if";
          Doc.space;
          expr_doc condition;
          Doc.space;
          Doc.text "then";
          Doc.space;
          expr_doc then_branch;
          Doc.space;
          Doc.text "else";
          Doc.space;
          expr_doc else_branch;
        ]
  | If { condition=Some condition; then_branch=Some then_branch; else_branch=None } ->
      Doc.concat
        [
          Doc.text "if";
          Doc.space;
          expr_doc condition;
          Doc.space;
          Doc.text "then";
          Doc.space;
          expr_doc then_branch;
        ]
  | If _ ->
      unsupported "incomplete if expression"
  | Tuple ->
      child_expr_docs expr |> List.map ~fn:expr_doc |> Doc.join (Doc.concat [ Doc.comma; Doc.space ])
  | List ->
      child_expr_docs expr
      |> List.map ~fn:expr_doc
      |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])
      |> fun items -> Doc.concat [ Doc.lbracket; items; Doc.rbracket ]
  | Array ->
      child_expr_docs expr
      |> List.map ~fn:expr_doc
      |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])
      |> fun items -> Doc.concat [ Doc.text "[|"; items; Doc.text "|]" ]
  | Record
  | RecordUpdate ->
      record_expr_doc expr
  | Sequence { left=Some left; right=Some right } ->
      Doc.concat [ expr_doc left; Doc.semi; Doc.space; expr_doc right ]
  | Sequence _ ->
      unsupported "incomplete sequence expression"
  | Let { first_binding=Some _; body=Some body } ->
      Doc.concat
        [
          let_bindings_doc
            ~keyword:"let"
            ~rec_token:(Ast.Node.first_child_token expr ~kind:Kind.REC_KW)
            expr;
          Doc.space;
          Doc.text "in";
          Doc.space;
          expr_doc body;
        ]
  | Let _ ->
      unsupported "incomplete let expression"
  | Fun { body=Some body } -> (
      match direct_pattern_docs expr with
      | [] -> unsupported "function expression without parameters"
      | parameters -> Doc.concat
        [
          Doc.text "fun";
          Doc.space;
          Doc.join Doc.space (List.map parameters ~fn:pattern_doc);
          Doc.space;
          Doc.arrow;
          Doc.space;
          expr_doc body;
        ]
    )
  | Fun _ ->
      unsupported "incomplete function expression"
  | Match { scrutinee=Some scrutinee; first_case=Some _ } ->
      let cases = ref [] in
      Ast.Expr.for_each_match_case
        expr
        ~fn:(fun match_case -> cases := match_case_doc match_case :: !cases);
      Doc.concat
        [
          Doc.text "match";
          Doc.space;
          expr_doc scrutinee;
          Doc.space;
          Doc.text "with";
          Doc.space;
          Doc.join Doc.space (List.reverse !cases);
        ]
  | Match _ ->
      unsupported "incomplete match expression"
  | Function { first_case=Some _ } ->
      let cases = ref [] in
      Ast.Expr.for_each_match_case
        expr
        ~fn:(fun match_case -> cases := match_case_doc match_case :: !cases);
      Doc.concat [ Doc.text "function"; Doc.space; Doc.join Doc.space (List.reverse !cases) ]
  | Function _ ->
      unsupported "incomplete function expression"
  | Try { body=Some body; first_case=Some _ } ->
      let cases = ref [] in
      Ast.Expr.for_each_match_case
        expr
        ~fn:(fun match_case -> cases := match_case_doc match_case :: !cases);
      Doc.concat
        [
          Doc.text "try";
          Doc.space;
          expr_doc body;
          Doc.space;
          Doc.text "with";
          Doc.space;
          Doc.join Doc.space (List.reverse !cases);
        ]
  | Try _ ->
      unsupported "incomplete try expression"
  | While { condition=Some condition; body=Some body } ->
      Doc.concat
        [
          Doc.text "while";
          Doc.space;
          expr_doc condition;
          Doc.space;
          Doc.text "do";
          Doc.space;
          expr_doc body;
          Doc.space;
          Doc.text "done";
        ]
  | While _ ->
      unsupported "incomplete while expression"
  | For { pattern=Some pattern; start_=Some start_; stop=Some stop; body=Some body } ->
      let direction =
        match Ast.Node.first_child_token expr ~kind:Kind.DOWNTO_KW with
        | Some token -> token_doc token
        | None -> Doc.text "to"
      in
      Doc.concat
        [
          Doc.text "for";
          Doc.space;
          pattern_doc pattern;
          Doc.space;
          Doc.equal;
          Doc.space;
          expr_doc start_;
          Doc.space;
          direction;
          Doc.space;
          expr_doc stop;
          Doc.space;
          Doc.text "do";
          Doc.space;
          expr_doc body;
          Doc.space;
          Doc.text "done";
        ]
  | For _ ->
      unsupported "incomplete for expression"
  | Assert { argument=Some argument } ->
      Doc.concat [ Doc.text "assert"; Doc.space; expr_doc argument ]
  | Assert _ ->
      unsupported "assert expression without argument"
  | Lazy { argument=Some argument } ->
      Doc.concat [ Doc.text "lazy"; Doc.space; expr_doc argument ]
  | Lazy _ ->
      unsupported "lazy expression without argument"
  | Assign { target=Some target; value=Some value } ->
      Doc.concat [ expr_doc target; Doc.space; Doc.text "<-"; Doc.space; expr_doc value ]
  | Assign _ ->
      unsupported "incomplete assignment expression"
  | FieldAccess { target=Some target; field=Some field } ->
      Doc.concat [ expr_doc target; Doc.text "."; token_doc field ]
  | FieldAccess _ ->
      unsupported "incomplete field access expression"
  | MethodCall { target=Some target; method_=Some method_ } ->
      Doc.concat [ expr_doc target; Doc.text "#"; token_doc method_ ]
  | MethodCall _ ->
      unsupported "incomplete method call expression"
  | PolyVariant { payload } ->
      let head =
        match first_ident_token expr with
        | Some tag -> Doc.concat [ Doc.text "`"; token_doc tag ]
        | None -> unsupported "polymorphic variant expression without tag"
      in
      (
        match payload with
        | Some payload -> Doc.concat [ head; Doc.space; expr_doc payload ]
        | None -> head
      )
  | ArrayIndex { target=Some target; index=Some index } ->
      Doc.concat [ expr_doc target; Doc.text ".("; expr_doc index; Doc.rparen ]
  | ArrayIndex _ ->
      unsupported "incomplete array index expression"
  | StringIndex { target=Some target; index=Some index } ->
      Doc.concat [ expr_doc target; Doc.text ".["; expr_doc index; Doc.rbracket ]
  | StringIndex _ ->
      unsupported "incomplete string index expression"
  | LabeledArg { label=Some label; value=None } ->
      Doc.concat [ Doc.text "~"; token_doc label ]
  | LabeledArg { label=Some label; value=Some value } ->
      Doc.concat [ Doc.text "~"; token_doc label; Doc.text ":"; expr_apply_argument_doc value ]
  | LabeledArg _ ->
      unsupported "labeled argument without label"
  | OptionalArg { label=Some label; value=None } ->
      Doc.concat [ Doc.text "?"; token_doc label ]
  | OptionalArg { label=Some label; value=Some value } ->
      Doc.concat [ Doc.text "?"; token_doc label; Doc.text ":"; expr_apply_argument_doc value ]
  | OptionalArg _ ->
      unsupported "optional argument without label"
  | LocalOpen _ ->
      local_open_expr_doc expr
  | LetModule _ ->
      let_module_expr_doc expr
  | FirstClassModule ->
      first_class_module_expr_doc expr
  | LetException _
  | Extension
  | Unreachable
  | Object
  | New
  | Attribute _
  | Error _
  | Unknown _ ->
      unsupported "unsupported expression"
  | BindingOperator _ ->
      binding_operator_expr_doc expr

and record_expr_field_doc = fun (field: Ast.RecordExpr.field) ->
  match field.path with
  | Some path -> (
      match field.value with
      | Some value -> Doc.concat [ path_doc path; Doc.space; Doc.equal; Doc.space; expr_doc value ]
      | None -> path_doc path
    )
  | None -> unsupported "unsupported record expression field"

and record_expr_fields_doc = fun expr ->
  let fields = ref [] in
  Ast.RecordExpr.for_each_field
    expr
    ~fn:(fun field -> fields := record_expr_field_doc field :: !fields);
  List.reverse !fields |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])

and record_expr_doc = fun expr ->
  let fields = record_expr_fields_doc expr in
  match Ast.RecordExpr.base expr with
  | Some base ->
      Doc.concat
        [ Doc.lbrace; Doc.space; expr_doc base; Doc.space; Doc.text "with"; (
            match fields with
            | Doc.Empty -> Doc.empty
            | fields -> Doc.concat [ Doc.space; fields ]
          ); Doc.space; Doc.rbrace; ]
  | None -> (
      match fields with
      | Doc.Empty -> Doc.concat [ Doc.lbrace; Doc.rbrace ]
      | fields -> Doc.concat [ Doc.lbrace; Doc.space; fields; Doc.space; Doc.rbrace ]
    )

and local_open_expr_doc = fun expr ->
  let local_open =
    match Ast.LocalOpenExpr.cast expr with
    | Some local_open -> local_open
    | None -> unsupported "unsupported local open expression"
  in
  match Ast.LocalOpenExpr.view local_open with
  | LetOpen {
    let_token=Some let_token;
    open_token=Some open_token;
    bang_token;
    module_path=Some module_path;
    in_token=Some in_token;
    body=Some body;

  } ->
      Doc.concat
        [
          token_doc let_token;
          Doc.space;
          token_doc open_token;
          optional_token_doc bang_token;
          Doc.space;
          path_doc module_path;
          Doc.space;
          token_doc in_token;
          Doc.space;
          expr_doc body;
        ]
  | LetOpen _ -> unsupported "incomplete let open expression"
  | Delimited {
    module_path=Some module_path;
    dot_token=Some dot_token;
    opening_token=Some opening_token;
    body=Some body;
    closing_token=Some closing_token;

  } -> Doc.concat
    [
      path_doc module_path;
      token_doc dot_token;
      token_doc opening_token;
      expr_doc body;
      token_doc closing_token;
    ]
  | Delimited _ -> unsupported "incomplete delimited local open expression"

and let_module_path_body_doc = fun expr ->
  let doc = ref None in
  Ast.LetModuleExpr.for_each_module_body_path_ident expr
    ~fn:(fun token ->
      let segment = token_doc token in
      doc := Some (
        match !doc with
        | None -> segment
        | Some doc -> Doc.concat [ doc; Doc.text "."; segment ]
      ));
  match !doc with
  | Some doc -> doc
  | None -> unsupported "let module expression path body without identifiers"

and let_module_body_doc = fun expr ->
  match Ast.LetModuleExpr.module_body expr with
  | Ast.LetModuleExpr.Path -> let_module_path_body_doc expr
  | Ast.LetModuleExpr.EmptyStruct -> Doc.concat [ Doc.text "struct"; Doc.space; Doc.text "end" ]
  | Ast.LetModuleExpr.Unsupported -> unsupported "unsupported let module body"

and let_module_expr_doc = fun expr ->
  let module_expr =
    match Ast.LetModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported "unsupported let module expression"
  in
  match Ast.LetModuleExpr.let_token module_expr, Ast.LetModuleExpr.module_token module_expr, Ast.LetModuleExpr.name
    module_expr, Ast.LetModuleExpr.equals_token module_expr, Ast.LetModuleExpr.in_token module_expr, Ast.LetModuleExpr.body
    module_expr with
  | Some let_token, Some module_token, Some name, Some equals_token, Some in_token, Some body ->
      Doc.concat
        [
          token_doc let_token;
          Doc.space;
          token_doc module_token;
          Doc.space;
          token_doc name;
          Doc.space;
          token_doc equals_token;
          Doc.space;
          let_module_body_doc module_expr;
          Doc.space;
          token_doc in_token;
          Doc.space;
          expr_doc body;
        ]
  | _ -> unsupported "incomplete let module expression"

and first_class_module_path_doc = fun expr ->
  let segments = ref [] in
  Ast.FirstClassModuleExpr.for_each_module_path_ident
    expr
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "first-class module expression without module path"
  | segments -> Doc.join (Doc.text ".") segments

and first_class_module_ascription_doc = fun expr ->
  let segments = ref [] in
  Ast.FirstClassModuleExpr.for_each_ascription_path_ident
    expr
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "first-class module expression without module type path"
  | segments -> Doc.join (Doc.text ".") segments

and first_class_module_expr_doc = fun expr ->
  let module_expr =
    match Ast.FirstClassModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported "unsupported first-class module expression"
  in
  match Ast.FirstClassModuleExpr.opening_token module_expr, Ast.FirstClassModuleExpr.module_token module_expr, Ast.FirstClassModuleExpr.module_path
    module_expr, Ast.FirstClassModuleExpr.ascription module_expr, Ast.FirstClassModuleExpr.closing_token
    module_expr with
  | Some opening_token, Some module_token, Ast.FirstClassModuleExpr.ModulePath, Ast.FirstClassModuleExpr.NoAscription, Some closing_token ->
      Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          first_class_module_path_doc module_expr;
          token_doc closing_token;
        ]
  | Some opening_token, Some module_token, Ast.FirstClassModuleExpr.ModulePath, Ast.FirstClassModuleExpr.PathAscription, Some closing_token -> (
      match Ast.FirstClassModuleExpr.colon_token module_expr with
      | Some colon_token -> Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          first_class_module_path_doc module_expr;
          Doc.space;
          token_doc colon_token;
          Doc.space;
          first_class_module_ascription_doc module_expr;
          token_doc closing_token;
        ]
      | None -> unsupported "first-class module ascription without colon token"
    )
  | _ ->
      unsupported "unsupported first-class module expression"

and binding_operator_clause_doc = fun (clause: Ast.BindingOperatorExpr.clause) ->
  match clause.keyword, clause.operator with
  | Some keyword, Some operator -> Doc.concat
    [ token_doc keyword; token_doc operator; Doc.space; let_binding_doc clause.binding ]
  | _ -> unsupported "incomplete binding operator clause"

and binding_operator_expr_doc = fun expr ->
  let view =
    match Ast.BindingOperatorExpr.cast expr with
    | Some view -> view
    | None -> unsupported "unsupported binding operator expression"
  in
  let clauses = ref [] in
  Ast.BindingOperatorExpr.for_each_clause
    view
    ~fn:(fun clause -> clauses := binding_operator_clause_doc clause :: !clauses);
  match List.reverse !clauses, Ast.BindingOperatorExpr.in_token view, Ast.BindingOperatorExpr.body view with
  | [], _, _ -> unsupported "binding operator expression without binding"
  | _, None, _ -> unsupported "binding operator expression without in"
  | _, _, None -> unsupported "binding operator expression without body"
  | clauses, Some in_token, Some body -> Doc.concat
    [ Doc.join Doc.space clauses; Doc.space; token_doc in_token; Doc.space; expr_doc body; ]

and let_binding_doc = fun binding ->
  let parts = let_binding_parts binding in
  match parts.pattern, parts.body with
  | Some pattern, Some body ->
      Doc.concat
        [ pattern_doc pattern; (
            match parts.parameters with
            | [] -> Doc.empty
            | parameters -> Doc.concat
              [ Doc.space; Doc.join Doc.space (List.map parameters ~fn:pattern_doc) ]
          ); (
            match parts.annotation with
            | Some annotation -> Doc.concat
              [ Doc.space; Doc.text ":"; Doc.space; type_expr_doc annotation ]
            | None -> Doc.empty
          ); Doc.space; Doc.equal; Doc.space; expr_doc body; ]
  | _ -> unsupported "incomplete let binding"

and let_bindings_doc = fun ~keyword ~rec_token node ->
  match let_binding_nodes node with
  | [] -> unsupported (keyword ^ " declaration without binding")
  | first :: rest ->
      let rest =
        List.map
          rest
          ~fn:(fun binding ->
            Doc.concat [ Doc.space; Doc.text "and"; Doc.space; let_binding_doc binding ])
      in
      Doc.concat
        (
          [ Doc.text keyword; (
              match rec_token with
              | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
              | None -> Doc.space
            ); let_binding_doc first; ] @ rest
        )

let let_decl_doc = fun decl ->
  let_bindings_doc ~keyword:"let" ~rec_token:(Ast.LetDeclaration.rec_token decl) decl

let type_parameter_doc = function
  | Ast.TypeDeclaration.Named { name; quote; variance; injective } -> Doc.concat
    [
      optional_token_doc variance;
      optional_token_doc injective;
      optional_token_doc quote;
      token_doc name;
    ]
  | Ast.TypeDeclaration.Wildcard { wildcard; variance; injective } -> Doc.concat
    [ optional_token_doc variance; optional_token_doc injective; token_doc wildcard ]

let type_parameters_doc = fun decl ->
  let parameters = ref [] in
  Ast.TypeDeclaration.for_each_parameter
    decl
    ~fn:(fun param -> parameters := type_parameter_doc param :: !parameters);
  match List.reverse !parameters with
  | [] -> Doc.empty
  | [ parameter ] -> Doc.concat [ parameter; Doc.space ]
  | parameters -> Doc.concat
    [ Doc.lparen; Doc.join (Doc.concat [ Doc.comma; Doc.space ]) parameters; Doc.rparen; Doc.space; ]

let type_decl_doc = fun decl ->
  match Ast.TypeDeclaration.name decl with
  | None -> unsupported "type declaration without name"
  | Some name -> (
      match Ast.TypeDeclaration.manifest decl with
      | Some manifest -> Doc.concat
        [
          Doc.text "type";
          Doc.space;
          type_parameters_doc decl;
          token_doc name;
          Doc.space;
          Doc.equal;
          Doc.space;
          type_expr_doc manifest;
        ]
      | None -> Doc.concat [ Doc.text "type"; Doc.space; type_parameters_doc decl; token_doc name ]
    )

let module_decl_path_body_doc = fun decl ->
  let segments = ref [] in
  Ast.ModuleDeclaration.for_each_body_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "module declaration path body without identifiers"
  | segments -> Doc.join (Doc.text ".") segments

let module_type_decl_path_body_doc = fun decl ->
  let segments = ref [] in
  Ast.ModuleTypeDeclaration.for_each_body_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "module type declaration path body without identifiers"
  | segments -> Doc.join (Doc.text ".") segments

let module_decl_body_doc = fun decl ->
  match Ast.ModuleDeclaration.body decl with
  | Path -> module_decl_path_body_doc decl
  | EmptyStruct -> Doc.concat [ Doc.text "struct"; Doc.space; Doc.text "end" ]
  | EmptySig -> Doc.concat [ Doc.text "sig"; Doc.space; Doc.text "end" ]
  | Unsupported -> unsupported "unsupported module declaration body"

let module_type_decl_body_doc = fun decl ->
  match Ast.ModuleTypeDeclaration.body decl with
  | Abstract -> Doc.empty
  | Path -> module_type_decl_path_body_doc decl
  | EmptySig -> Doc.concat [ Doc.text "sig"; Doc.space; Doc.text "end" ]
  | Unsupported -> unsupported "unsupported module type declaration body"

let module_decl_doc = fun decl ->
  match Ast.ModuleDeclaration.name decl with
  | Some name ->
      let head = Doc.concat
        [ Doc.text "module"; (
            match Ast.ModuleDeclaration.rec_token decl with
            | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
            | None -> Doc.space
          ); token_doc name; ]
      in
      (
        match Ast.ModuleDeclaration.separator_token decl with
        | Some separator when Kind.(Ast.Token.kind separator = COLON) -> Doc.concat
          [ head; token_doc separator; Doc.space; module_decl_body_doc decl ]
        | Some separator -> Doc.concat
          [ head; Doc.space; token_doc separator; Doc.space; module_decl_body_doc decl ]
        | None -> unsupported "module declaration without separator"
      )
  | None -> unsupported "module declaration without name"

let module_type_decl_doc = fun decl ->
  match Ast.ModuleTypeDeclaration.name decl with
  | None -> unsupported "module type declaration without name"
  | Some name ->
      let head = Doc.concat
        [ Doc.text "module"; Doc.space; Doc.text "type"; Doc.space; token_doc name ] in
      (
        match Ast.ModuleTypeDeclaration.equals_token decl, Ast.ModuleTypeDeclaration.body decl with
        | None, Abstract -> head
        | Some equals_token, (Path | EmptySig) -> Doc.concat
          [ head; Doc.space; token_doc equals_token; Doc.space; module_type_decl_body_doc decl ]
        | Some _, (Abstract | Unsupported) -> unsupported "unsupported module type declaration body"
        | None, _ -> unsupported "module type declaration body without equals token"
      )

let value_decl_doc = fun decl ->
  match Ast.ValueDeclaration.name decl, Ast.ValueDeclaration.type_annotation decl with
  | Some name, Some annotation -> Doc.concat
    [ Doc.text "val"; Doc.space; token_doc name; Doc.text ":"; Doc.space; type_expr_doc annotation ]
  | _ -> unsupported "incomplete value declaration"

let external_decl_doc = fun decl ->
  match Ast.ExternalDeclaration.name decl, Ast.ExternalDeclaration.type_annotation decl with
  | Some name, Some annotation ->
      let primitives = ref [] in
      Ast.ExternalDeclaration.for_each_primitive_string
        decl
        ~fn:(fun token -> primitives := token_doc token :: !primitives);
      (
        match List.reverse !primitives with
        | [] -> unsupported "external declaration without primitive strings"
        | primitives ->
            Doc.concat
              [
                Doc.text "external";
                Doc.space;
                token_doc name;
                Doc.text ":";
                Doc.space;
                type_expr_doc annotation;
                Doc.space;
                Doc.equal;
                Doc.space;
                Doc.join Doc.space primitives;
              ]
      )
  | _ -> unsupported "incomplete external declaration"

let open_decl_doc = fun decl -> Doc.concat [ Doc.text "open"; Doc.space; open_path_doc decl ]

let include_decl_doc = fun decl ->
  Doc.concat [ Doc.text "include"; Doc.space; include_path_doc decl ]

let exception_decl_doc = fun decl ->
  match Ast.ExceptionDeclaration.name decl with
  | Some name -> Doc.concat [ Doc.text "exception"; Doc.space; token_doc name ]
  | None -> unsupported "exception declaration without name"

let structure_item_doc = fun item ->
  match Ast.StructureItem.view item with
  | Let decl ->
      let_decl_doc decl
  | Type decl ->
      type_decl_doc decl
  | Module decl ->
      module_decl_doc decl
  | Open decl ->
      open_decl_doc decl
  | Include decl ->
      include_decl_doc decl
  | External decl ->
      external_decl_doc decl
  | Exception decl ->
      exception_decl_doc decl
  | ModuleType decl ->
      module_type_decl_doc decl
  | Expr expr_item -> (
      match Ast.ExprItem.expr expr_item with
      | Some expr -> expr_doc expr
      | None -> unsupported "expression item without expression"
    )
  | Class _
  | Extension _
  | Attribute _
  | Error _
  | Unknown _ ->
      unsupported "unsupported structure item"

let signature_item_doc = fun item ->
  match Ast.SignatureItem.view item with
  | Value decl -> value_decl_doc decl
  | Type decl -> type_decl_doc decl
  | Module decl -> module_decl_doc decl
  | Open decl -> open_decl_doc decl
  | Include decl -> include_decl_doc decl
  | External decl -> external_decl_doc decl
  | Exception decl -> exception_decl_doc decl
  | ModuleType decl -> module_type_decl_doc decl
  | Class _
  | Extension _
  | Attribute _
  | Error _
  | Unknown _ -> unsupported "unsupported signature item"

let implementation_doc = fun implementation ->
  let docs = ref [] in
  Ast.Implementation.for_each_item
    implementation
    ~fn:(fun item -> docs := structure_item_doc item :: !docs);
  Doc.lines (List.reverse !docs)

let interface_doc = fun interface ->
  let docs = ref [] in
  Ast.Interface.for_each_item interface ~fn:(fun item -> docs := signature_item_doc item :: !docs);
  Doc.lines (List.reverse !docs)

let source_file = fun source_file ->
  try
    match Ast.SourceFile.view source_file with
    | Empty -> Ok Doc.empty
    | Implementation implementation -> Ok (implementation_doc implementation)
    | Interface interface -> Ok (interface_doc interface)
  with
  | Unsupported err -> Error err
