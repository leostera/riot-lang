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
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match Ast.Pattern.cast child with
      | Some pattern -> docs := pattern :: !docs
      | None -> ());
  List.reverse !docs

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token
    node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Kind.(Ast.Token.kind token = IDENT) then
            found := Some token);
  !found

let let_binding_nodes = fun node ->
  let bindings = ref [] in
  Ast.Node.for_each_child_node
    node
    ~fn:(fun child ->
      match Ast.LetBinding.cast child with
      | Some binding -> bindings := binding :: !bindings
      | None -> ());
  List.reverse !bindings

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
  | Record
  | Extension
  | Attribute _
  | LocalOpen
  | LocallyAbstractType
  | FirstClassModule
  | Interval _
  | Lazy _
  | Exception _
  | Error _
  | Unknown _ ->
      unsupported "unsupported pattern"

and parameter_doc = fun parameter ->
  match Ast.Parameter.view parameter with
  | Labeled { label=Some label; pattern=None } ->
      Doc.concat [ Doc.text "~"; token_doc label ]
  | Labeled { label=Some label; pattern=Some pattern } ->
      Doc.concat [ Doc.text "~"; token_doc label; Doc.text ":"; pattern_doc pattern ]
  | Labeled _ ->
      unsupported "labeled parameter without label"
  | Optional { label=Some label; pattern=None } ->
      Doc.concat [ Doc.text "?"; token_doc label ]
  | Optional { label=Some label; pattern=Some pattern } ->
      Doc.concat [ Doc.text "?"; token_doc label; Doc.text ":"; pattern_doc pattern ]
  | Optional _ ->
      unsupported "optional parameter without label"
  | OptionalDefault { label=Some label; pattern=Some pattern; default=Some default } ->
      Doc.concat
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
  | OptionalDefault _ ->
      unsupported "incomplete optional parameter default"
  | Unknown _ ->
      unsupported "unsupported parameter"

and match_case_doc = fun match_case ->
  let view = Ast.MatchCase.view match_case in
  match view.pattern, view.body with
  | Some pattern, Some body ->
      let guard =
        match view.guard with
        | Some guard -> Doc.concat [ Doc.space; Doc.text "when"; Doc.space; expr_doc guard ]
        | None -> Doc.empty
      in
      Doc.concat [ Doc.bar; Doc.space; pattern_doc pattern; guard; Doc.space; Doc.arrow; Doc.space; expr_doc body ]
  | _ ->
      unsupported "incomplete match case"

and expr_doc = fun expr ->
  match Ast.Expr.view expr with
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
      Doc.concat [ expr_doc left; Doc.space; token_doc operator; Doc.space; expr_doc right ]
  | Infix _ ->
      unsupported "incomplete infix expression"
  | Prefix { operator=Some operator; operand=Some operand } ->
      Doc.concat [ token_doc operator; expr_doc operand ]
  | Prefix _ ->
      unsupported "incomplete prefix expression"
  | Apply { callee=Some callee; argument=Some argument } ->
      Doc.concat [ expr_doc callee; Doc.space; expr_doc argument ]
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
      child_expr_docs expr
      |> List.map ~fn:expr_doc
      |> Doc.join (Doc.concat [ Doc.comma; Doc.space ])
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
      | parameters ->
          Doc.concat [
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
      Ast.Expr.for_each_match_case expr ~fn:(fun match_case -> cases := match_case_doc match_case :: !cases);
      Doc.concat [
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
      Ast.Expr.for_each_match_case expr ~fn:(fun match_case -> cases := match_case_doc match_case :: !cases);
      Doc.concat [ Doc.text "function"; Doc.space; Doc.join Doc.space (List.reverse !cases) ]
  | Function _ ->
      unsupported "incomplete function expression"
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
      Doc.concat [ Doc.text "~"; token_doc label; Doc.text ":"; expr_doc value ]
  | LabeledArg _ ->
      unsupported "labeled argument without label"
  | OptionalArg { label=Some label; value=None } ->
      Doc.concat [ Doc.text "?"; token_doc label ]
  | OptionalArg { label=Some label; value=Some value } ->
      Doc.concat [ Doc.text "?"; token_doc label; Doc.text ":"; expr_doc value ]
  | OptionalArg _ ->
      unsupported "optional argument without label"
  | LocalOpen _
  | LetModule _
  | LetException _
  | BindingOperator _
  | FirstClassModule
  | Extension
  | Unreachable
  | Object
  | New
  | Try _
  | While _
  | For _
  | Attribute _
  | Record
  | RecordUpdate
  | Error _
  | Unknown _ ->
      unsupported "unsupported expression"

and let_binding_doc = fun binding ->
  let view = Ast.LetBinding.view binding in
  match view.pattern, view.body with
  | Some pattern, Some body -> Doc.concat
    [
      pattern_doc pattern;
      (
        let parameters = ref [] in
        Ast.LetBinding.for_each_parameter
          binding
          ~fn:(fun parameter -> parameters := pattern_doc parameter :: !parameters);
        match List.reverse !parameters with
        | [] -> Doc.empty
        | parameters -> Doc.concat [ Doc.space; Doc.join Doc.space parameters ]
      );
      Doc.space;
      Doc.equal;
      Doc.space;
      expr_doc body;
    ]
  | _ -> unsupported "incomplete let binding"

and let_bindings_doc = fun ~keyword ~rec_token node ->
  match let_binding_nodes node with
  | [] ->
      unsupported (keyword ^ " declaration without binding")
  | first :: rest ->
      let rest =
        List.map
          rest
          ~fn:(fun binding -> Doc.concat [ Doc.space; Doc.text "and"; Doc.space; let_binding_doc binding ])
      in
      Doc.concat
        ([
          Doc.text keyword;
          (
            match rec_token with
            | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
            | None -> Doc.space
          );
          let_binding_doc first;
        ]
        @ rest)

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

let module_decl_doc = fun decl ->
  match Ast.ModuleDeclaration.name decl with
  | Some name ->
      Doc.concat
        [ Doc.text "module"; (
            match Ast.ModuleDeclaration.rec_token decl with
            | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
            | None -> Doc.space
          ); token_doc name; ]
  | None -> unsupported "module declaration without name"

let value_decl_doc = fun decl ->
  match Ast.ValueDeclaration.name decl, Ast.ValueDeclaration.type_annotation decl with
  | Some name, Some annotation -> Doc.concat
    [ Doc.text "val"; Doc.space; token_doc name; Doc.text ":"; Doc.space; type_expr_doc annotation ]
  | _ -> unsupported "incomplete value declaration"

let external_decl_doc = fun decl ->
  match Ast.ExternalDeclaration.name decl, Ast.ExternalDeclaration.type_annotation decl with
  | Some name, Some annotation -> Doc.concat
    [
      Doc.text "external";
      Doc.space;
      token_doc name;
      Doc.text ":";
      Doc.space;
      type_expr_doc annotation;
    ]
  | _ -> unsupported "incomplete external declaration"

let open_decl_doc = fun decl -> Doc.concat [ Doc.text "open"; Doc.space; open_path_doc decl ]

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
  | Expr expr_item -> (
      match Ast.ExprItem.expr expr_item with
      | Some expr -> expr_doc expr
      | None -> unsupported "expression item without expression"
    )
  | ModuleType _
  | Include _
  | External _
  | Exception _
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
  | External decl -> external_decl_doc decl
  | ModuleType _
  | Include _
  | Exception _
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
