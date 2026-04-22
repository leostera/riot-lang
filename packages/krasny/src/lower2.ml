open Std
open Std.Collections
module Ast = Syn.Ast2
module Doc = Doc

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
      let parts = ref [] in
      Ast.Pattern.for_each_child_pattern
        pattern
        ~fn:(fun child -> parts := pattern_doc child :: !parts);
      Doc.join (Doc.concat [ Doc.comma; Doc.space ]) (List.reverse !parts)
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
  | Or _
  | List
  | Array
  | Record
  | PolyVariant
  | Extension
  | Attribute _
  | LocalOpen
  | LocallyAbstractType
  | FirstClassModule
  | Interval _
  | Lazy _
  | Exception _
  | LabeledParam _
  | OptionalParam _
  | OptionalParamDefault _
  | Error _
  | Unknown _ ->
      unsupported "unsupported pattern"

let rec expr_doc = fun expr ->
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
  | If _ ->
      unsupported "incomplete if expression"
  | Tuple ->
      let parts = ref [] in
      Ast.Expr.for_each_child_expr expr ~fn:(fun child -> parts := expr_doc child :: !parts);
      Doc.join (Doc.concat [ Doc.comma; Doc.space ]) (List.reverse !parts)
  | Let _
  | LocalOpen _
  | LetModule _
  | LetException _
  | BindingOperator _
  | FirstClassModule
  | Extension
  | Unreachable
  | Object
  | New
  | Match _
  | Fun _
  | Function _
  | Try _
  | While _
  | For _
  | Assert _
  | Lazy _
  | Attribute _
  | Sequence _
  | Assign _
  | FieldAccess _
  | MethodCall _
  | PolyVariant _
  | List
  | Array
  | Record
  | RecordUpdate
  | ArrayIndex _
  | StringIndex _
  | LabeledArg _
  | OptionalArg _
  | Error _
  | Unknown _ ->
      unsupported "unsupported expression"

let let_binding_doc = fun binding ->
  let view = Ast.LetBinding.view binding in
  match view.pattern, view.body with
  | Some pattern, Some body -> Doc.concat
    [ pattern_doc pattern; Doc.space; Doc.equal; Doc.space; expr_doc body ]
  | _ -> unsupported "incomplete let binding"

let let_decl_doc = fun decl ->
  match Ast.LetDeclaration.first_binding decl with
  | Some binding ->
      Doc.concat
        [ Doc.text "let"; (
            match Ast.LetDeclaration.rec_token decl with
            | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
            | None -> Doc.space
          ); let_binding_doc binding; ]
  | None -> unsupported "let declaration without binding"

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
