open Std
open Std.Data

let span_to_json (span : Ceibo.Span.t) =
  Json.Object
    [
      ("start", Json.Int span.start);
      ("end", Json.Int span.end_);
    ]

let syntax_node_to_json syntax_node =
  Json.Object
    [
      ( "kind",
        Json.String
          (Syntax_kind.to_string (Ceibo.Red.SyntaxNode.kind syntax_node)) );
      ("span", span_to_json (Ceibo.Red.SyntaxNode.span syntax_node));
    ]

let syntax_token_to_json syntax_token =
  Json.Object
    [
      ( "kind",
        Json.String
          (Syntax_kind.to_string (Ceibo.Red.SyntaxToken.kind syntax_token)) );
      ("text", Json.String (Ceibo.Red.SyntaxToken.text syntax_token));
      ("span", span_to_json (Ceibo.Red.SyntaxToken.span syntax_token));
    ]

let option_to_json to_json = function
  | Some value -> to_json value
  | None -> Json.Null

let token_to_json token = syntax_token_to_json (Cst.Token.syntax_token token)

let module_path_to_json path =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.ModulePath.syntax_node path));
      ( "segments",
        Json.Array (List.map token_to_json (Cst.ModulePath.segments path)) );
    ]

let rec pattern_literal_to_json = function
  | Cst.PatternLiteral.String { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "string");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.PatternLiteral.Int { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "int");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.PatternLiteral.Bool { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "bool");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.PatternLiteral.Unit { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "unit");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

and pattern_to_json = function
  | Cst.Pattern.Identifier { syntax_node; name_token } ->
      Json.Object
        [
          ("tag", Json.String "identifier");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
        ]
  | Cst.Pattern.Wildcard { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "wildcard");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]
  | Cst.Pattern.Literal literal ->
      Json.Object
        [
          ("tag", Json.String "literal");
          ("literal", pattern_literal_to_json literal);
        ]
  | Cst.Pattern.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", pattern_to_json inner);
        ]
  | Cst.Pattern.Unknown syntax_node ->
      Json.Object
        [
          ("tag", Json.String "unknown");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

and parameter_to_json = function
  | Cst.Parameter.Positional { syntax_node; name_token } ->
      Json.Object
        [
          ("tag", Json.String "positional");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", option_to_json token_to_json name_token);
        ]
  | Cst.Parameter.Labeled { syntax_node; label_token; binding_name_token } ->
      Json.Object
        [
          ("tag", Json.String "labeled");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label_token", token_to_json label_token);
          ("binding_name_token", option_to_json token_to_json binding_name_token);
        ]
  | Cst.Parameter.Optional
      { syntax_node; label_token; binding_name_token; has_default } ->
      Json.Object
        [
          ("tag", Json.String "optional");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label_token", token_to_json label_token);
          ("binding_name_token", option_to_json token_to_json binding_name_token);
          ("has_default", Json.Bool has_default);
        ]
  | Cst.Parameter.LocallyAbstract syntax_node ->
      Json.Object
        [
          ("tag", Json.String "locally_abstract");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]
  | Cst.Parameter.Unknown syntax_node ->
      Json.Object
        [
          ("tag", Json.String "unknown");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

and literal_to_json = function
  | Cst.Literal.String { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "string");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.Literal.Int { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "int");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.Literal.Bool { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "bool");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.Literal.Unit { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "unit");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

and expression_to_json = function
  | Cst.Expression.Path { syntax_node; path } ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("path", module_path_to_json path);
        ]
  | Cst.Expression.Literal literal ->
      Json.Object
        [
          ("tag", Json.String "literal");
          ("literal", literal_to_json literal);
        ]
  | Cst.Expression.Apply { syntax_node; callee; argument } ->
      Json.Object
        [
          ("tag", Json.String "apply");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("callee", expression_to_json callee);
          ("argument", expression_to_json argument);
        ]
  | Cst.Expression.FieldAccess { syntax_node; receiver; field_name } ->
      Json.Object
        [
          ("tag", Json.String "field_access");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("receiver", expression_to_json receiver);
          ("field_name", token_to_json field_name);
        ]
  | Cst.Expression.Infix { syntax_node; left; operator_token; right } ->
      Json.Object
        [
          ("tag", Json.String "infix");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("left", expression_to_json left);
          ("operator_token", token_to_json operator_token);
          ("right", expression_to_json right);
        ]
  | Cst.Expression.Tuple { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "tuple");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map expression_to_json elements));
        ]
  | Cst.Expression.List { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "list");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map expression_to_json elements));
        ]
  | Cst.Expression.Array { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "array");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map expression_to_json elements));
        ]
  | Cst.Expression.Record { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]
  | Cst.Expression.LocalOpen { syntax_node; module_path; body; via_let_open } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", module_path_to_json module_path);
          ("body", expression_to_json body);
          ("via_let_open", Json.Bool via_let_open);
        ]
  | Cst.Expression.Fun { syntax_node; parameters; body } ->
      Json.Object
        [
          ("tag", Json.String "fun");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("parameters", Json.Array (List.map parameter_to_json parameters));
          ("body", expression_to_json body);
        ]
  | Cst.Expression.Function { syntax_node; cases } ->
      Json.Object
        [
          ("tag", Json.String "function");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("cases", Json.Array (List.map match_case_to_json cases));
        ]
  | Cst.Expression.Let
      { syntax_node; binding_pattern; bound_value; body; is_recursive } ->
      Json.Object
        [
          ("tag", Json.String "let");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binding_pattern", pattern_to_json binding_pattern);
          ("bound_value", expression_to_json bound_value);
          ("body", expression_to_json body);
          ("is_recursive", Json.Bool is_recursive);
        ]
  | Cst.Expression.Match { syntax_node; scrutinee; cases } ->
      Json.Object
        [
          ("tag", Json.String "match");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("scrutinee", expression_to_json scrutinee);
          ("cases", Json.Array (List.map match_case_to_json cases));
        ]
  | Cst.Expression.Try { syntax_node; body; cases } ->
      Json.Object
        [
          ("tag", Json.String "try");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("body", expression_to_json body);
          ("cases", Json.Array (List.map match_case_to_json cases));
        ]
  | Cst.Expression.If { syntax_node; condition; then_branch; else_branch } ->
      Json.Object
        [
          ("tag", Json.String "if");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("condition", expression_to_json condition);
          ("then_branch", expression_to_json then_branch);
          ("else_branch", option_to_json expression_to_json else_branch);
        ]
  | Cst.Expression.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", expression_to_json inner);
        ]
  | Cst.Expression.Unknown syntax_node ->
      Json.Object
        [
          ("tag", Json.String "unknown");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

and match_case_to_json { syntax_node; pattern; guard; body } =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("pattern", pattern_to_json pattern);
      ("guard", option_to_json expression_to_json guard);
      ("body", expression_to_json body);
    ]

let type_variable_to_json type_variable =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeVariable.syntax_node type_variable));
      ("name_token", token_to_json (Cst.TypeVariable.name_token type_variable));
    ]

let type_parameter_to_json type_parameter =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeParameter.syntax_node type_parameter));
      ( "type_variable",
        option_to_json type_variable_to_json
          (Cst.TypeParameter.type_variable type_parameter) );
    ]

let record_field_to_json field =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.RecordField.syntax_node field));
      ("field_name", token_to_json (Cst.RecordField.field_name_token field));
      ("is_mutable", Json.Bool (Cst.RecordField.is_mutable field));
    ]

let variant_constructor_to_json constr =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.VariantConstructor.syntax_node constr));
      ( "constructor_name",
        token_to_json (Cst.VariantConstructor.constructor_name_token constr) );
    ]

let poly_variant_tag_to_json tag =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.PolyVariantTag.syntax_node tag));
      ("tag_name", token_to_json (Cst.PolyVariantTag.tag_name_token tag));
    ]

let type_definition_to_json = function
  | Cst.TypeDefinition.Abstract ->
      Json.Object [ ("tag", Json.String "abstract") ]
  | Cst.TypeDefinition.Alias { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "alias");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]
  | Cst.TypeDefinition.Record fields ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("fields", Json.Array (List.map record_field_to_json fields));
        ]
  | Cst.TypeDefinition.Variant constructors ->
      Json.Object
        [
          ("tag", Json.String "variant");
          ( "constructors",
            Json.Array (List.map variant_constructor_to_json constructors) );
        ]
  | Cst.TypeDefinition.PolyVariant tags ->
      Json.Object
        [
          ("tag", Json.String "poly_variant");
          ("tags", Json.Array (List.map poly_variant_tag_to_json tags));
        ]
  | Cst.TypeDefinition.Other syntax_node ->
      Json.Object
        [
          ("tag", Json.String "other");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

let type_declaration_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeDeclaration.syntax_node decl));
      ("type_name", module_path_to_json (Cst.TypeDeclaration.type_name decl));
      ( "type_params",
        Json.Array
          (List.map type_parameter_to_json (Cst.TypeDeclaration.type_params decl))
      );
      ( "type_definition",
        type_definition_to_json (Cst.TypeDeclaration.type_definition decl) );
    ]

let let_binding_to_json binding =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.LetBinding.syntax_node binding));
      ("binding_name", token_to_json (Cst.LetBinding.binding_name_token binding));
      ( "parameters",
        Json.Array (List.map parameter_to_json (Cst.LetBinding.parameters binding))
      );
      ("value", expression_to_json (Cst.LetBinding.value binding));
      ("is_recursive", Json.Bool (Cst.LetBinding.is_recursive binding));
    ]

let module_declaration_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.ModuleDeclaration.syntax_node decl));
      ("module_name", token_to_json (Cst.ModuleDeclaration.module_name_token decl));
    ]

let module_type_declaration_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.ModuleTypeDeclaration.syntax_node decl));
      ( "module_type_name",
        token_to_json (Cst.ModuleTypeDeclaration.module_type_name_token decl) );
    ]

let open_statement_to_json stmt =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.OpenStatement.syntax_node stmt));
      ("module_path", module_path_to_json (Cst.OpenStatement.module_path stmt));
      ("bang_token", option_to_json token_to_json (Cst.OpenStatement.bang_token stmt));
    ]

let item_to_json = function
  | Cst.Item.TypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "type_declaration");
          ("item", type_declaration_to_json decl);
        ]
  | Cst.Item.LetBinding binding ->
      Json.Object
        [
          ("tag", Json.String "let_binding");
          ("item", let_binding_to_json binding);
        ]
  | Cst.Item.ModuleDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_declaration");
          ("item", module_declaration_to_json decl);
        ]
  | Cst.Item.ModuleTypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_type_declaration");
          ("item", module_type_declaration_to_json decl);
        ]
  | Cst.Item.OpenStatement stmt ->
      Json.Object
        [
          ("tag", Json.String "open_statement");
          ("item", open_statement_to_json stmt);
        ]
  | Cst.Item.Unknown syntax_node ->
      Json.Object
        [
          ("tag", Json.String "unknown");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]

let of_source_file source_file =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.SourceFile.syntax_node source_file));
      ("items", Json.Array (List.map item_to_json (Cst.SourceFile.items source_file)));
      ( "let_bindings",
        Json.Array
          (List.map let_binding_to_json (Cst.SourceFile.let_bindings source_file))
      );
      ( "expressions",
        Json.Array
          (List.map expression_to_json (Cst.SourceFile.expressions source_file))
      );
    ]

let of_error (error : Cst_builder.error) =
  Json.Object
    [
      ("message", Json.String error.message);
      ("syntax_kind", Json.String (Syntax_kind.to_string error.syntax_kind));
      ("span", span_to_json error.span);
      ("context", Json.Array (List.map Json.string error.context));
    ]

let of_result = function
  | Ok source_file ->
      Json.Object
        [
          ("status", Json.String "ok");
          ("cst", of_source_file source_file);
        ]
  | Error error ->
      Json.Object
        [
          ("status", Json.String "error");
          ("error", of_error error);
        ]
