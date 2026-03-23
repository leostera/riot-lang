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

let rec ident_to_json = function
  | Cst.Ident.Ident { syntax_node; name_token } ->
      Json.Object
        [
          ("tag", Json.String "ident");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
        ]
  | Cst.Ident.Qualified { syntax_node; prefix; dot_token; name_token } ->
      Json.Object
        [
          ("tag", Json.String "qualified");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("prefix", ident_to_json prefix);
          ("dot_token", token_to_json dot_token);
          ("name_token", token_to_json name_token);
        ]

let rec object_type_field_to_json
    ({ syntax_node; field_name; field_type } : Cst.object_type_field)
    =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("field_name", token_to_json field_name);
      ("field_type", core_type_to_json field_type);
    ]

and record_type_field_to_json
    ({ syntax_node; field_name; field_type; is_mutable; attributes } : Cst.record_type_field)
    =
  let attributes = List.map attribute_to_json attributes in
  Json.Object
    ([
       ("syntax_node", syntax_node_to_json syntax_node);
       ("field_name", token_to_json field_name);
       ("field_type", core_type_to_json field_type);
       ("is_mutable", Json.Bool is_mutable);
     ]
    @ if attributes = [] then [] else [ ("attributes", Json.Array attributes) ])

and poly_variant_tag_to_json ({ syntax_node; tag_name; payload_type } : Cst.poly_variant_tag)
    =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("tag_name", token_to_json tag_name);
      ("payload_type", option_to_json core_type_to_json payload_type);
    ]

and poly_variant_bound_to_json = function
  | Cst.PolyVariantBound.Exact ->
      Json.Object [ ("tag", Json.String "exact") ]
  | Cst.PolyVariantBound.UpperBound { marker_token } ->
      Json.Object
        [
          ("tag", Json.String "upper_bound");
          ("marker_token", token_to_json marker_token);
        ]
  | Cst.PolyVariantBound.LowerBound { marker_token } ->
      Json.Object
        [
          ("tag", Json.String "lower_bound");
          ("marker_token", token_to_json marker_token);
        ]

and row_field_to_json = function
  | Cst.RowField.Tag tag ->
      Json.Object
        [
          ("tag", Json.String "tag");
          ("field", poly_variant_tag_to_json tag);
        ]
  | Cst.RowField.Inherit { syntax_node; type_ } ->
      Json.Object
        [
          ("tag", Json.String "inherit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("type", core_type_to_json type_);
        ]

and type_binder_to_json = function
  | Cst.TypeBinder.Quoted { syntax_node; name_token } ->
      Json.Object
        [
          ("tag", Json.String "quoted");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
        ]
  | Cst.TypeBinder.Bare { name_token } ->
      Json.Object
        [
          ("tag", Json.String "bare");
          ("name_token", token_to_json name_token);
        ]

and arrow_label_to_json = function
  | Cst.ArrowLabel.Named { sigil_token; label_token } ->
      Json.Object
        [
          ("tag", Json.String "named");
          ("sigil_token", option_to_json token_to_json sigil_token);
          ("label_token", token_to_json label_token);
        ]
  | Cst.ArrowLabel.OptionalNamed { sigil_token; label_token } ->
      Json.Object
        [
          ("tag", Json.String "optional_named");
          ("sigil_token", token_to_json sigil_token);
          ("label_token", token_to_json label_token);
        ]

and core_type_to_json = function
  | Cst.CoreType.Wildcard { syntax_node; wildcard_token } ->
      Json.Object
        [
          ("tag", Json.String "wildcard");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("wildcard_token", token_to_json wildcard_token);
        ]
  | Cst.CoreType.Var { syntax_node; name_token } ->
      Json.Object
        [
          ("tag", Json.String "var");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
        ]
  | Cst.CoreType.Constr { syntax_node; constructor_path; arguments } ->
      Json.Object
        [
          ("tag", Json.String "constr");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("constructor_path", ident_to_json constructor_path);
          ("arguments", Json.Array (List.map core_type_to_json arguments));
        ]
  | Cst.CoreType.Class { syntax_node; hash_token; class_path; arguments } ->
      Json.Object
        [
          ("tag", Json.String "class");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("hash_token", token_to_json hash_token);
          ("class_path", ident_to_json class_path);
          ("arguments", Json.Array (List.map core_type_to_json arguments));
        ]
  | Cst.CoreType.Alias { syntax_node; type_; name_token } ->
      Json.Object
        [
          ("tag", Json.String "alias");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("type", core_type_to_json type_);
          ("name_token", token_to_json name_token);
        ]
  | Cst.CoreType.Attribute { syntax_node; type_; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("type", core_type_to_json type_);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.CoreType.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]
  | Cst.CoreType.Poly { syntax_node; binders; body } ->
      Json.Object
        [
          ("tag", Json.String "poly");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binders", Json.Array (List.map type_binder_to_json binders));
          ("body", core_type_to_json body);
        ]
  | Cst.CoreType.Arrow { syntax_node; label; parameter_type; result_type } ->
      Json.Object
        [
          ("tag", Json.String "arrow");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label", option_to_json arrow_label_to_json label);
          ("parameter_type", core_type_to_json parameter_type);
          ("result_type", core_type_to_json result_type);
        ]
  | Cst.CoreType.Tuple { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "tuple");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map core_type_to_json elements));
        ]
  | Cst.CoreType.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", core_type_to_json inner);
        ]
  | Cst.CoreType.LocalOpen { syntax_node; module_path; type_ } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
          ("type", core_type_to_json type_);
        ]
  | Cst.CoreType.PolyVariant poly_variant ->
      Json.Object
        [
          ("tag", Json.String "poly_variant");
          ("syntax_node", syntax_node_to_json (Cst.PolyVariant.syntax_node poly_variant));
          ("kind", poly_variant_bound_to_json (Cst.PolyVariant.kind poly_variant));
          ("fields", Json.Array (List.map row_field_to_json (Cst.PolyVariant.fields poly_variant)));
        ]
  | Cst.CoreType.Record { syntax_node; fields } ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map record_type_field_to_json fields));
        ]
  | Cst.CoreType.FirstClassModule { syntax_node; module_type } ->
      Json.Object
        [
          ("tag", Json.String "first_class_module");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_type", module_type_to_json module_type);
        ]
  | Cst.CoreType.Object { syntax_node; fields } ->
      Json.Object
        [
          ("tag", Json.String "object");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map object_type_field_to_json fields));
        ]

and module_type_constraint_to_json
    ({ syntax_node; constrained_type; replacement_type; is_destructive } :
      Cst.module_type_constraint) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("constrained_type", core_type_to_json constrained_type);
      ("replacement_type", core_type_to_json replacement_type);
      ("is_destructive", Json.Bool is_destructive);
    ]

and functor_parameter_to_json
    ({ syntax_node; name_token; module_type } : Cst.functor_parameter) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("name_token", token_to_json name_token);
      ("module_type", module_type_to_json module_type);
    ]

and module_type_to_json = function
  | Cst.ModuleType.Path path ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("path", ident_to_json path);
        ]
  | Cst.ModuleType.TypeOf { syntax_node; module_path } ->
      Json.Object
        [
          ("tag", Json.String "type_of");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
        ]
  | Cst.ModuleType.Signature { syntax_node; signature_syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "signature");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("signature_syntax_node", syntax_node_to_json signature_syntax_node);
        ]
  | Cst.ModuleType.Functor { syntax_node; parameters; result } ->
      Json.Object
        [
          ("tag", Json.String "functor");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("parameters", Json.Array (List.map functor_parameter_to_json parameters));
          ("result", module_type_to_json result);
        ]
  | Cst.ModuleType.With { syntax_node; base; constraints } ->
      Json.Object
        [
          ("tag", Json.String "with");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("base", module_type_to_json base);
          ( "constraints",
            Json.Array (List.map module_type_constraint_to_json constraints) );
        ]
  | Cst.ModuleType.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", module_type_to_json inner);
        ]
  | Cst.ModuleType.Attribute { syntax_node; module_type; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_type", module_type_to_json module_type);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ModuleType.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

and module_expression_to_json = function
  | Cst.ModuleExpression.Path path ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("path", ident_to_json path);
        ]
  | Cst.ModuleExpression.Structure { syntax_node; item_syntax_nodes } ->
      Json.Object
        [
          ("tag", Json.String "structure");
          ("syntax_node", syntax_node_to_json syntax_node);
          ( "item_syntax_nodes",
            Json.Array (List.map syntax_node_to_json item_syntax_nodes) );
        ]
  | Cst.ModuleExpression.Functor { syntax_node; parameters; body } ->
      Json.Object
        [
          ("tag", Json.String "functor");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("parameters", Json.Array (List.map functor_parameter_to_json parameters));
          ("body", module_expression_to_json body);
        ]
  | Cst.ModuleExpression.Apply { syntax_node; callee; argument } ->
      Json.Object
        [
          ("tag", Json.String "apply");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("callee", module_expression_to_json callee);
          ("argument", module_expression_to_json argument);
        ]
  | Cst.ModuleExpression.ApplyUnit { syntax_node; callee } ->
      Json.Object
        [
          ("tag", Json.String "apply_unit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("callee", module_expression_to_json callee);
        ]
  | Cst.ModuleExpression.Constraint
      { syntax_node; module_expression; module_type } ->
      Json.Object
        [
          ("tag", Json.String "constraint");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_expression", module_expression_to_json module_expression);
          ("module_type", module_type_to_json module_type);
        ]
  | Cst.ModuleExpression.Unpack { syntax_node; expression; module_type } ->
      Json.Object
        [
          ("tag", Json.String "unpack");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("expression", expression_to_json expression);
          ("module_type", option_to_json module_type_to_json module_type);
        ]
  | Cst.ModuleExpression.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", module_expression_to_json inner);
        ]
  | Cst.ModuleExpression.Attribute { syntax_node; module_expression; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_expression", module_expression_to_json module_expression);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ModuleExpression.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

and exception_declaration_to_json (decl : Cst.exception_declaration) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json decl.syntax_node);
      ("name_token", token_to_json decl.name_token);
    ]

and pattern_literal_to_json = function
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
  | Cst.PatternLiteral.Float { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "float");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.PatternLiteral.Char { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "char");
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
  | Cst.Pattern.Attribute { syntax_node; pattern; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("pattern", pattern_to_json pattern);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.Pattern.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]
  | Cst.Pattern.Lazy { syntax_node; pattern } ->
      Json.Object
        [
          ("tag", Json.String "lazy");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("pattern", pattern_to_json pattern);
        ]
  | Cst.Pattern.Exception { syntax_node; pattern } ->
      Json.Object
        [
          ("tag", Json.String "exception");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("pattern", pattern_to_json pattern);
        ]
  | Cst.Pattern.Range { syntax_node; lower_token; upper_token } ->
      Json.Object
        [
          ("tag", Json.String "range");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("lower_token", token_to_json lower_token);
          ("upper_token", token_to_json upper_token);
        ]
  | Cst.Pattern.Operator { syntax_node; operator_tokens } ->
      Json.Object
        [
          ("tag", Json.String "operator");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("operator_tokens", Json.Array (List.map token_to_json operator_tokens));
        ]
  | Cst.Pattern.FirstClassModule { syntax_node; name_token; module_type } ->
      Json.Object
        [
          ("tag", Json.String "first_class_module");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("module_type", option_to_json module_type_to_json module_type);
        ]
  | Cst.Pattern.Literal literal ->
      Json.Object
        [
          ("tag", Json.String "literal");
          ("literal", pattern_literal_to_json literal);
        ]
  | Cst.Pattern.PolyVariant { syntax_node; tag_token; payload } ->
      Json.Object
        [
          ("tag", Json.String "poly_variant");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("tag_token", token_to_json tag_token);
          ("payload", option_to_json pattern_to_json payload);
        ]
  | Cst.Pattern.PolyVariantInherit { syntax_node; type_path } ->
      Json.Object
        [
          ("tag", Json.String "poly_variant_inherit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("type_path", ident_to_json type_path);
        ]
  | Cst.Pattern.Constructor
      { syntax_node; constructor_path; existentials; arguments } ->
      Json.Object
        [
          ("tag", Json.String "constructor");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("constructor_path", ident_to_json constructor_path);
          ( "existentials",
            option_to_json
              constructor_pattern_existentials_to_json
              existentials );
          ("arguments", Json.Array (List.map pattern_to_json arguments));
        ]
  | Cst.Pattern.Tuple { syntax_node; elements; open_tail } ->
      Json.Object
        [
          ("tag", Json.String "tuple");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map tuple_pattern_element_to_json elements));
          ("open_tail", option_to_json tuple_pattern_open_tail_to_json open_tail);
        ]
  | Cst.Pattern.List { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "list");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map pattern_to_json elements));
        ]
  | Cst.Pattern.Array { syntax_node; elements } ->
      Json.Object
        [
          ("tag", Json.String "array");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("elements", Json.Array (List.map pattern_to_json elements));
        ]
  | Cst.Pattern.Record { syntax_node; fields; closedness } ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map record_pattern_field_to_json fields));
          ("closedness", record_pattern_closedness_to_json closedness);
        ]
  | Cst.Pattern.Cons { syntax_node; head; tail } ->
      Json.Object
        [
          ("tag", Json.String "cons");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("head", pattern_to_json head);
          ("tail", pattern_to_json tail);
        ]
  | Cst.Pattern.Or { syntax_node; alternatives } ->
      Json.Object
        [
          ("tag", Json.String "or");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("alternatives", Json.Array (List.map pattern_to_json alternatives));
        ]
  | Cst.Pattern.Alias { syntax_node; pattern; name_token } ->
      Json.Object
        [
          ("tag", Json.String "alias");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("pattern", pattern_to_json pattern);
          ("name_token", token_to_json name_token);
        ]
  | Cst.Pattern.Typed { syntax_node; pattern; type_ } ->
      Json.Object
        [
          ("tag", Json.String "typed");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("pattern", pattern_to_json pattern);
          ("type", core_type_to_json type_);
        ]
  | Cst.Pattern.Effect { syntax_node; effect_pattern; continuation } ->
      Json.Object
        [
          ("tag", Json.String "effect");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("effect", pattern_to_json effect_pattern);
          ("continuation", pattern_to_json continuation);
        ]
  | Cst.Pattern.LocalOpen { syntax_node; module_path; pattern } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
          ("pattern", pattern_to_json pattern);
        ]
  | Cst.Pattern.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", pattern_to_json inner);
        ]

and constructor_pattern_existentials_to_json
    ({ syntax_node; binders } : Cst.constructor_pattern_existentials) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("binders", Json.Array (List.map type_binder_to_json binders));
    ]

and record_pattern_field_to_json field =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json field.syntax_node);
      ("field_path", ident_to_json field.field_path);
      ("pattern", option_to_json pattern_to_json field.pattern);
    ]

and tuple_pattern_element_to_json ({ label_token; pattern } : Cst.tuple_pattern_element)
    =
  Json.Object
    [
      ("label_token", option_to_json token_to_json label_token);
      ("pattern", pattern_to_json pattern);
    ]

and tuple_pattern_open_tail_to_json
    ({ dotdot_token } : Cst.tuple_pattern_open_tail) =
  Json.Object [ ("dotdot_token", token_to_json dotdot_token) ]

and record_pattern_closedness_to_json = function
  | Cst.Closed ->
      Json.Object [ ("tag", Json.String "closed") ]
  | Cst.Open { wildcard_token } ->
      Json.Object
        [
          ("tag", Json.String "open");
          ("wildcard_token", token_to_json wildcard_token);
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
  | Cst.Parameter.LocallyAbstract { syntax_node; binders } ->
      Json.Object
        [
          ("tag", Json.String "locally_abstract");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binders", Json.Array (List.map type_binder_to_json binders));
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
  | Cst.Literal.Float { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "float");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("literal_token", token_to_json literal_token);
        ]
  | Cst.Literal.Char { syntax_node; literal_token } ->
      Json.Object
        [
          ("tag", Json.String "char");
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

and attribute_to_json (attr : Cst.attribute) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json attr.syntax_node);
      ("sigil_token", token_to_json attr.sigil_token);
      ("name", ident_to_json attr.name);
      ("payload_syntax_node", option_to_json syntax_node_to_json attr.payload_syntax_node);
      ("payload", option_to_json payload_to_json attr.payload);
    ]

and pattern_payload_to_json
    ({ pattern_syntax_node; guard_syntax_node } : Cst.pattern_payload) =
  Json.Object
    [
      ("pattern_syntax_node", syntax_node_to_json pattern_syntax_node);
      ( "guard_syntax_node",
        option_to_json syntax_node_to_json guard_syntax_node );
    ]

and payload_to_json = function
  | Cst.Payload.Structure { item_syntax_nodes } ->
      Json.Object
        [
          ("tag", Json.String "structure");
          ("item_syntax_nodes", Json.Array (List.map syntax_node_to_json item_syntax_nodes));
        ]
  | Cst.Payload.Signature { item_syntax_nodes } ->
      Json.Object
        [
          ("tag", Json.String "signature");
          ("item_syntax_nodes", Json.Array (List.map syntax_node_to_json item_syntax_nodes));
        ]
  | Cst.Payload.Type type_ ->
      Json.Object
        [
          ("tag", Json.String "type");
          ("type", core_type_to_json type_);
        ]
  | Cst.Payload.Pattern payload ->
      Json.Object
        [
          ("tag", Json.String "pattern");
          ("payload", pattern_payload_to_json payload);
        ]

and extension_to_json (ext : Cst.extension) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json ext.syntax_node);
      ("sigil_token", token_to_json ext.sigil_token);
      ("name", ident_to_json ext.name);
      ("payload_syntax_node", option_to_json syntax_node_to_json ext.payload_syntax_node);
      ("payload", option_to_json payload_to_json ext.payload);
    ]

and object_member_to_json = function
  | Cst.ObjectMember.Method
      {
        syntax_node;
        attributes;
        name_token;
        body;
        type_;
        is_private;
        is_virtual;
        is_override;
      } ->
      Json.Object
        [
          ("tag", Json.String "method");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("attributes", Json.Array (List.map attribute_to_json attributes));
          ("name_token", token_to_json name_token);
          ("body", option_to_json expression_to_json body);
          ("type", option_to_json core_type_to_json type_);
          ("is_private", Json.Bool is_private);
          ("is_virtual", Json.Bool is_virtual);
          ("is_override", Json.Bool is_override);
        ]
  | Cst.ObjectMember.Value
      {
        syntax_node;
        attributes;
        name_token;
        value;
        type_;
        is_mutable;
        is_virtual;
        is_override;
      } ->
      Json.Object
        [
          ("tag", Json.String "value");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("attributes", Json.Array (List.map attribute_to_json attributes));
          ("name_token", token_to_json name_token);
          ("value", option_to_json expression_to_json value);
          ("type", option_to_json core_type_to_json type_);
          ("is_mutable", Json.Bool is_mutable);
          ("is_virtual", Json.Bool is_virtual);
          ("is_override", Json.Bool is_override);
        ]
  | Cst.ObjectMember.Inherit { syntax_node; attributes; expression } ->
      Json.Object
        [
          ("tag", Json.String "inherit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("attributes", Json.Array (List.map attribute_to_json attributes));
          ("expression", expression_to_json expression);
        ]
  | Cst.ObjectMember.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]
  | Cst.ObjectMember.Initializer { syntax_node; body } ->
      Json.Object
        [
          ("tag", Json.String "initializer");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("body", option_to_json expression_to_json body);
        ]

and binding_operator_binding_to_json
    ({ keyword_token; operator_token; binding_pattern; bound_value } :
      Cst.binding_operator_binding) =
  Json.Object
    [
      ("keyword_token", token_to_json keyword_token);
      ("operator_token", token_to_json operator_token);
      ("binding_pattern", pattern_to_json binding_pattern);
      ("bound_value", expression_to_json bound_value);
    ]

and for_direction_to_json = function
  | Cst.To { direction_token } ->
      Json.Object
        [
          ("tag", Json.String "to");
          ("token", token_to_json direction_token);
        ]
  | Cst.Downto { direction_token } ->
      Json.Object
        [
          ("tag", Json.String "downto");
          ("token", token_to_json direction_token);
        ]

and expression_to_json = function
  | Cst.Expression.Path { syntax_node; path } ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("path", ident_to_json path);
        ]
  | Cst.Expression.Operator { syntax_node; operator_tokens } ->
      Json.Object
        [
          ("tag", Json.String "operator");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("operator_tokens", Json.Array (List.map token_to_json operator_tokens));
        ]
  | Cst.Expression.Literal literal ->
      Json.Object
        [
          ("tag", Json.String "literal");
          ("literal", literal_to_json literal);
        ]
  | Cst.Expression.Unreachable { syntax_node; dot_token } ->
      Json.Object
        [
          ("tag", Json.String "unreachable");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("dot_token", token_to_json dot_token);
        ]
  | Cst.Expression.Attribute attr ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("attribute", attribute_to_json attr);
        ]
  | Cst.Expression.Extension ext ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json ext);
        ]
  | Cst.Expression.Object { syntax_node; self_pattern; members } ->
      Json.Object
        [
          ("tag", Json.String "object");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("self_pattern", option_to_json pattern_to_json self_pattern);
          ("members", Json.Array (List.map object_member_to_json members));
        ]
  | Cst.Expression.PolyVariant { syntax_node; tag_token; payload } ->
      Json.Object
        [
          ("tag", Json.String "poly_variant");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("tag_token", token_to_json tag_token);
          ("payload", option_to_json expression_to_json payload);
        ]
  | Cst.Expression.FirstClassModule { syntax_node; module_expression; module_type } ->
      Json.Object
        [
          ("tag", Json.String "first_class_module");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_expression", module_expression_to_json module_expression);
          ("module_type", option_to_json module_type_to_json module_type);
        ]
  | Cst.Expression.LetModule
      { syntax_node; module_name_token; module_expression; body } ->
      Json.Object
        [
          ("tag", Json.String "let_module");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_name_token", token_to_json module_name_token);
          ("module_expression", module_expression_to_json module_expression);
          ("body", expression_to_json body);
        ]
  | Cst.Expression.LetException
      { syntax_node; exception_declaration; body } ->
      Json.Object
        [
          ("tag", Json.String "let_exception");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("exception_declaration", exception_declaration_to_json exception_declaration);
          ("body", expression_to_json body);
        ]
  | Cst.Expression.Assert { syntax_node; asserted } ->
      Json.Object
        [
          ("tag", Json.String "assert");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("asserted", expression_to_json asserted);
        ]
  | Cst.Expression.Lazy { syntax_node; body } ->
      Json.Object
        [
          ("tag", Json.String "lazy");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("body", expression_to_json body);
        ]
  | Cst.Expression.While { syntax_node; condition; body } ->
      Json.Object
        [
          ("tag", Json.String "while");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("condition", expression_to_json condition);
          ("body", expression_to_json body);
        ]
  | Cst.Expression.For
      { syntax_node; iterator_token; start_expr; direction; end_expr; body } ->
      Json.Object
        [
          ("tag", Json.String "for");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("iterator_token", token_to_json iterator_token);
          ("start_expr", expression_to_json start_expr);
          ("direction", for_direction_to_json direction);
          ("end_expr", expression_to_json end_expr);
          ("body", expression_to_json body);
        ]
  | Cst.Expression.Apply { syntax_node; callee; argument } ->
      Json.Object
        [
          ("tag", Json.String "apply");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("callee", expression_to_json callee);
          ("argument", apply_argument_to_json argument);
        ]
  | Cst.Expression.MethodCall { syntax_node; receiver; method_name } ->
      Json.Object
        [
          ("tag", Json.String "method_call");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("receiver", expression_to_json receiver);
          ("method_name", token_to_json method_name);
        ]
  | Cst.Expression.New { syntax_node; class_path } ->
      Json.Object
        [
          ("tag", Json.String "new");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_path", ident_to_json class_path);
        ]
  | Cst.Expression.Prefix { syntax_node; operator_token; operand } ->
      Json.Object
        [
          ("tag", Json.String "prefix");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("operator_token", token_to_json operator_token);
          ("operand", expression_to_json operand);
        ]
  | Cst.Expression.FieldAccess { syntax_node; receiver; field_name } ->
      Json.Object
        [
          ("tag", Json.String "field_access");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("receiver", expression_to_json receiver);
          ("field_name", token_to_json field_name);
        ]
  | Cst.Expression.Index { syntax_node; collection; index } ->
      Json.Object
        [
          ("tag", Json.String "index");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("collection", expression_to_json collection);
          ("index", expression_to_json index);
        ]
  | Cst.Expression.ObjectUpdate { syntax_node; fields } ->
      Json.Object
        [
          ("tag", Json.String "object_update");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map record_expression_field_to_json fields));
        ]
  | Cst.Expression.InstanceVariableAssign
      { syntax_node; name_token; operator_token; value } ->
      Json.Object
        [
          ("tag", Json.String "instance_variable_assign");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("operator_token", token_to_json operator_token);
          ("value", expression_to_json value);
        ]
  | Cst.Expression.FieldAssign
      { syntax_node; target; operator_token; value } ->
      Json.Object
        [
          ("tag", Json.String "field_assign");
          ("syntax_node", syntax_node_to_json syntax_node);
          ( "target",
            expression_to_json (Cst.Expression.FieldAccess target) );
          ("operator_token", token_to_json operator_token);
          ("value", expression_to_json value);
        ]
  | Cst.Expression.Assign { syntax_node; target; operator_token; value } ->
      Json.Object
        [
          ("tag", Json.String "assign");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("target", expression_to_json target);
          ("operator_token", token_to_json operator_token);
          ("value", expression_to_json value);
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
  | Cst.Expression.Typed { syntax_node; expression; type_ } ->
      Json.Object
        [
          ("tag", Json.String "typed");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("expression", expression_to_json expression);
          ("type", core_type_to_json type_);
        ]
  | Cst.Expression.Polymorphic { syntax_node; expression; type_ } ->
      Json.Object
        [
          ("tag", Json.String "polymorphic");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("expression", expression_to_json expression);
          ("type", core_type_to_json type_);
        ]
  | Cst.Expression.Coerce
      { syntax_node; expression; from_type; to_type } ->
      Json.Object
        [
          ("tag", Json.String "coerce");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("expression", expression_to_json expression);
          ("from_type", option_to_json core_type_to_json from_type);
          ("to_type", core_type_to_json to_type);
        ]
  | Cst.Expression.Sequence { syntax_node; left; right } ->
      Json.Object
        [
          ("tag", Json.String "sequence");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("left", expression_to_json left);
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
  | Cst.Expression.Record (Cst.RecordExpression.Literal { syntax_node; fields }) ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("shape", Json.String "literal");
          ("fields", Json.Array (List.map record_expression_field_to_json fields));
        ]
  | Cst.Expression.Record
      (Cst.RecordExpression.Update { syntax_node; base; fields }) ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("shape", Json.String "update");
          ("base", expression_to_json base);
          ("fields", Json.Array (List.map record_expression_field_to_json fields));
        ]
  | Cst.Expression.LocalOpen { syntax_node; module_path; body; via_let_open } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
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
  | Cst.Expression.LetOperator { syntax_node; binding; and_bindings; body } ->
      Json.Object
        [
          ("tag", Json.String "let_operator");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binding", binding_operator_binding_to_json binding);
          ("and_bindings", Json.Array (List.map binding_operator_binding_to_json and_bindings));
          ("body", expression_to_json body);
        ]
  | Cst.Expression.Let
      { syntax_node; binding_pattern; bound_value; and_bindings; body; is_recursive } ->
      Json.Object
        [
          ("tag", Json.String "let");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binding_pattern", pattern_to_json binding_pattern);
          ("bound_value", expression_to_json bound_value);
          ("and_bindings", Json.Array (List.map let_binding_to_json and_bindings));
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

and apply_argument_to_json = function
  | Cst.Positional expr ->
      Json.Object
        [
          ("tag", Json.String "positional");
          ("value", expression_to_json expr);
        ]
  | Cst.Labeled { syntax_node; label_token; value } ->
      Json.Object
        [
          ("tag", Json.String "labeled");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label_token", token_to_json label_token);
          ("value", option_to_json expression_to_json value);
        ]
  | Cst.Optional { syntax_node; label_token; value } ->
      Json.Object
        [
          ("tag", Json.String "optional");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label_token", token_to_json label_token);
          ("value", option_to_json expression_to_json value);
        ]

and record_expression_field_to_json field =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json field.syntax_node);
      ("field_path", ident_to_json field.field_path);
      ("value", option_to_json expression_to_json field.value);
    ]

and match_case_to_json { syntax_node; pattern; guard; body } =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("pattern", pattern_to_json pattern);
      ("guard", option_to_json expression_to_json guard);
      ("body", expression_to_json body);
    ]

and let_binding_to_json binding =
    Json.Object
      [
        ("syntax_node", syntax_node_to_json (Cst.LetBinding.syntax_node binding));
        ("attributes", Json.Array (List.map attribute_to_json (Cst.LetBinding.attributes binding)));
        ("binding_pattern", pattern_to_json (Cst.LetBinding.binding_pattern binding));
        ( "binding_name",
          option_to_json token_to_json (Cst.LetBinding.binding_name_token binding) );
        ( "parameters",
          Json.Array (List.map parameter_to_json (Cst.LetBinding.parameters binding))
        );
        ("value", expression_to_json (Cst.LetBinding.value binding));
        ("is_recursive", Json.Bool (Cst.LetBinding.is_recursive binding));
      ]

let type_variable_to_json type_variable =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeVariable.syntax_node type_variable));
      ("name_token", token_to_json (Cst.TypeVariable.name_token type_variable));
    ]

let type_parameter_variance_to_json = function
  | Cst.TypeParameterVariance.Covariant { marker_token } ->
      Json.Object
        [
          ("tag", Json.String "covariant");
          ("marker_token", token_to_json marker_token);
        ]
  | Cst.TypeParameterVariance.Contravariant { marker_token } ->
      Json.Object
        [
          ("tag", Json.String "contravariant");
          ("marker_token", token_to_json marker_token);
        ]

let type_parameter_to_json type_parameter =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeParameter.syntax_node type_parameter));
      ( "variance",
        option_to_json type_parameter_variance_to_json
          (Cst.TypeParameter.variance type_parameter) );
      ("is_injective", Json.Bool (Cst.TypeParameter.is_injective type_parameter));
      ( "type_variable",
        option_to_json type_variable_to_json
          (Cst.TypeParameter.type_variable type_parameter) );
    ]

let record_field_to_json field =
  let attributes =
    Cst.RecordField.attributes field |> List.map attribute_to_json
  in
  Json.Object
    ([
       ("syntax_node", syntax_node_to_json (Cst.RecordField.syntax_node field));
       ("field_name", token_to_json (Cst.RecordField.field_name_token field));
       ("field_type", core_type_to_json (Cst.RecordField.field_type field));
       ("is_mutable", Json.Bool (Cst.RecordField.is_mutable field));
     ]
    @ if attributes = [] then [] else [ ("attributes", Json.Array attributes) ])

let constructor_arguments_to_json = function
  | Cst.ConstructorArguments.Tuple elements ->
      Json.Object
        [
          ("tag", Json.String "tuple");
          ("elements", Json.Array (List.map core_type_to_json elements));
        ]
  | Cst.ConstructorArguments.Record fields ->
      Json.Object
        [
          ("tag", Json.String "record");
          ("fields", Json.Array (List.map record_field_to_json fields));
        ]

let variant_constructor_to_json constr =
  let arguments =
    Cst.VariantConstructor.arguments constr
    |> Option.map constructor_arguments_to_json
  in
  let result_type =
    Cst.VariantConstructor.result_type constr
    |> Option.map core_type_to_json
  in
  Json.Object
    ([
       ("syntax_node", syntax_node_to_json (Cst.VariantConstructor.syntax_node constr));
       ( "constructor_name",
         token_to_json (Cst.VariantConstructor.constructor_name_token constr) );
     ]
    @
    (match arguments with
    | Some arguments -> [ ("arguments", arguments) ]
    | None -> [])
    @
    [ ( "payload_type",
        option_to_json core_type_to_json (Cst.VariantConstructor.payload_type constr) );
    ]
    @
    (match result_type with
    | Some result_type -> [ ("result_type", result_type) ]
    | None -> []))

let type_constraint_to_json ({ syntax_node; left; right } : Cst.type_constraint) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("left", core_type_to_json left);
      ("right", core_type_to_json right);
    ]

let private_flag_to_json = function
  | Cst.PrivateFlag.Public ->
      Json.Object [ ("tag", Json.String "public") ]
  | Cst.PrivateFlag.Private { private_token } ->
      Json.Object
        [
          ("tag", Json.String "private");
          ("private_token", token_to_json private_token);
        ]

let type_definition_to_json = function
  | Cst.TypeDefinition.Abstract ->
      Json.Object [ ("tag", Json.String "abstract") ]
  | Cst.TypeDefinition.Alias { syntax_node; manifest } ->
      Json.Object
        [
          ("tag", Json.String "alias");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("manifest", core_type_to_json manifest);
        ]
  | Cst.TypeDefinition.Extensible { syntax_node } ->
      Json.Object
        [
          ("tag", Json.String "extensible");
          ("syntax_node", syntax_node_to_json syntax_node);
        ]
  | Cst.TypeDefinition.FirstClassModule { syntax_node; module_type } ->
      Json.Object
        [
          ("tag", Json.String "first_class_module");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_type", module_type_to_json module_type);
        ]
  | Cst.TypeDefinition.Object { syntax_node; fields } ->
      Json.Object
        [
          ("tag", Json.String "object");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map object_type_field_to_json fields));
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
  | Cst.TypeDefinition.PolyVariant poly_variant ->
      Json.Object
        [
          ("tag", Json.String "poly_variant");
          ("syntax_node", syntax_node_to_json (Cst.PolyVariant.syntax_node poly_variant));
          ("kind", poly_variant_bound_to_json (Cst.PolyVariant.kind poly_variant));
          ("fields", Json.Array (List.map row_field_to_json (Cst.PolyVariant.fields poly_variant)));
        ]

let type_declaration_to_json decl =
  let constraints =
    Cst.TypeDeclaration.constraints decl
    |> List.map type_constraint_to_json
  in
  Json.Object
    ([
       ("syntax_node", syntax_node_to_json (Cst.TypeDeclaration.syntax_node decl));
       ("type_name", ident_to_json (Cst.TypeDeclaration.type_name decl));
       ( "type_params",
         Json.Array
           (List.map type_parameter_to_json (Cst.TypeDeclaration.type_params decl))
       );
       ( "type_definition",
         type_definition_to_json (Cst.TypeDeclaration.type_definition decl) );
     ]
    @
    (match Cst.TypeDeclaration.private_flag decl with
    | Cst.PrivateFlag.Public ->
        []
    | private_flag ->
        [ ("private_flag", private_flag_to_json private_flag) ])
    @
    (if constraints = [] then []
     else [ ("constraints", Json.Array constraints) ])
    @
    [ ( "is_destructive_substitution",
        Json.Bool (Cst.TypeDeclaration.is_destructive_substitution decl) );
    ])

let type_extension_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.TypeExtension.syntax_node decl));
      ("type_name", ident_to_json (Cst.TypeExtension.type_name decl));
      ( "type_params",
        Json.Array
          (List.map type_parameter_to_json (Cst.TypeExtension.type_params decl))
      );
      ( "constructors",
        Json.Array
          (List.map variant_constructor_to_json
             (Cst.TypeExtension.constructors decl)) );
    ]

let module_declaration_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.ModuleDeclaration.syntax_node decl));
      ("module_name", token_to_json (Cst.ModuleDeclaration.module_name_token decl));
      ( "functor_parameters",
        Json.Array
          (List.map functor_parameter_to_json
             (Cst.ModuleDeclaration.functor_parameters decl)) );
      ( "module_type",
        option_to_json module_type_to_json (Cst.ModuleDeclaration.module_type decl) );
      ( "module_expression",
        option_to_json module_expression_to_json
          (Cst.ModuleDeclaration.module_expression decl) );
      ( "is_destructive_substitution",
        Json.Bool (Cst.ModuleDeclaration.is_destructive_substitution decl) );
      ("is_recursive", Json.Bool (Cst.ModuleDeclaration.is_recursive decl));
    ]

let recursive_module_declaration_to_json decl =
  Json.Object
    [
      ( "syntax_node",
        syntax_node_to_json (Cst.RecursiveModuleDeclaration.syntax_node decl) );
      ( "declarations",
        Json.Array
          (List.map module_declaration_to_json
             (Cst.RecursiveModuleDeclaration.declarations decl)) );
    ]

let module_type_declaration_to_json decl =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.ModuleTypeDeclaration.syntax_node decl));
      ( "module_type_name",
        token_to_json (Cst.ModuleTypeDeclaration.module_type_name_token decl) );
      ( "module_type",
        option_to_json module_type_to_json (Cst.ModuleTypeDeclaration.module_type decl) );
      ( "is_destructive_substitution",
        Json.Bool (Cst.ModuleTypeDeclaration.is_destructive_substitution decl) );
    ]

let open_statement_to_json stmt =
  let open_target_to_json = function
    | Cst.OpenStatement.Path path ->
        Json.Object
          [ ("tag", Json.String "path"); ("value", ident_to_json path) ]
    | Cst.OpenStatement.ModuleExpression module_expression ->
        Json.Object
          [
            ("tag", Json.String "module_expression");
            ("value", module_expression_to_json module_expression);
          ]
  in
  Json.Object
    [
      ("syntax_node", syntax_node_to_json (Cst.OpenStatement.syntax_node stmt));
      ("target", open_target_to_json (Cst.OpenStatement.target stmt));
      ( "module_path",
        option_to_json ident_to_json (Cst.OpenStatement.module_path stmt) );
      ("bang_token", option_to_json token_to_json (Cst.OpenStatement.bang_token stmt));
    ]

let value_declaration_to_json (decl : Cst.value_declaration) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json decl.syntax_node);
      ("name_token", token_to_json decl.name_token);
      ("type", core_type_to_json decl.type_);
    ]

let external_declaration_to_json (decl : Cst.external_declaration) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json decl.syntax_node);
      ("name_token", token_to_json decl.name_token);
      ("type", core_type_to_json decl.type_);
      ("primitive_name_tokens", Json.Array (List.map token_to_json decl.primitive_name_tokens));
    ]

let rec class_field_to_json = function
  | Cst.ClassField.Method
      { syntax_node; name_token; body; type_; is_private; is_virtual; is_override } ->
      Json.Object
        [
          ("tag", Json.String "method");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("body", option_to_json expression_to_json body);
          ("type", option_to_json core_type_to_json type_);
          ("is_private", Json.Bool is_private);
          ("is_virtual", Json.Bool is_virtual);
          ("is_override", Json.Bool is_override);
        ]
  | Cst.ClassField.Value
      { syntax_node; name_token; value; type_; is_mutable; is_virtual; is_override } ->
      Json.Object
        [
          ("tag", Json.String "value");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("value", option_to_json expression_to_json value);
          ("type", option_to_json core_type_to_json type_);
          ("is_mutable", Json.Bool is_mutable);
          ("is_virtual", Json.Bool is_virtual);
          ("is_override", Json.Bool is_override);
        ]
  | Cst.ClassField.Inherit { syntax_node; class_expression } ->
      Json.Object
        [
          ("tag", Json.String "inherit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_expression", class_expression_to_json class_expression);
        ]
  | Cst.ClassField.Constraint { syntax_node; left; right } ->
      Json.Object
        [
          ("tag", Json.String "constraint");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("left", core_type_to_json left);
          ("right", core_type_to_json right);
        ]
  | Cst.ClassField.Initializer { syntax_node; body } ->
      Json.Object
        [
          ("tag", Json.String "initializer");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("body", option_to_json expression_to_json body);
        ]
  | Cst.ClassField.Attribute { syntax_node; field; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("field", class_field_to_json field);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ClassField.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

and class_expression_to_json = function
  | Cst.ClassExpression.Path path ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("path", ident_to_json path);
        ]
  | Cst.ClassExpression.Structure { syntax_node; self_pattern; fields } ->
      Json.Object
        [
          ("tag", Json.String "structure");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("self_pattern", option_to_json pattern_to_json self_pattern);
          ("fields", Json.Array (List.map class_field_to_json fields));
        ]
  | Cst.ClassExpression.Fun { syntax_node; parameters; body } ->
      Json.Object
        [
          ("tag", Json.String "fun");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("parameters", Json.Array (List.map parameter_to_json parameters));
          ("body", class_expression_to_json body);
        ]
  | Cst.ClassExpression.Apply { syntax_node; callee; argument } ->
      Json.Object
        [
          ("tag", Json.String "apply");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("callee", class_expression_to_json callee);
          ("argument", apply_argument_to_json argument);
        ]
  | Cst.ClassExpression.Let
      { syntax_node; binding_pattern; bound_value; and_bindings; body; is_recursive } ->
      Json.Object
        [
          ("tag", Json.String "let");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("binding_pattern", pattern_to_json binding_pattern);
          ("bound_value", expression_to_json bound_value);
          ("and_bindings", Json.Array (List.map let_binding_to_json and_bindings));
          ("body", class_expression_to_json body);
          ("is_recursive", Json.Bool is_recursive);
        ]
  | Cst.ClassExpression.Constraint
      { syntax_node; class_expression; class_type } ->
      Json.Object
        [
          ("tag", Json.String "constraint");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_expression", class_expression_to_json class_expression);
          ("class_type", class_type_to_json class_type);
        ]
  | Cst.ClassExpression.LocalOpen
      { syntax_node; module_path; class_expression; via_let_open } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
          ("class_expression", class_expression_to_json class_expression);
          ("via_let_open", Json.Bool via_let_open);
        ]
  | Cst.ClassExpression.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", class_expression_to_json inner);
        ]
  | Cst.ClassExpression.Attribute
      { syntax_node; class_expression; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_expression", class_expression_to_json class_expression);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ClassExpression.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

and class_type_field_to_json = function
  | Cst.ClassTypeField.Inherit { syntax_node; class_type } ->
      Json.Object
        [
          ("tag", Json.String "inherit");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_type", class_type_to_json class_type);
        ]
  | Cst.ClassTypeField.Value { syntax_node; name_token; type_; is_mutable } ->
      Json.Object
        [
          ("tag", Json.String "value");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("type", core_type_to_json type_);
          ("is_mutable", Json.Bool is_mutable);
        ]
  | Cst.ClassTypeField.Method { syntax_node; name_token; type_; is_private } ->
      Json.Object
        [
          ("tag", Json.String "method");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("name_token", token_to_json name_token);
          ("type", core_type_to_json type_);
          ("is_private", Json.Bool is_private);
        ]
  | Cst.ClassTypeField.Constraint { syntax_node; left; right } ->
      Json.Object
        [
          ("tag", Json.String "constraint");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("left", core_type_to_json left);
          ("right", core_type_to_json right);
        ]
  | Cst.ClassTypeField.Attribute { syntax_node; field; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("field", class_type_field_to_json field);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ClassTypeField.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

and class_type_to_json = function
  | Cst.ClassType.Path path ->
      Json.Object
        [
          ("tag", Json.String "path");
          ("path", ident_to_json path);
        ]
  | Cst.ClassType.Signature { syntax_node; fields } ->
      Json.Object
        [
          ("tag", Json.String "signature");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("fields", Json.Array (List.map class_type_field_to_json fields));
        ]
  | Cst.ClassType.Arrow { syntax_node; label; parameter_type; result_type } ->
      Json.Object
        [
          ("tag", Json.String "arrow");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("label", option_to_json arrow_label_to_json label);
          ("parameter_type", core_type_to_json parameter_type);
          ("result_type", class_type_to_json result_type);
        ]
  | Cst.ClassType.Parenthesized { syntax_node; inner } ->
      Json.Object
        [
          ("tag", Json.String "parenthesized");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("inner", class_type_to_json inner);
        ]
  | Cst.ClassType.LocalOpen { syntax_node; module_path; class_type } ->
      Json.Object
        [
          ("tag", Json.String "local_open");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("module_path", ident_to_json module_path);
          ("class_type", class_type_to_json class_type);
        ]
  | Cst.ClassType.Attribute { syntax_node; class_type; attribute } ->
      Json.Object
        [
          ("tag", Json.String "attribute");
          ("syntax_node", syntax_node_to_json syntax_node);
          ("class_type", class_type_to_json class_type);
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.ClassType.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension");
          ("extension", extension_to_json extension);
        ]

let class_declaration_to_json
    ({ syntax_node; type_params; class_name; class_type; class_body } :
      Cst.class_declaration) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("type_params", Json.Array (List.map type_parameter_to_json type_params));
      ("class_name", token_to_json class_name);
      ("class_type", option_to_json class_type_to_json class_type);
      ("class_body", option_to_json class_expression_to_json class_body);
    ]

let class_type_declaration_to_json
    ({ syntax_node; type_params; class_type_name; class_type_body } :
      Cst.class_type_declaration) =
  Json.Object
    [
      ("syntax_node", syntax_node_to_json syntax_node);
      ("type_params", Json.Array (List.map type_parameter_to_json type_params));
      ("class_type_name", token_to_json class_type_name);
      ("class_type_body", class_type_to_json class_type_body);
    ]

let include_statement_to_json (stmt : Cst.include_statement) =
  let target =
    match stmt.target with
    | Cst.ModuleExpression module_expression ->
        Json.Object
          [
            ("tag", Json.String "module_expression");
            ("value", module_expression_to_json module_expression);
          ]
    | Cst.ModuleType module_type ->
        Json.Object
          [
            ("tag", Json.String "module_type");
            ("value", module_type_to_json module_type);
          ]
  in
  Json.Object
    [
      ("syntax_node", syntax_node_to_json stmt.syntax_node);
      ("target", target);
    ]

let structure_item_to_json = function
  | Cst.StructureItem.TypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "type_declaration");
          ("item", type_declaration_to_json decl);
        ]
  | Cst.StructureItem.TypeExtension decl ->
      Json.Object
        [
          ("tag", Json.String "type_extension");
          ("item", type_extension_to_json decl);
        ]
  | Cst.StructureItem.LetBinding binding ->
      Json.Object
        [
          ("tag", Json.String "let_binding");
          ("item", let_binding_to_json binding);
        ]
  | Cst.StructureItem.Expression expr ->
      Json.Object
        [
          ("tag", Json.String "expression");
          ("item", expression_to_json expr);
        ]
  | Cst.StructureItem.ClassDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "class_declaration");
          ("item", class_declaration_to_json decl);
        ]
  | Cst.StructureItem.Attribute attribute ->
      Json.Object
        [
          ("tag", Json.String "attribute_item");
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.StructureItem.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension_item");
          ("extension", extension_to_json extension);
        ]
  | Cst.StructureItem.ClassTypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "class_type_declaration");
          ("item", class_type_declaration_to_json decl);
        ]
  | Cst.StructureItem.ModuleDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_declaration");
          ("item", module_declaration_to_json decl);
        ]
  | Cst.StructureItem.RecursiveModuleDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "recursive_module_declaration");
          ("item", recursive_module_declaration_to_json decl);
        ]
  | Cst.StructureItem.ModuleTypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_type_declaration");
          ("item", module_type_declaration_to_json decl);
        ]
  | Cst.StructureItem.OpenStatement stmt ->
      Json.Object
        [
          ("tag", Json.String "open_statement");
          ("item", open_statement_to_json stmt);
        ]
  | Cst.StructureItem.ValueDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "value_declaration");
          ("item", value_declaration_to_json decl);
        ]
  | Cst.StructureItem.ExternalDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "external_declaration");
          ("item", external_declaration_to_json decl);
        ]
  | Cst.StructureItem.IncludeStatement stmt ->
      Json.Object
        [
          ("tag", Json.String "include_statement");
          ("item", include_statement_to_json stmt);
        ]
  | Cst.StructureItem.ExceptionDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "exception_declaration");
          ("item", exception_declaration_to_json decl);
        ]

let signature_item_to_json = function
  | Cst.SignatureItem.TypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "type_declaration");
          ("item", type_declaration_to_json decl);
        ]
  | Cst.SignatureItem.TypeExtension decl ->
      Json.Object
        [
          ("tag", Json.String "type_extension");
          ("item", type_extension_to_json decl);
        ]
  | Cst.SignatureItem.Attribute attribute ->
      Json.Object
        [
          ("tag", Json.String "attribute_item");
          ("attribute", attribute_to_json attribute);
        ]
  | Cst.SignatureItem.Extension extension ->
      Json.Object
        [
          ("tag", Json.String "extension_item");
          ("extension", extension_to_json extension);
        ]
  | Cst.SignatureItem.ClassDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "class_declaration");
          ("item", class_declaration_to_json decl);
        ]
  | Cst.SignatureItem.ClassTypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "class_type_declaration");
          ("item", class_type_declaration_to_json decl);
        ]
  | Cst.SignatureItem.ModuleDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_declaration");
          ("item", module_declaration_to_json decl);
        ]
  | Cst.SignatureItem.RecursiveModuleDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "recursive_module_declaration");
          ("item", recursive_module_declaration_to_json decl);
        ]
  | Cst.SignatureItem.ModuleTypeDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "module_type_declaration");
          ("item", module_type_declaration_to_json decl);
        ]
  | Cst.SignatureItem.OpenStatement stmt ->
      Json.Object
        [
          ("tag", Json.String "open_statement");
          ("item", open_statement_to_json stmt);
        ]
  | Cst.SignatureItem.ValueDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "value_declaration");
          ("item", value_declaration_to_json decl);
        ]
  | Cst.SignatureItem.IncludeStatement stmt ->
      Json.Object
        [
          ("tag", Json.String "include_statement");
          ("item", include_statement_to_json stmt);
        ]
  | Cst.SignatureItem.ExceptionDeclaration decl ->
      Json.Object
        [
          ("tag", Json.String "exception_declaration");
          ("item", exception_declaration_to_json decl);
        ]

let of_source_file source_file =
  let items =
    match source_file with
    | Cst.Implementation { items; _ } ->
        List.map structure_item_to_json items
    | Cst.Interface { items; _ } ->
        List.map signature_item_to_json items
  in
  Json.Object
    [
      ( "kind",
        Json.String
          (match Cst.SourceFile.kind source_file with
          | `Implementation -> "implementation"
          | `Interface -> "interface") );
      ("syntax_node", syntax_node_to_json (Cst.SourceFile.syntax_node source_file));
      ("items", Json.Array items);
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
