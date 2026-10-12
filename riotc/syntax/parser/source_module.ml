// Parse module imports and top-level declarations into the shared RiotML AST.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Syntax.Ast.{
  SourceModule, Import, ModulePath, ModuleName, ImportedName, MemberName,
  Declaration, ValueDeclaration, ValueDeclarationData, TypeDeclaration,
  TypeDeclarationData, TypeName, TypeVariableName, TypeDefinition, TypeAlias, TypeRecord,
  TypeVariants, RecordTypeField, VariantDeclaration, TupleVariant,
  TupleVariantData, RecordVariant, RecordVariantData, TypeExpr, ValueName, Pattern,
  ConstructorName,
  Function, FunctionExpression, Ascription, AscriptionExpression,
  make_expression, expression_with_span, expression_span, pattern_span
}
use Syntax.Lexer.{
  Token, TokenInfo, IdentifierToken, SymbolToken, UnterminatedString,
  UnterminatedEscape, lex, SourceSpan
}
use Syntax.Parser.Common.{
  ParseStep, reverse_list, is_constructor_name, parser_error_from_lexer,
  parse_error_message
}
use Syntax.Parser.Types.{parse_type, parse_type_list}
use Syntax.Parser.Patterns.{parse_pattern}
use Syntax.Parser.Expressions.{parse_expression}

type ModuleParseState = ModuleParseState {
  reversed_imports: List<Import>,
  reversed_declarations: List<Declaration>
}

type RecordFieldsState = RecordFieldsState {
  reversed_fields: List<RecordTypeField>,
  remaining_tokens: List<Token>
}

type VariantListState = VariantListState {
  reversed_variants: List<VariantDeclaration>,
  remaining_tokens: List<Token>
}

type ValueParametersState = ValueParametersState {
  reversed_parameters: List<Pattern>,
  remaining_tokens: List<Token>
}

let parse_selected_imports = fn reversed_path -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) ->
      Err("an import must select at least one member")
    | _ -> parse_selected_import_names reversed_path Nil tokens
  }

let parse_selected_import_names = fn reversed_path -> fn reversed_names -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      continue_selected_imports reversed_path
        (Cons(ImportedName(MemberName(name)), reversed_names)) remaining
    | _ -> Err("expected a selected member name in import")
  }

let continue_selected_imports = fn reversed_path -> fn reversed_names -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      parse_selected_import_names reversed_path reversed_names remaining
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) -> Ok(ParseStep {
      value: Import {
        module_path: ModulePath(reverse_list reversed_path),
        selected_names: reverse_list reversed_names
      },
      remaining_tokens: remaining
    })
    | _ -> Err("expected `,` or `}` after imported member")
  }

let parse_import_path = fn reversed_path -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ".", span: _ }), Cons(IdentifierToken(TokenInfo { lexeme: component, span: _ }), remaining)) ->
      parse_import_path (Cons(ModuleName(component), reversed_path)) remaining
    | Cons(SymbolToken(TokenInfo { lexeme: ".", span: _ }), Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining)) ->
      parse_selected_imports reversed_path remaining
    | _ -> Err("expected `.{selected, names}` after module path")
  }

let parse_import = fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: first_component, span: _ }), remaining) ->
      parse_import_path (Cons(ModuleName(first_component), Nil)) remaining
    | _ -> Err("expected a module path after `use`")
  }

let parse_record_fields_from_state = fn (RecordFieldsState {
  reversed_fields,
  remaining_tokens
}) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) -> Ok(ParseStep {
      value: reverse_list reversed_fields,
      remaining_tokens: remaining
    })
    | Cons(IdentifierToken(TokenInfo { lexeme: field_name, span: _ }), rest) ->
      parse_record_field_type reversed_fields (ValueName(field_name)) rest
    | _ -> Err("expected a record field name")
  }

let parse_record_field_type = fn reversed_fields -> fn field_name -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span: _ }), type_tokens) ->
      match (parse_type type_tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: field_type, remaining_tokens }) ->
          continue_record_fields (Cons(RecordTypeField {
            name: field_name,
            field_type: field_type
          }, reversed_fields)) remaining_tokens
      }
    | _ -> Err("expected `:` after record field name")
  }

let continue_record_fields = fn reversed_fields -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      parse_record_fields_from_state (RecordFieldsState {
        reversed_fields: reversed_fields,
        remaining_tokens: remaining
      })
    | Cons(SymbolToken(TokenInfo { lexeme: "}", span: _ }), remaining) -> Ok(ParseStep {
      value: reverse_list reversed_fields,
      remaining_tokens: remaining
    })
    | _ -> Err("expected `,` or `}` after record field type")
  }

let parse_record_fields = fn tokens ->
  parse_record_fields_from_state (RecordFieldsState {
    reversed_fields: Nil,
    remaining_tokens: tokens
  })

let parse_type_parameters = fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "<", span: _ }), remaining) ->
      parse_type_parameter_list Nil remaining
    | _ -> Ok(ParseStep { value: Nil, remaining_tokens: tokens })
  }

let parse_type_parameter_list = fn reversed_parameters -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ">", span: _ }), remaining) -> Ok(ParseStep {
      value: reverse_list reversed_parameters,
      remaining_tokens: remaining
    })
    | Cons(SymbolToken(TokenInfo { lexeme: "'", span: _ }), Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining)) ->
      continue_type_parameters (Cons(TypeVariableName(name), reversed_parameters)) remaining
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      continue_type_parameters (Cons(TypeVariableName(name), reversed_parameters)) remaining
    | _ -> Err("expected a type parameter or `>`")
  }

let continue_type_parameters = fn reversed_parameters -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ",", span: _ }), remaining) ->
      parse_type_parameter_list reversed_parameters remaining
    | Cons(SymbolToken(TokenInfo { lexeme: ">", span: _ }), remaining) -> Ok(ParseStep {
      value: reverse_list reversed_parameters,
      remaining_tokens: remaining
    })
    | _ -> Err("expected `,` or `>` after type parameter")
  }

let parse_variant = fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: constructor, span: _ }), remaining) ->
      parse_variant_payload (ConstructorName(constructor)) remaining
    | _ -> Err("expected a variant constructor name")
  }

let parse_variant_payload = fn constructor_name -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), remaining) ->
      match (parse_type_list parse_type remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep {
            value: fields,
            remaining_tokens: Cons(SymbolToken(TokenInfo { lexeme: ")", span: _ }), rest)
          }) -> Ok(ParseStep {
          value: TupleVariant(TupleVariantData {
            constructor_name: constructor_name,
            fields: fields
          }),
          remaining_tokens: rest
        })
        | _ -> Err("expected `)` after variant payload types")
      }
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      match (parse_record_fields remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: fields, remaining_tokens }) -> Ok(ParseStep {
          value: RecordVariant(RecordVariantData {
            constructor_name: constructor_name,
            fields: fields
          }),
          remaining_tokens: remaining_tokens
        })
      }
    | _ -> Ok(ParseStep {
      value: TupleVariant(TupleVariantData {
        constructor_name: constructor_name,
        fields: Nil
      }),
      remaining_tokens: tokens
    })
  }

let parse_variant_list = fn (VariantListState {
  reversed_variants,
  remaining_tokens
}) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), rest) ->
      parse_variant_list (VariantListState {
        reversed_variants: reversed_variants,
        remaining_tokens: rest
      })
    | _ -> match (parse_variant remaining_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: variant, remaining_tokens }) ->
        continue_variant_list (Cons(variant, reversed_variants)) remaining_tokens
    }
  }

let continue_variant_list = fn reversed_variants -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), remaining) ->
      parse_variant_list (VariantListState {
        reversed_variants: reversed_variants,
        remaining_tokens: remaining
      })
    | _ -> Ok(ParseStep {
      value: TypeVariants(reverse_list reversed_variants),
      remaining_tokens: tokens
    })
  }

let starts_variant_definition = fn tokens ->
  match tokens {
    | Cons(IdentifierToken(_), Cons(SymbolToken(TokenInfo { lexeme: "(", span: _ }), _)) -> true
    | Cons(IdentifierToken(_), Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), _)) -> true
    | Cons(IdentifierToken(_), Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), _)) -> true
    | _ -> false
  }

let parse_type_definition = fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "{", span: _ }), remaining) ->
      match (parse_record_fields remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: fields, remaining_tokens }) -> Ok(ParseStep {
          value: TypeRecord(fields),
          remaining_tokens: remaining_tokens
        })
      }
    | Cons(SymbolToken(TokenInfo { lexeme: "|", span: _ }), remaining) ->
      parse_variant_list (VariantListState {
        reversed_variants: Nil,
        remaining_tokens: remaining
      })
    | _ -> match (starts_variant_definition tokens) {
      | true -> parse_variant_list (VariantListState {
        reversed_variants: Nil,
        remaining_tokens: tokens
      })
      | false -> match (parse_type tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: alias, remaining_tokens }) -> Ok(ParseStep {
          value: TypeAlias(alias),
          remaining_tokens: remaining_tokens
        })
      }
    }
  }

let parse_type_declaration = fn declaration_start -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: name_span }), remaining) ->
      match (parse_type_parameters remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: parameters, remaining_tokens }) ->
          parse_type_declaration_equals (TypeName(name)) declaration_start
            name_span parameters remaining_tokens
      }
    | _ -> Err("expected a type name")
  }

let parse_type_declaration_equals = fn name -> fn declaration_start ->
  fn name_span -> fn parameters -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "=", span: _ }), definition_tokens) ->
      match (parse_type_definition definition_tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: definition, remaining_tokens }) -> {
          let SourceSpan { start_codepoint, end_codepoint: _ } = declaration_start;
          let SourceSpan { start_codepoint: _, end_codepoint } = name_span;
          Ok(ParseStep {
            value: TypeDeclaration(TypeDeclarationData {
              name: name,
              parameters: parameters,
              definition: definition,
              span: SourceSpan {
                start_codepoint: start_codepoint,
                end_codepoint: end_codepoint
              }
            }),
            remaining_tokens: remaining_tokens
          })
        }
      }
    | _ -> Err("expected `=` after type declaration")
  }

let parse_value_parameters = fn (ValueParametersState {
  reversed_parameters,
  remaining_tokens
}) ->
  match remaining_tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: "=", span: _ }), remaining) -> Ok(ParseStep {
      value: reverse_list reversed_parameters,
      remaining_tokens: remaining
    })
    | _ -> match (parse_pattern remaining_tokens) {
      | Err(message) -> Err(message)
      | Ok(ParseStep { value: parameter, remaining_tokens }) ->
        parse_value_parameters (ValueParametersState {
          reversed_parameters: Cons(parameter, reversed_parameters),
          remaining_tokens: remaining_tokens
        })
    }
  }

let wrap_value_parameters = fn parameters -> fn body ->
  match parameters {
    | Nil -> body
    | Cons(parameter, remaining) -> {
      let parameter_span = pattern_span parameter;
      let body_span = expression_span body;
      let SourceSpan { start_codepoint, end_codepoint: _ } = parameter_span;
      let SourceSpan { start_codepoint: _, end_codepoint } = body_span;
      expression_with_span (make_expression (Function(FunctionExpression {
        parameter: parameter,
        body: wrap_value_parameters remaining body
      }))) (SourceSpan {
        start_codepoint: start_codepoint,
        end_codepoint: end_codepoint
      })
    }
  }

let annotation_span_for_body = fn annotation_span -> fn body ->
  match annotation_span {
    | None -> expression_span body
    | Some(SourceSpan { start_codepoint, end_codepoint: _ }) -> {
      let SourceSpan { start_codepoint: _, end_codepoint } = expression_span body;
      SourceSpan { start_codepoint: start_codepoint, end_codepoint: end_codepoint }
    }
  }

let apply_value_annotation = fn annotation -> fn annotation_span -> fn body ->
  match annotation {
    | None -> body
    | Some(type_expression) -> expression_with_span
      (make_expression (Ascription(AscriptionExpression {
        expression: body,
        annotation: type_expression
      }))) (annotation_span_for_body annotation_span body)
  }

let parse_value_declaration = fn declaration_start -> fn tokens ->
  match tokens {
    | Cons(IdentifierToken(TokenInfo { lexeme: name, span: _ }), remaining) ->
      parse_value_declaration_annotation (ValueName(name)) declaration_start
        None None remaining
    | _ -> Err("expected a value name after `let`")
  }

let parse_value_declaration_annotation = fn name -> fn declaration_start ->
  fn annotation -> fn annotation_span -> fn tokens ->
  match tokens {
    | Cons(SymbolToken(TokenInfo { lexeme: ":", span }), remaining) ->
      match (parse_type remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: type_expression, remaining_tokens }) ->
          parse_value_declaration_parameters name declaration_start
            (Some(type_expression)) (Some(span)) remaining_tokens
      }
    | _ -> parse_value_declaration_parameters name declaration_start
      annotation annotation_span tokens
  }

let parse_value_declaration_parameters = fn name -> fn declaration_start ->
  fn annotation -> fn annotation_span -> fn tokens ->
  match (parse_value_parameters (ValueParametersState {
    reversed_parameters: Nil,
    remaining_tokens: tokens
  })) {
    | Err(message) -> Err(message)
    | Ok(ParseStep { value: parameters, remaining_tokens }) ->
      match (parse_expression remaining_tokens) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: body, remaining_tokens }) -> {
          let declared_body = apply_value_annotation annotation annotation_span
            (wrap_value_parameters parameters body);
          let SourceSpan { start_codepoint, end_codepoint: _ } = declaration_start;
          let SourceSpan { start_codepoint: _, end_codepoint } = expression_span declared_body;
          Ok(ParseStep {
            value: ValueDeclaration(ValueDeclarationData {
              name: name,
              body: declared_body,
              span: SourceSpan {
                start_codepoint: start_codepoint,
                end_codepoint: end_codepoint
              }
            }),
            remaining_tokens: remaining_tokens
          })
        }
      }
  }

let parse_module_items = fn (ModuleParseState {
  reversed_imports,
  reversed_declarations
}) -> fn tokens ->
  match tokens {
    | Nil -> Ok(SourceModule {
      imports: reverse_list reversed_imports,
      declarations: reverse_list reversed_declarations
    })
    | Cons(IdentifierToken(TokenInfo { lexeme: "use", span: _ }), remaining) ->
      match (parse_import remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: imported_module, remaining_tokens }) ->
          parse_module_items (ModuleParseState {
            reversed_imports: Cons(imported_module, reversed_imports),
            reversed_declarations: reversed_declarations
          }) remaining_tokens
      }
    | Cons(IdentifierToken(TokenInfo { lexeme: "type", span }), remaining) ->
      match (parse_type_declaration span remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: declaration, remaining_tokens }) ->
          parse_module_items (ModuleParseState {
            reversed_imports: reversed_imports,
            reversed_declarations: Cons(declaration, reversed_declarations)
          }) remaining_tokens
      }
    | Cons(IdentifierToken(TokenInfo { lexeme: "let", span }), remaining) ->
      match (parse_value_declaration span remaining) {
        | Err(message) -> Err(message)
        | Ok(ParseStep { value: declaration, remaining_tokens }) ->
          parse_module_items (ModuleParseState {
            reversed_imports: reversed_imports,
            reversed_declarations: Cons(declaration, reversed_declarations)
          }) remaining_tokens
      }
    | _ -> Err("expected a top-level `use`, `type`, or `let` declaration")
  }

let parse_source_module_tokens = fn tokens ->
  parse_module_items (ModuleParseState {
    reversed_imports: Nil,
    reversed_declarations: Nil
  }) tokens

let parse_source_module = fn source ->
  match (lex source) {
    | Err(error) -> Err(parse_error_message (parser_error_from_lexer error))
    | Ok(tokens) -> parse_source_module_tokens tokens
  }
