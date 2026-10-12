// Resolve value, type, and constructor names in a source module.
use Prelude.{Nil, Cons, Some, None, Ok, Err}
use String.{equal}
use Syntax.Ast.{
  SourceModule, Import, ImportedName, MemberName,
  ValueDeclaration, ValueDeclarationData, TypeDeclaration, TypeDeclarationData,
  TypeAlias, TypeRecord, TypeVariants, RecordTypeField,
  TupleVariant, TupleVariantData, RecordVariant,
  RecordVariantData, ValueName, TypeName, TypeVariableName, ConstructorName, Integer,
  Boolean, StringLiteral, Unit, VariableExpr, TupleExpr, RecordExpr,
  RecordExpression, RecordExpressionField, Binary, BinaryExpression, Negate,
  Function, FunctionExpression, Apply, ApplicationExpression, Let,
  LetExpression, Match, MatchExpression, MatchArm, ConstructorExpr,
  ConstructorExpression, Ascription, AscriptionExpression, Block,
  Variable, Wildcard, PatternUnit, PatternInteger, PatternBoolean,
  PatternString, TuplePattern, ConstructorPattern, ConstructorPatternData,
  RecordPattern, RecordPatternData, RecordPatternField, AscribedPattern,
  AscribedPatternData, NamedType, NamedTypeExpression, ArrowType,
  expression_kind, pattern_kind,
  ArrowTypeExpression, TupleType, RecordType, VariantType, TypeVariable
}

let append_names = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(name, remaining) -> Cons(name, append_names remaining second)
  }

let selected_import_names = fn imports ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path: _, selected_names }, remaining) ->
      append_names (selected_names_list selected_names) (selected_import_names remaining)
  }

let selected_names_list = fn names ->
  match names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      Cons(name, selected_names_list remaining)
  }

let declared_value_names = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(ValueDeclarationData { name: ValueName(name), body: _ }), remaining) ->
      Cons(name, declared_value_names remaining)
    | Cons(TypeDeclaration(_), remaining) -> declared_value_names remaining
  }

let declared_variant_names = fn variants ->
  match variants {
    | Nil -> Nil
    | Cons(TupleVariant(TupleVariantData {
        constructor_name: ConstructorName(name), fields: _
      }), remaining) -> Cons(name, declared_variant_names remaining)
    | Cons(RecordVariant(RecordVariantData {
        constructor_name: ConstructorName(name), fields: _
      }), remaining) -> Cons(name, declared_variant_names remaining)
  }

let declared_type_names = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(_), remaining) -> declared_type_names remaining
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: TypeAlias(_)
      }), remaining) -> Cons(name, declared_type_names remaining)
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: TypeRecord(_)
      }), remaining) -> Cons(name, declared_type_names remaining)
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: TypeVariants(variants)
      }), remaining) -> append_names (Cons(name, declared_variant_names variants))
        (declared_type_names remaining)
  }

let declared_type_declaration_names = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(_), remaining) -> declared_type_declaration_names remaining
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: _
      }), remaining) -> Cons(name, declared_type_declaration_names remaining)
  }

let declared_constructor_names = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(_), remaining) -> declared_constructor_names remaining
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: TypeAlias(_)
      }), remaining) -> declared_constructor_names remaining
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: TypeName(name), parameters: _, definition: TypeRecord(_)
      }), remaining) -> Cons(name, declared_constructor_names remaining)
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: _, parameters: _, definition: TypeVariants(variants)
      }), remaining) -> append_names (declared_variant_names variants)
        (declared_constructor_names remaining)
  }

let contains_name = fn wanted -> fn names ->
  match names {
    | Nil -> false
    | Cons(name, remaining) -> match (equal wanted name) {
      | true -> true
      | false -> contains_name wanted remaining
    }
  }

let find_duplicate_name = fn names ->
  match names {
    | Nil -> None
    | Cons(name, remaining) -> match (contains_name name remaining) {
      | true -> Some(name)
      | false -> find_duplicate_name remaining
    }
  }

let find_common_name = fn first -> fn second ->
  match first {
    | Nil -> None
    | Cons(name, remaining) -> match (contains_name name second) {
      | true -> Some(name)
      | false -> find_common_name remaining second
    }
  }

let validate_unique_names = fn names -> fn description ->
  match (find_duplicate_name names) {
    | None -> Ok(())
    | Some(name) -> Err(description + " `" + name + "`")
  }

let builtin_type_name = fn name ->
  match name {
    | "i32" -> true
    | "bool" -> true
    | "string" -> true
    | "unit" -> true
    | _ -> false
  }

let validate_type_name = fn name -> fn visible_names ->
  match (builtin_type_name name) {
    | true -> Ok(())
    | false -> match (contains_name name visible_names) {
      | true -> Ok(())
      | false -> Err("unbound type name `" + name + "`")
    }
  }

let type_parameter_names = fn parameters ->
  match parameters {
    | Nil -> Nil
    | Cons(TypeVariableName(name), remaining) ->
      Cons(name, type_parameter_names remaining)
  }

let validate_type_list = fn types -> fn visible_names -> fn type_variables ->
  match types {
    | Nil -> Ok(())
    | Cons(type_expr, remaining) -> match (validate_type type_expr visible_names type_variables) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_type_list remaining visible_names type_variables
    }
  }

let record_type_field_names = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordTypeField { name: ValueName(name), field_type: _ }, remaining) ->
      Cons(name, record_type_field_names remaining)
  }

let validate_record_field_types = fn fields -> fn visible_names -> fn type_variables ->
  match (validate_unique_names (record_type_field_names fields)
      "duplicate record field") {
    | Err(message) -> Err(message)
    | Ok(_) -> match fields {
      | Nil -> Ok(())
      | Cons(RecordTypeField { name: _, field_type }, remaining) ->
        match (validate_type field_type visible_names type_variables) {
          | Err(message) -> Err(message)
          | Ok(_) -> validate_record_field_types remaining visible_names type_variables
        }
    }
  }

let validate_variant_types = fn variants -> fn visible_names -> fn type_variables ->
  match variants {
    | Nil -> Ok(())
    | Cons(TupleVariant(TupleVariantData { constructor_name: _, fields }), remaining) ->
      match (validate_type_list fields visible_names type_variables) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_variant_types remaining visible_names type_variables
      }
    | Cons(RecordVariant(RecordVariantData { constructor_name: _, fields }), remaining) ->
      match (validate_record_field_types fields visible_names type_variables) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_variant_types remaining visible_names type_variables
      }
  }

let validate_type = fn type_expr -> fn visible_names -> fn type_variables ->
  match type_expr {
    | NamedType(NamedTypeExpression { name: TypeName(name), arguments }) ->
      match (validate_type_name name visible_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_type_list arguments visible_names type_variables
      }
    | ArrowType(ArrowTypeExpression { argument, result }) ->
      match (validate_type argument visible_names type_variables) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_type result visible_names type_variables
      }
    | TupleType(items) -> validate_type_list items visible_names type_variables
    | RecordType(fields) -> validate_record_field_types fields visible_names type_variables
    | VariantType(variants) -> validate_variant_types variants visible_names type_variables
    | TypeVariable(TypeVariableName(name)) -> match (contains_name name type_variables) {
      | true -> Ok(())
      | false -> Err("unbound type variable `" + name + "`")
    }
  }

let validate_type_definition = fn definition -> fn visible_names -> fn type_variables ->
  match definition {
    | TypeAlias(type_expr) -> validate_type type_expr visible_names type_variables
    | TypeRecord(fields) -> validate_record_field_types fields visible_names type_variables
    | TypeVariants(variants) -> validate_variant_types variants visible_names type_variables
  }

let validate_declaration_types = fn declarations -> fn visible_names ->
  match declarations {
    | Nil -> Ok(())
    | Cons(ValueDeclaration(_), remaining) ->
      validate_declaration_types remaining visible_names
    | Cons(TypeDeclaration(TypeDeclarationData {
        name: _, parameters, definition
      }), remaining) -> match (validate_unique_names
          (type_parameter_names parameters) "duplicate type parameter") {
      | Err(message) -> Err(message)
      | Ok(_) -> match (validate_type_definition definition visible_names
          (type_parameter_names parameters)) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_declaration_types remaining visible_names
      }
    }
  }

let bound_pattern_names = fn pattern ->
  match (pattern_kind pattern) {
    | Variable(ValueName(name)) -> Cons(name, Nil)
    | Wildcard -> Nil
    | PatternUnit -> Nil
    | PatternInteger(_) -> Nil
    | PatternBoolean(_) -> Nil
    | PatternString(_) -> Nil
    | TuplePattern(patterns) -> bound_pattern_list_names patterns
    | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
      bound_pattern_list_names arguments
    | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
      bound_record_pattern_names fields
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      bound_pattern_names inner
  }

let bound_pattern_list_names = fn patterns ->
  match patterns {
    | Nil -> Nil
    | Cons(pattern, remaining) -> append_names
      (bound_pattern_names pattern) (bound_pattern_list_names remaining)
  }

let bound_record_pattern_names = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordPatternField { name: _, pattern }, remaining) -> append_names
      (bound_pattern_names pattern) (bound_record_pattern_names remaining)
  }

let validate_unique_pattern_bindings = fn pattern ->
  match (find_duplicate_name (bound_pattern_names pattern)) {
    | None -> Ok(())
    | Some(name) -> Err("duplicate pattern binding `" + name + "`")
  }

let validate_value_name = fn name -> fn local_names -> fn module_names ->
  match (contains_name name local_names) {
    | true -> Ok(())
    | false -> match (contains_name name module_names) {
      | true -> Ok(())
      | false -> Err("unbound value name `" + name + "`")
    }
  }

let validate_type_annotation = fn annotation -> fn module_names ->
  match annotation {
    | None -> Ok(())
    | Some(type_expr) -> validate_type type_expr module_names Nil
  }

let validate_constructor_pattern = fn name -> fn module_names ->
  validate_type_name name module_names

let validate_pattern_list = fn patterns -> fn module_names ->
  match patterns {
    | Nil -> Ok(())
    | Cons(pattern, remaining) -> match (validate_pattern pattern module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_pattern_list remaining module_names
    }
  }

let validate_record_pattern_fields = fn fields -> fn module_names ->
  match fields {
    | Nil -> Ok(())
    | Cons(RecordPatternField { name: _, pattern }, remaining) ->
      match (validate_pattern pattern module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_record_pattern_fields remaining module_names
      }
  }

let validate_pattern = fn pattern -> fn module_names ->
  match (pattern_kind pattern) {
    | Variable(_) -> Ok(())
    | Wildcard -> Ok(())
    | PatternUnit -> Ok(())
    | PatternInteger(_) -> Ok(())
    | PatternBoolean(_) -> Ok(())
    | PatternString(_) -> Ok(())
    | TuplePattern(patterns) -> match (validate_pattern_list patterns module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_unique_pattern_bindings pattern
    }
    | ConstructorPattern(ConstructorPatternData {
        constructor_name: ConstructorName(name), arguments
      }) -> match (validate_constructor_pattern name module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (validate_pattern_list arguments module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_unique_pattern_bindings pattern
      }
    }
    | RecordPattern(RecordPatternData {
        constructor_name: ConstructorName(name), fields
      }) -> match (validate_constructor_pattern name module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> match (validate_record_pattern_fields fields module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_unique_pattern_bindings pattern
      }
    }
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation }) ->
      match (validate_type annotation module_names Nil) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_pattern inner module_names
      }
  }

let validate_expression_list = fn expressions -> fn local_names -> fn module_names ->
  match expressions {
    | Nil -> Ok(())
    | Cons(expression, remaining) -> match (validate_expression
        expression local_names module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_expression_list remaining local_names module_names
    }
  }

let validate_record_expression_fields = fn fields -> fn local_names -> fn module_names ->
  match fields {
    | Nil -> Ok(())
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      match (validate_expression value local_names module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_record_expression_fields remaining local_names module_names
      }
  }

let validate_match_arms = fn arms -> fn local_names -> fn module_names ->
  match arms {
    | Nil -> Ok(())
    | Cons(MatchArm { pattern, body }, remaining) ->
      match (validate_pattern pattern module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (validate_expression body
          (append_names (bound_pattern_names pattern) local_names) module_names) {
          | Err(message) -> Err(message)
          | Ok(_) -> validate_match_arms remaining local_names module_names
        }
      }
  }

let validate_expression = fn expression -> fn local_names -> fn module_names ->
  match (expression_kind expression) {
    | Integer(_) -> Ok(())
    | Boolean(_) -> Ok(())
    | StringLiteral(_) -> Ok(())
    | Unit -> Ok(())
    | VariableExpr(ValueName(name)) -> validate_value_name name local_names module_names
    | TupleExpr(items) -> validate_expression_list items local_names module_names
    | RecordExpr(RecordExpression {
        constructor_name: ConstructorName(name), fields
      }) -> match (validate_type_name name module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_record_expression_fields fields local_names module_names
    }
    | Binary(BinaryExpression { operator: _, left, right }) ->
      match (validate_expression left local_names module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_expression right local_names module_names
      }
    | Negate(operand) -> validate_expression operand local_names module_names
    | Function(FunctionExpression { parameter, body }) ->
      match (validate_pattern parameter module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_expression body
          (append_names (bound_pattern_names parameter) local_names) module_names
      }
    | Apply(ApplicationExpression { callee, argument }) ->
      match (validate_expression callee local_names module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_expression argument local_names module_names
      }
    | Let(LetExpression { pattern, annotation, value, body }) ->
      match (validate_type_annotation annotation module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> match (validate_pattern pattern module_names) {
          | Err(message) -> Err(message)
          | Ok(_) -> match (validate_expression value local_names module_names) {
            | Err(message) -> Err(message)
            | Ok(_) -> validate_expression body
              (append_names (bound_pattern_names pattern) local_names) module_names
          }
        }
      }
    | Match(MatchExpression { scrutinee, arms }) ->
      match (validate_expression scrutinee local_names module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_match_arms arms local_names module_names
      }
    | ConstructorExpr(ConstructorExpression {
        constructor_name: ConstructorName(name), arguments
      }) -> match (validate_constructor_pattern name module_names) {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_expression_list arguments local_names module_names
    }
    | Ascription(AscriptionExpression { expression: inner, annotation }) ->
      match (validate_type annotation module_names Nil) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_expression inner local_names module_names
      }
    | Block(items) -> validate_expression_list items local_names module_names
  }

let validate_declaration_values = fn declarations -> fn module_names ->
  match declarations {
    | Nil -> Ok(())
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body }), remaining) ->
      match (validate_expression body Nil module_names) {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_declaration_values remaining module_names
      }
    | Cons(TypeDeclaration(_), remaining) ->
      validate_declaration_values remaining module_names
  }

let validate_unique_declarations = fn declarations ->
  match (validate_unique_names (declared_value_names declarations)
      "duplicate value declaration") {
    | Err(message) -> Err(message)
    | Ok(_) -> match (validate_unique_names
        (declared_type_declaration_names declarations) "duplicate type declaration") {
      | Err(message) -> Err(message)
      | Ok(_) -> validate_unique_names (declared_constructor_names declarations)
        "duplicate constructor name"
    }
  }

let validate_source_module = fn (SourceModule { imports, declarations }) -> {
  let imported_names = selected_import_names imports;
  let local_names = append_names (declared_value_names declarations)
    (declared_type_names declarations);
  let module_names = append_names local_names imported_names;
  match (validate_unique_declarations declarations) {
    | Err(message) -> Err(message)
    | Ok(_) -> match (validate_unique_names imported_names
        "ambiguous imported name") {
      | Err(message) -> Err(message)
      | Ok(_) -> match (find_common_name imported_names local_names) {
        | Some(name) -> Err("imported name conflicts with local declaration `" + name + "`")
        | None -> match (validate_declaration_types declarations module_names) {
          | Err(message) -> Err(message)
          | Ok(_) -> validate_declaration_values declarations module_names
        }
      }
    }
  }
}

let check_source_module_names = fn source_module ->
  match (validate_source_module source_module) {
    | Err(message) -> message
    | Ok(_) -> ""
  }
