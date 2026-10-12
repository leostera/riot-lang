// Hindley-Milner inference for RiotML source modules.
use Prelude.{List, Nil, Cons, Option, Some, None, Result, Ok, Err}
use Std.List.{fold}
use String.{equal}
use Syntax.Ast.{
  TypeName, ValueName, ModuleName, SourceModule, Declaration, ValueDeclaration,
  ValueDeclarationData, TypeDeclaration, TypeDeclarationData, TypeDefinition,
  TypeAlias, TypeRecord, TypeVariants, VariantDeclaration, TupleVariant,
  TupleVariantData, RecordVariant, RecordVariantData, RecordTypeField,
  TypeVariableName, ConstructorName, Import, ImportedName, MemberName,
  TypeExpr, NamedType, NamedTypeExpression, ArrowTypeExpression, TypeVariable,
  ArrowType, TupleType, RecordType, VariantType,
  Expr, Integer, IntegerLiteral, Boolean, StringLiteral, Unit, VariableExpr,
  TupleExpr, RecordExpr, RecordExpression, RecordExpressionField, Binary,
  BinaryExpression, BinaryOperator, Addition, Subtraction, Multiplication,
  Division, Remainder, Equal, NotEqual, LessThan, LessThanOrEqual,
  GreaterThan, GreaterThanOrEqual, Negate, Function, FunctionExpression,
  Apply, ApplicationExpression, Let, LetExpression, Match, MatchExpression,
  MatchArm, ConstructorExpr, ConstructorExpression, Ascription,
  AscriptionExpression, Block, Pattern, Variable, PatternUnit, Wildcard,
  PatternInteger, PatternBoolean, PatternString, TuplePattern,
  ConstructorPattern, ConstructorPatternData, RecordPattern,
  RecordPatternData, RecordPatternField, AscribedPattern, AscribedPatternData,
  ModulePath, expression_kind, pattern_kind, expression_span, pattern_span
}
use Syntax.Lexer.{SourceSpan}
use Check.Names.{check_source_module_names}
use Check.IntegerLiterals.{integer_type_name, integer_literal_fits}
use Modules.{LoadedModule}
use Host.{read_file}

type InferenceType =
  | FreeVariable(i32)
  | GenericVariable(i32)
  | AppliedType(InferredNamedType)
  | FunctionType(InferredFunctionType)
  | InferredTuple(List<InferenceType>)
  | InferredRecord(List<InferredRecordField>)
  | InferredVariant(List<InferredVariantCase>)

type InferredNamedType = InferredNamedType {
  name: string,
  arguments: List<InferenceType>
}

type InferredFunctionType = InferredFunctionType {
  argument: InferenceType,
  result: InferenceType
}

type InferredRecordField = InferredRecordField {
  name: ValueName,
  field_type: InferenceType
}

type InferredVariantCase = InferredVariantCase {
  name: ConstructorName,
  fields: List<InferenceType>
}

type TypeSubstitution = TypeSubstitution {
  variable: i32,
  replacement: InferenceType
}

type ExpressionTypeAnnotation = ExpressionTypeAnnotation {
  span: SourceSpan,
  inferred_type: InferenceType
}

type PatternTypeAnnotation = PatternTypeAnnotation {
  span: SourceSpan,
  inferred_type: InferenceType
}

type BindingScope = ModuleValueBinding | LocalValueBinding

type BindingIdentity = BindingIdentity {
  source_path: string,
  declaration_span: SourceSpan,
  name: ValueName,
  scope: BindingScope
}

type ResolvedValueReference = ResolvedValueReference {
  reference_span: SourceSpan,
  binding: BindingIdentity
}

type ConstructorIdentity = ConstructorIdentity {
  type_identity: string,
  constructor_name: ConstructorName
}

type ResolvedConstructorReference = ResolvedConstructorReference {
  reference_span: SourceSpan,
  entity: ConstructorIdentity
}

// Annotation variables are interpreted through the final module substitution table.
type TypedModule = TypedModule {
  source_module: SourceModule,
  expression_types: List<ExpressionTypeAnnotation>,
  pattern_types: List<PatternTypeAnnotation>,
  nominal_types: List<TypeVisibility>,
  substitutions: List<TypeSubstitution>,
  bindings: List<BindingIdentity>,
  value_references: List<ResolvedValueReference>,
  constructor_references: List<ResolvedConstructorReference>
}

type BindingEnvironmentEntry = BindingEnvironmentEntry {
  name: ValueName,
  binding: BindingIdentity
}

type BindingResolution = BindingResolution {
  bindings: List<BindingIdentity>,
  value_references: List<ResolvedValueReference>,
  constructor_references: List<ResolvedConstructorReference>
}

type PatternResolution = PatternResolution {
  environment: List<BindingEnvironmentEntry>,
  state: BindingResolution
}

type InferenceState = InferenceState {
  next_variable: i32,
  substitutions: List<TypeSubstitution>,
  expression_annotations: List<ExpressionTypeAnnotation>,
  pattern_annotations: List<PatternTypeAnnotation>
}

type TypeScheme = TypeScheme {
  quantified_variables: List<i32>,
  body: InferenceType
}

type ValueScheme = SchemeBinding(ValueName, TypeScheme)

type FreshType = FreshInference(InferenceType, InferenceState)

type FreshTypeMapping = FreshTypeMapping {
  generic_variable: i32,
  fresh_type: InferenceType
}

type FreshMappings = FreshMappings {
  mappings: List<FreshTypeMapping>,
  state: InferenceState
}

type InstantiationState = InstantiationState {
  inferred_type: InferenceType,
  state: InferenceState
}

type TypeVisibility = VisibleType(VisibleTypeData)

type VisibleTypeData = VisibleTypeData {
  declaration: TypeDeclarationData,
  type_identity: string,
  visible_names: List<string>,
  available_names: List<string>
}

type InferredModuleInterface = InferredModuleInterface {
  values: List<ValueScheme>,
  types: List<TypeVisibility>
}

type CheckedModuleInterface = CheckedModuleInterface {
  loaded_module: LoadedModule,
  interface: InferredModuleInterface,
  typed_module: TypedModule
}

type PreparedValues = PreparedValues {
  bindings: List<ValueScheme>,
  state: InferenceState
}

type InferenceResult = InferenceResult {
  inferred_type: InferenceType,
  state: InferenceState
}

type PatternBindings = PatternBindings {
  bindings: List<ValueScheme>,
  state: InferenceState
}

type TypeConversion = TypeConversion {
  inferred_type: InferenceType,
  state: InferenceState
}

type TypeParameterBinding = TypeParameterBinding {
  name: string,
  inferred_type: InferenceType
}

type TypeParameters = TypedParameters(TypeParameterState)

type TypeParameterState = TypeParameterState {
  bindings: List<TypeParameterBinding>,
  generic_variables: List<i32>,
  next_index: i32
}

type TypeArgumentBindings = TypeArgumentBindings {
  bindings: List<TypeParameterBinding>,
  remaining_parameters: List<TypeVariableName>,
  remaining_arguments: List<InferenceType>
}

type InferredConstructor = InferredConstructor {
  name: ConstructorName,
  scheme: TypeScheme,
  field_names: List<ValueName>
}

type ConstructorShape = ConstructorShape(List<InferenceType>, InferenceType)

type InferredTypeList = InferredTypeList(List<InferenceType>, InferenceState)

type InstantiatedConstructor = InstantiatedConstructor(
  List<InferenceType>, InferenceType, InferenceState
)

type InferenceArgumentState = InferenceArgumentState {
  inferred_type: InferenceType,
  state: InferenceState
}

let initial_inference_state = fn () -> InferenceState {
  next_variable: 0,
  substitutions: Nil,
  expression_annotations: Nil,
  pattern_annotations: Nil
}

let lookup_substitution = fn variable -> fn substitutions ->
  match substitutions {
    | Nil -> None
    | Cons(TypeSubstitution { variable: existing, replacement }, remaining) ->
      match (variable == existing) {
        | true -> Some(replacement)
        | false -> lookup_substitution variable remaining
      }
  }

let apply_substitutions = fn inferred_type -> fn substitutions ->
  match inferred_type {
    | FreeVariable(variable) -> match (lookup_substitution variable substitutions) {
      | None -> FreeVariable(variable)
      | Some(replacement) -> apply_substitutions replacement substitutions
    }
    | GenericVariable(variable) -> GenericVariable(variable)
    | AppliedType(InferredNamedType { name, arguments }) ->
      AppliedType(InferredNamedType {
        name: name,
        arguments: apply_type_list_substitutions arguments substitutions
      })
    | FunctionType(InferredFunctionType { argument, result }) ->
      FunctionType(InferredFunctionType {
        argument: apply_substitutions argument substitutions,
        result: apply_substitutions result substitutions
      })
    | InferredTuple(items) -> InferredTuple(apply_type_list_substitutions items substitutions)
    | InferredRecord(fields) -> InferredRecord(apply_record_substitutions fields substitutions)
    | InferredVariant(cases) -> InferredVariant(apply_variant_substitutions cases substitutions)
  }

let apply_record_substitutions = fn fields -> fn substitutions ->
  match fields {
    | Nil -> Nil
    | Cons(InferredRecordField { name, field_type }, remaining) -> Cons(
      InferredRecordField {
        name: name,
        field_type: apply_substitutions field_type substitutions
      },
      apply_record_substitutions remaining substitutions)
  }

let apply_variant_substitutions = fn cases -> fn substitutions ->
  match cases {
    | Nil -> Nil
    | Cons(InferredVariantCase { name, fields }, remaining) -> Cons(
      InferredVariantCase {
        name: name,
        fields: apply_type_list_substitutions fields substitutions
      },
      apply_variant_substitutions remaining substitutions)
  }

let apply_type_list_substitutions = fn types -> fn substitutions ->
  match types {
    | Nil -> Nil
    | Cons(inferred_type, remaining) ->
      Cons(apply_substitutions inferred_type substitutions,
        apply_type_list_substitutions remaining substitutions)
  }

let type_occurs = fn wanted_variable -> fn inferred_type -> fn substitutions ->
  match (apply_substitutions inferred_type substitutions) {
    | FreeVariable(variable) -> wanted_variable == variable
    | GenericVariable(_) -> false
    | AppliedType(InferredNamedType { name: _, arguments }) ->
      type_list_occurs wanted_variable arguments substitutions
    | FunctionType(InferredFunctionType { argument, result }) ->
      match (type_occurs wanted_variable argument substitutions) {
        | true -> true
        | false -> type_occurs wanted_variable result substitutions
      }
    | InferredTuple(items) -> type_list_occurs wanted_variable items substitutions
    | InferredRecord(fields) -> record_fields_occur wanted_variable fields substitutions
    | InferredVariant(cases) -> variant_cases_occur wanted_variable cases substitutions
  }

let record_fields_occur = fn wanted_variable -> fn fields -> fn substitutions ->
  match fields {
    | Nil -> false
    | Cons(InferredRecordField { name: _, field_type }, remaining) ->
      match (type_occurs wanted_variable field_type substitutions) {
        | true -> true
        | false -> record_fields_occur wanted_variable remaining substitutions
      }
  }

let variant_cases_occur = fn wanted_variable -> fn cases -> fn substitutions ->
  match cases {
    | Nil -> false
    | Cons(InferredVariantCase { name: _, fields }, remaining) ->
      match (type_list_occurs wanted_variable fields substitutions) {
        | true -> true
        | false -> variant_cases_occur wanted_variable remaining substitutions
      }
  }

let type_list_occurs = fn wanted_variable -> fn types -> fn substitutions ->
  match types {
    | Nil -> false
    | Cons(inferred_type, remaining) -> match (type_occurs
        wanted_variable inferred_type substitutions) {
      | true -> true
      | false -> type_list_occurs wanted_variable remaining substitutions
    }
  }

let fresh_type_variable = fn (InferenceState {
  next_variable, substitutions, expression_annotations, pattern_annotations
}) -> FreshInference(
  FreeVariable(next_variable),
  InferenceState {
    next_variable: next_variable + 1,
    substitutions: substitutions,
    expression_annotations: expression_annotations,
    pattern_annotations: pattern_annotations
  }
)

let add_type_substitution = fn variable -> fn replacement ->
  fn (InferenceState {
    next_variable, substitutions, expression_annotations, pattern_annotations
  }) -> InferenceState {
    next_variable: next_variable,
    substitutions: Cons(TypeSubstitution {
      variable: variable,
      replacement: replacement
    }, substitutions),
    expression_annotations: expression_annotations,
    pattern_annotations: pattern_annotations
  }

let unify_type_lists = fn expected -> fn actual -> fn state ->
  match expected {
    | Nil -> match actual {
      | Nil -> Ok(state)
      | Cons(_, _) -> Err("tuple types have different lengths")
    }
    | Cons(expected_type, expected_remaining) -> match actual {
      | Nil -> Err("tuple types have different lengths")
      | Cons(actual_type, actual_remaining) -> match (unify_types
          expected_type actual_type state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> unify_type_lists expected_remaining actual_remaining next_state
      }
    }
  }

let bind_free_type_variable = fn variable -> fn replacement ->
  fn (InferenceState {
    next_variable, substitutions, expression_annotations, pattern_annotations
  }) -> {
  let state = InferenceState {
    next_variable: next_variable,
    substitutions: substitutions,
    expression_annotations: expression_annotations,
    pattern_annotations: pattern_annotations
  };
  match (type_occurs variable replacement substitutions) {
    | true -> Err("recursive type")
    | false -> Ok(add_type_substitution variable replacement state)
  }
}

let unify_types = fn expected -> fn actual -> fn (InferenceState {
  next_variable,
  substitutions,
  expression_annotations,
  pattern_annotations
}) -> {
  let state = InferenceState {
    next_variable: next_variable,
    substitutions: substitutions,
    expression_annotations: expression_annotations,
    pattern_annotations: pattern_annotations
  };
  let expected_resolved = apply_substitutions expected substitutions;
  let actual_resolved = apply_substitutions actual substitutions;
  match expected_resolved {
    | FreeVariable(expected_variable) -> match actual_resolved {
      | FreeVariable(actual_variable) -> match (expected_variable == actual_variable) {
        | true -> Ok(state)
        | false -> bind_free_type_variable expected_variable actual_resolved state
      }
      | _ -> bind_free_type_variable expected_variable actual_resolved state
    }
    | GenericVariable(expected_variable) -> match actual_resolved {
      | GenericVariable(actual_variable) -> match (expected_variable == actual_variable) {
        | true -> Ok(state)
        | false -> Err("type mismatch")
      }
      | _ -> Err("type mismatch")
    }
    | AppliedType(InferredNamedType {
        name: expected_name,
        arguments: expected_arguments
      }) -> match actual_resolved {
      | FreeVariable(actual_variable) -> bind_free_type_variable
        actual_variable expected_resolved state
      | AppliedType(InferredNamedType {
          name: actual_name,
          arguments: actual_arguments
        }) -> match (equal expected_name actual_name) {
        | false -> Err("type mismatch: expected `" + expected_name + "`, got `" + actual_name + "`")
        | true -> unify_type_lists expected_arguments actual_arguments state
      }
      | _ -> Err("type mismatch")
    }
    | FunctionType(InferredFunctionType {
        argument: expected_argument,
        result: expected_result
      }) -> match actual_resolved {
      | FreeVariable(actual_variable) -> bind_free_type_variable
        actual_variable expected_resolved state
      | FunctionType(InferredFunctionType {
          argument: actual_argument,
          result: actual_result
        }) -> match (unify_types expected_argument actual_argument state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> unify_types expected_result actual_result next_state
      }
      | _ -> Err("type mismatch: expected a function")
    }
    | InferredTuple(expected_items) -> match actual_resolved {
      | FreeVariable(actual_variable) -> bind_free_type_variable
        actual_variable expected_resolved state
      | InferredTuple(actual_items) -> unify_type_lists expected_items actual_items state
      | _ -> Err("type mismatch: expected a tuple")
    }
    | InferredRecord(expected_fields) -> match actual_resolved {
      | FreeVariable(actual_variable) -> bind_free_type_variable
        actual_variable expected_resolved state
      | InferredRecord(actual_fields) -> unify_record_fields expected_fields actual_fields state
      | _ -> Err("type mismatch: expected a record")
    }
    | InferredVariant(expected_cases) -> match actual_resolved {
      | FreeVariable(actual_variable) -> bind_free_type_variable
        actual_variable expected_resolved state
      | InferredVariant(actual_cases) -> unify_variant_cases expected_cases actual_cases state
      | _ -> Err("type mismatch: expected a variant")
    }
  }
}

let unify_record_fields = fn expected -> fn actual -> fn state ->
  match expected {
    | Nil -> match actual {
      | Nil -> Ok(state)
      | Cons(_, _) -> Err("record types have different fields")
    }
    | Cons(InferredRecordField {
        name: ValueName(expected_name),
        field_type: expected_type
      }, expected_remaining) -> match actual {
      | Nil -> Err("record types have different fields")
      | Cons(InferredRecordField {
          name: ValueName(actual_name),
          field_type: actual_type
        }, actual_remaining) -> match (equal expected_name actual_name) {
        | false -> Err("record types have different fields")
        | true -> match (unify_types expected_type actual_type state) {
          | Err(message) -> Err(message)
          | Ok(next_state) -> unify_record_fields expected_remaining actual_remaining next_state
        }
      }
    }
  }

let unify_variant_cases = fn expected -> fn actual -> fn state ->
  match expected {
    | Nil -> match actual {
      | Nil -> Ok(state)
      | Cons(_, _) -> Err("variant types have different constructors")
    }
    | Cons(InferredVariantCase {
        name: ConstructorName(expected_name),
        fields: expected_fields
      }, expected_remaining) -> match actual {
      | Nil -> Err("variant types have different constructors")
      | Cons(InferredVariantCase {
          name: ConstructorName(actual_name),
          fields: actual_fields
        }, actual_remaining) -> match (equal expected_name actual_name) {
        | false -> Err("variant types have different constructors")
        | true -> match (unify_type_lists expected_fields actual_fields state) {
          | Err(message) -> Err(message)
          | Ok(next_state) -> unify_variant_cases expected_remaining actual_remaining next_state
        }
      }
    }
  }

let integer_contains = fn wanted -> fn integers ->
  match integers {
    | Nil -> false
    | Cons(integer, remaining) -> match (wanted == integer) {
      | true -> true
      | false -> integer_contains wanted remaining
    }
  }

let append_unique_integers = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(integer, remaining) -> match (integer_contains integer second) {
      | true -> append_unique_integers remaining second
      | false -> Cons(integer, append_unique_integers remaining (Cons(integer, second)))
    }
  }

let free_type_variables = fn inferred_type ->
  match inferred_type {
    | FreeVariable(variable) -> Cons(variable, Nil)
    | GenericVariable(_) -> Nil
    | AppliedType(InferredNamedType { name: _, arguments }) -> free_type_variable_list arguments
    | FunctionType(InferredFunctionType { argument, result }) -> append_unique_integers
      (free_type_variables argument) (free_type_variables result)
    | InferredTuple(items) -> free_type_variable_list items
    | InferredRecord(fields) -> free_record_field_variables fields
    | InferredVariant(cases) -> free_variant_case_variables cases
  }

let free_record_field_variables = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(InferredRecordField { name: _, field_type }, remaining) ->
      append_unique_integers (free_type_variables field_type)
        (free_record_field_variables remaining)
  }

let free_variant_case_variables = fn cases ->
  match cases {
    | Nil -> Nil
    | Cons(InferredVariantCase { name: _, fields }, remaining) ->
      append_unique_integers (free_type_variable_list fields)
        (free_variant_case_variables remaining)
  }

let free_type_variable_list = fn inferred_types ->
  match inferred_types {
    | Nil -> Nil
    | Cons(inferred_type, remaining) -> append_unique_integers
      (free_type_variables inferred_type) (free_type_variable_list remaining)
  }

let free_scheme_variables = fn (TypeScheme { quantified_variables: _, body }) ->
  fn substitutions -> free_type_variables (apply_substitutions body substitutions)

let free_environment_variables = fn environment -> fn substitutions ->
  match environment {
    | Nil -> Nil
    | Cons(SchemeBinding(_, scheme), remaining) -> append_unique_integers
      (free_scheme_variables scheme substitutions)
      (free_environment_variables remaining substitutions)
  }

let generalize_type = fn environment -> fn inferred_type -> fn substitutions -> {
  let resolved_type = apply_substitutions inferred_type substitutions;
  let type_variables = free_type_variables resolved_type;
  let environment_variables = free_environment_variables environment substitutions;
  let quantified_variables = retain_unbound_variables type_variables environment_variables;
  TypeScheme {
    quantified_variables: quantified_variables,
    body: replace_free_variables_with_generics resolved_type quantified_variables
  }
}

let retain_unbound_variables = fn variables -> fn environment_variables ->
  match variables {
    | Nil -> Nil
    | Cons(variable, remaining) -> match (integer_contains variable environment_variables) {
      | true -> retain_unbound_variables remaining environment_variables
      | false -> Cons(variable, retain_unbound_variables remaining environment_variables)
    }
  }

let replace_free_variables_with_generics = fn inferred_type -> fn variables ->
  match inferred_type {
    | FreeVariable(variable) -> match (integer_contains variable variables) {
      | true -> GenericVariable(variable)
      | false -> FreeVariable(variable)
    }
    | GenericVariable(variable) -> GenericVariable(variable)
    | AppliedType(InferredNamedType { name, arguments }) ->
      AppliedType(InferredNamedType {
        name: name,
        arguments: replace_free_variable_list arguments variables
      })
    | FunctionType(InferredFunctionType { argument, result }) ->
      FunctionType(InferredFunctionType {
        argument: replace_free_variables_with_generics argument variables,
        result: replace_free_variables_with_generics result variables
      })
    | InferredTuple(items) -> InferredTuple(replace_free_variable_list items variables)
    | InferredRecord(fields) -> InferredRecord(replace_record_fields fields variables)
    | InferredVariant(cases) -> InferredVariant(replace_variant_cases cases variables)
  }

let replace_record_fields = fn fields -> fn variables ->
  match fields {
    | Nil -> Nil
    | Cons(InferredRecordField { name, field_type }, remaining) -> Cons(
      InferredRecordField {
        name: name,
        field_type: replace_free_variables_with_generics field_type variables
      },
      replace_record_fields remaining variables)
  }

let replace_variant_cases = fn cases -> fn variables ->
  match cases {
    | Nil -> Nil
    | Cons(InferredVariantCase { name, fields }, remaining) -> Cons(
      InferredVariantCase {
        name: name,
        fields: replace_free_variable_list fields variables
      },
      replace_variant_cases remaining variables)
  }

let replace_free_variable_list = fn inferred_types -> fn variables ->
  match inferred_types {
    | Nil -> Nil
    | Cons(inferred_type, remaining) -> Cons(
      replace_free_variables_with_generics inferred_type variables,
      replace_free_variable_list remaining variables)
  }

let find_fresh_type_mapping = fn wanted_variable -> fn mappings ->
  match mappings {
    | Nil -> None
    | Cons(FreshTypeMapping { generic_variable, fresh_type }, remaining) ->
      match (wanted_variable == generic_variable) {
        | true -> Some(fresh_type)
        | false -> find_fresh_type_mapping wanted_variable remaining
      }
  }

let replace_scheme_generics = fn inferred_type -> fn quantified -> fn mappings ->
  fn (state: InferenceState) ->
  match inferred_type {
    | GenericVariable(variable) -> match (integer_contains variable quantified) {
      | false -> Ok(InstantiationState {
        inferred_type: GenericVariable(variable), state: state
      })
      | true -> match (find_fresh_type_mapping variable mappings) {
        | Some(fresh_type) -> Ok(InstantiationState {
          inferred_type: fresh_type, state: state
        })
        | None -> Err("missing type variable mapping")
      }
    }
    | FreeVariable(variable) -> Ok(InstantiationState {
      inferred_type: FreeVariable(variable), state: state
    })
    | AppliedType(InferredNamedType { name, arguments }) ->
      match (replace_scheme_type_list arguments quantified mappings state) {
        | Err(message) -> Err(message)
        | Ok(InstantiationState {
            inferred_type: InferredTuple(replaced_arguments), state: next_state
          }) -> Ok(InstantiationState {
            inferred_type: AppliedType(InferredNamedType { name: name, arguments: replaced_arguments }),
            state: next_state
          })
        | Ok(_) -> Err("internal error replacing a named type")
      }
    | FunctionType(InferredFunctionType { argument, result }) ->
      match (replace_scheme_generics argument quantified mappings state) {
        | Err(message) -> Err(message)
        | Ok(InstantiationState { inferred_type: replaced_argument, state: next_state }) ->
          match (replace_scheme_generics result quantified mappings next_state) {
            | Err(message) -> Err(message)
            | Ok(InstantiationState { inferred_type: replaced_result, state: final_state }) ->
              Ok(InstantiationState {
                inferred_type: FunctionType(InferredFunctionType {
                  argument: replaced_argument,
                  result: replaced_result
                }),
                state: final_state
              })
          }
      }
    | InferredTuple(items) -> match (replace_scheme_type_list items quantified mappings state) {
      | Err(message) -> Err(message)
      | Ok(InstantiationState { inferred_type: InferredTuple(replaced_items), state: next_state }) ->
        Ok(InstantiationState { inferred_type: InferredTuple(replaced_items), state: next_state })
      | Ok(_) -> Err("internal error replacing a tuple type")
    }
    | InferredRecord(fields) -> match (replace_scheme_record_fields fields quantified mappings state) {
      | Err(message) -> Err(message)
      | Ok(InstantiationState { inferred_type: InferredRecord(replaced_fields), state: next_state }) ->
        Ok(InstantiationState { inferred_type: InferredRecord(replaced_fields), state: next_state })
      | Ok(_) -> Err("internal error replacing a record type")
    }
    | InferredVariant(cases) -> match (replace_scheme_variant_cases cases quantified mappings state) {
      | Err(message) -> Err(message)
      | Ok(InstantiationState { inferred_type: InferredVariant(replaced_cases), state: next_state }) ->
        Ok(InstantiationState { inferred_type: InferredVariant(replaced_cases), state: next_state })
      | Ok(_) -> Err("internal error replacing a variant type")
    }
  }

let replace_scheme_record_fields = fn fields -> fn quantified -> fn mappings ->
  fn (state: InferenceState) ->
  match fields {
    | Nil -> Ok(InstantiationState { inferred_type: InferredRecord(Nil), state: state })
    | Cons(InferredRecordField { name, field_type }, remaining) ->
      match (replace_scheme_generics field_type quantified mappings state) {
        | Err(message) -> Err(message)
        | Ok(InstantiationState { inferred_type: replaced_type, state: next_state }) ->
          match (replace_scheme_record_fields remaining quantified mappings next_state) {
            | Err(message) -> Err(message)
            | Ok(InstantiationState { inferred_type: InferredRecord(rest), state: final_state }) ->
              Ok(InstantiationState {
                inferred_type: InferredRecord(Cons(InferredRecordField {
                  name: name,
                  field_type: replaced_type
                }, rest)),
                state: final_state
              })
            | Ok(_) -> Err("internal error replacing record fields")
          }
      }
  }

let replace_scheme_variant_cases = fn cases -> fn quantified -> fn mappings ->
  fn (state: InferenceState) ->
  match cases {
    | Nil -> Ok(InstantiationState { inferred_type: InferredVariant(Nil), state: state })
    | Cons(InferredVariantCase { name, fields }, remaining) ->
      match (replace_scheme_type_list fields quantified mappings state) {
        | Err(message) -> Err(message)
        | Ok(InstantiationState { inferred_type: InferredTuple(replaced_fields), state: next_state }) ->
          match (replace_scheme_variant_cases remaining quantified mappings next_state) {
            | Err(message) -> Err(message)
            | Ok(InstantiationState { inferred_type: InferredVariant(rest), state: final_state }) ->
              Ok(InstantiationState {
                inferred_type: InferredVariant(Cons(InferredVariantCase {
                  name: name,
                  fields: replaced_fields
                }, rest)),
                state: final_state
              })
            | Ok(_) -> Err("internal error replacing variant cases")
          }
        | Ok(_) -> Err("internal error replacing variant fields")
      }
  }

let replace_scheme_type_list = fn inferred_types -> fn quantified -> fn mappings ->
  fn (state: InferenceState) ->
  match inferred_types {
    | Nil -> Ok(InstantiationState { inferred_type: InferredTuple(Nil), state: state })
    | Cons(inferred_type, remaining) -> match (replace_scheme_generics inferred_type quantified mappings state) {
      | Err(message) -> Err(message)
      | Ok(InstantiationState { inferred_type: first_type, state: next_state }) ->
        match (replace_scheme_type_list remaining quantified mappings next_state) {
          | Err(message) -> Err(message)
          | Ok(InstantiationState { inferred_type: InferredTuple(rest_types), state: final_state }) ->
            Ok(InstantiationState {
              inferred_type: InferredTuple(Cons(first_type, rest_types)),
              state: final_state
            })
          | Ok(_) -> Err("internal error replacing a type list")
        }
    }
  }

let create_fresh_mappings = fn variables -> fn (InferenceState {
  next_variable,
  substitutions,
  expression_annotations,
  pattern_annotations
}) ->
  match variables {
    | Nil -> Ok(FreshMappings {
      mappings: Nil,
      state: InferenceState {
        next_variable: next_variable,
        substitutions: substitutions,
        expression_annotations: expression_annotations,
        pattern_annotations: pattern_annotations
      }
    })
    | Cons(variable, remaining) -> {
      let inferred_type = FreeVariable(next_variable);
      let next_state = InferenceState {
        next_variable: next_variable + 1,
        substitutions: substitutions,
        expression_annotations: expression_annotations,
        pattern_annotations: pattern_annotations
      };
      match (create_fresh_mappings remaining next_state) {
        | Err(message) -> Err(message)
        | Ok(FreshMappings { mappings, state: final_state }) -> Ok(FreshMappings {
          mappings: Cons(FreshTypeMapping {
            generic_variable: variable,
            fresh_type: inferred_type
          }, mappings),
          state: final_state
        })
      }
    }
  }

let instantiate_scheme = fn scheme -> fn state -> {
  let TypeScheme { quantified_variables, body } = scheme;
  let substitutions = module_state_substitutions state;
  let resolved_body = apply_substitutions body substitutions;
  match (create_fresh_mappings quantified_variables state) {
    | Err(message) -> Err(message)
    | Ok(FreshMappings { mappings, state: next_state }) ->
      match (replace_scheme_generics resolved_body quantified_variables mappings next_state) {
        | Err(message) -> Err(message)
        | Ok(InstantiationState { inferred_type, state: final_state }) ->
          Ok(FreshInference(inferred_type, final_state))
      }
  }
}

let contains_text = fn wanted -> fn names ->
  match names {
    | Nil -> false
    | Cons(name, remaining) -> match (equal wanted name) {
      | true -> true
      | false -> contains_text wanted remaining
    }
  }

let imported_member_names = fn names ->
  match names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      Cons(name, imported_member_names remaining)
  }

let append_texts = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(name, remaining) -> Cons(name, append_texts remaining second)
  }

let value_name_text = fn (ValueName(name)) -> name
let type_name_text = fn (TypeName(name)) -> name
let constructor_name_text = fn (ConstructorName(name)) -> name

let record_type_field_names = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordTypeField { name: ValueName(name), field_type: _ }, remaining) ->
      Cons(name, record_type_field_names remaining)
  }

let variant_names = fn variants ->
  match variants {
    | Nil -> Nil
    | Cons(TupleVariant(TupleVariantData {
        constructor_name: ConstructorName(name), fields: _
      }), remaining) -> Cons(name, variant_names remaining)
    | Cons(RecordVariant(RecordVariantData {
        constructor_name: ConstructorName(name), fields
      }), remaining) -> append_texts (Cons(name, record_type_field_names fields))
        (variant_names remaining)
  }

let type_declaration_export_names = fn (TypeDeclarationData {
  name: TypeName(name), parameters: _, definition
}) ->
  match definition {
    | TypeAlias(_) -> Cons(name, Nil)
    | TypeRecord(fields) -> append_texts (Cons(name, Nil)) (record_type_field_names fields)
    | TypeVariants(variants) -> append_texts (Cons(name, Nil)) (variant_names variants)
  }

let declaration_type_declarations = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(TypeDeclaration(declaration), remaining) ->
      Cons(declaration, declaration_type_declarations remaining)
    | Cons(ValueDeclaration(_), remaining) -> declaration_type_declarations remaining
  }

let find_visible_type_visibility = fn wanted_name -> fn visible_types ->
  match visible_types {
    | Nil -> None
    | Cons(VisibleType(VisibleTypeData {
        declaration: TypeDeclarationData {
          name: TypeName(name), parameters, definition, span
        },
        type_identity,
        visible_names,
        available_names
      }), remaining) -> match (equal wanted_name name) {
      | false -> find_visible_type_visibility wanted_name remaining
      | true -> match (contains_text name visible_names) {
        | true -> Some(VisibleType(VisibleTypeData {
          declaration: TypeDeclarationData {
            name: TypeName(name), parameters: parameters, definition: definition,
            span: span
          },
          type_identity: type_identity,
          visible_names: visible_names,
          available_names: available_names
        }))
        | false -> find_visible_type_visibility wanted_name remaining
      }
    }
  }

let find_available_type_visibility = fn wanted_name -> fn visible_types ->
  match visible_types {
    | Nil -> None
    | Cons(VisibleType(VisibleTypeData {
        declaration: TypeDeclarationData {
          name: TypeName(name), parameters, definition, span
        },
        type_identity,
        visible_names,
        available_names
      }), remaining) -> match (equal wanted_name name) {
      | false -> find_available_type_visibility wanted_name remaining
      | true -> match (contains_text name available_names) {
        | true -> Some(VisibleType(VisibleTypeData {
          declaration: TypeDeclarationData {
            name: TypeName(name), parameters: parameters, definition: definition,
            span: span
          },
          type_identity: type_identity,
          visible_names: visible_names,
          available_names: available_names
        }))
        | false -> find_available_type_visibility wanted_name remaining
      }
    }
  }

let find_type_visibility = fn wanted_name -> fn visible_types ->
  match (find_visible_type_visibility wanted_name visible_types) {
    | Some(visible_type) -> Some(visible_type)
    | None -> find_available_type_visibility wanted_name visible_types
  }

let prepend_type_parameter = fn name -> fn index -> fn (TypedParameters(TypeParameterState {
  bindings,
  generic_variables,
  next_index
})) -> TypedParameters(TypeParameterState {
  bindings: Cons(TypeParameterBinding {
    name: name,
    inferred_type: GenericVariable(index)
  }, bindings),
  generic_variables: Cons(index, generic_variables),
  next_index: next_index
})

let type_parameter_bindings = fn (parameters: List<TypeVariableName>) -> fn (index: i32) ->
  match parameters {
    | Nil -> TypedParameters(TypeParameterState {
      bindings: Nil,
      generic_variables: Nil,
      next_index: index
    })
    | Cons(TypeVariableName(name), remaining) -> prepend_type_parameter name index
      (type_parameter_bindings remaining (index + 1))
  }

let lookup_type_parameter = fn wanted_name -> fn bindings ->
  match bindings {
    | Nil -> None
    | Cons(TypeParameterBinding { name, inferred_type }, remaining) ->
      match (equal wanted_name name) {
        | true -> Some(inferred_type)
        | false -> lookup_type_parameter wanted_name remaining
      }
  }

let convert_type_list = fn types -> fn variables -> fn visible_types ->
  fn aliases ->
  match types {
    | Nil -> Ok(Nil)
    | Cons(type_expr, remaining) -> match (convert_type_expression
        type_expr variables visible_types aliases) {
      | Err(message) -> Err(message)
      | Ok(first_type) -> match (convert_type_list
          remaining variables visible_types aliases) {
        | Err(message) -> Err(message)
        | Ok(rest) -> Ok(Cons(first_type, rest))
      }
    }
  }

let convert_record_type_fields = fn fields -> fn variables -> fn visible_types ->
  fn aliases ->
  match fields {
    | Nil -> Ok(Nil)
    | Cons(RecordTypeField { name, field_type }, remaining) ->
      match (convert_type_expression field_type variables visible_types aliases) {
        | Err(message) -> Err(message)
        | Ok(converted_type) -> match (convert_record_type_fields
            remaining variables visible_types aliases) {
          | Err(message) -> Err(message)
          | Ok(rest) -> Ok(Cons(InferredRecordField {
            name: name,
            field_type: converted_type
          }, rest))
        }
      }
  }

let builtin_type_name = fn name ->
  match name {
    | "i8" -> true
    | "i16" -> true
    | "i32" -> true
    | "i64" -> true
    | "u8" -> true
    | "u16" -> true
    | "u32" -> true
    | "u64" -> true
    | "bool" -> true
    | "string" -> true
    | "unit" -> true
    | _ -> false
  }

let builtin_inferred_type = fn name ->
  AppliedType(InferredNamedType { name: name, arguments: Nil })

let type_argument_bindings = fn parameters -> fn arguments ->
  match parameters {
    | Nil -> match arguments {
      | Nil -> Ok(Nil)
      | Cons(_, _) -> Err("too many type arguments")
    }
    | Cons(TypeVariableName(name), remaining_parameters) -> match arguments {
      | Nil -> Err("too few type arguments")
      | Cons(argument, remaining_arguments) -> match (type_argument_bindings
          remaining_parameters remaining_arguments) {
        | Err(message) -> Err(message)
        | Ok(rest) -> Ok(Cons(TypeParameterBinding {
          name: name,
          inferred_type: argument
        }, rest))
      }
    }
  }

let type_variable_names = fn bindings ->
  match bindings {
    | Nil -> Nil
    | Cons(TypeParameterBinding { name, inferred_type: _ }, remaining) ->
      Cons(name, type_variable_names remaining)
  }

let record_field_types = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(InferredRecordField { name: _, field_type }, remaining) ->
      Cons(field_type, record_field_types remaining)
  }

let convert_variant_type_cases = fn variants -> fn variables -> fn visible_types ->
  fn aliases ->
  match variants {
    | Nil -> Ok(Nil)
    | Cons(TupleVariant(TupleVariantData { constructor_name, fields }), remaining) ->
      match (convert_type_list fields variables visible_types aliases) {
        | Err(message) -> Err(message)
        | Ok(converted_fields) -> match (convert_variant_type_cases
            remaining variables visible_types aliases) {
          | Err(message) -> Err(message)
          | Ok(rest) -> Ok(Cons(InferredVariantCase {
            name: constructor_name,
            fields: converted_fields
          }, rest))
        }
      }
    | Cons(RecordVariant(RecordVariantData { constructor_name, fields }), remaining) ->
      match (convert_record_type_fields fields variables visible_types aliases) {
        | Err(message) -> Err(message)
        | Ok(converted_record_fields) -> match (convert_variant_type_cases
            remaining variables visible_types aliases) {
          | Err(message) -> Err(message)
          | Ok(rest) -> Ok(Cons(InferredVariantCase {
            name: constructor_name,
            fields: record_field_types converted_record_fields
          }, rest))
        }
      }
  }

let convert_type_expression = fn type_expression -> fn variables -> fn visible_types ->
  fn aliases ->
  match type_expression {
    | NamedType(NamedTypeExpression { name: TypeName(name), arguments }) ->
      match (builtin_type_name name) {
        | true -> match arguments {
          | Nil -> Ok(builtin_inferred_type name)
          | Cons(_, _) -> Err("primitive type `" + name + "` takes no arguments")
        }
        | false -> match (find_type_visibility name visible_types) {
          | None -> Err("unbound type name `" + name + "`")
          | Some(VisibleType(VisibleTypeData {
              declaration: TypeDeclarationData { name: _, parameters, definition },
              type_identity: type_identity,
              visible_names: _,
              available_names: _
            })) -> match (convert_type_list arguments variables visible_types aliases) {
            | Err(message) -> Err(message)
            | Ok(converted_arguments) -> match (type_argument_bindings
                parameters converted_arguments) {
              | Err(message) -> Err("type `" + name + "`: " + message)
              | Ok(argument_bindings) -> match definition {
                | TypeAlias(alias_body) -> match (contains_text type_identity aliases) {
                  | true -> Err("recursive type alias `" + name + "`")
                  | false -> convert_type_expression alias_body
                    (append_type_parameter_bindings argument_bindings variables)
                    visible_types (Cons(type_identity, aliases))
                }
                | _ -> Ok(AppliedType(InferredNamedType {
                  name: type_identity,
                  arguments: converted_arguments
                }))
              }
            }
          }
        }
      }
    | ArrowType(ArrowTypeExpression { argument, result }) ->
      match (convert_type_expression argument variables visible_types aliases) {
        | Err(message) -> Err(message)
        | Ok(converted_argument) -> match (convert_type_expression
            result variables visible_types aliases) {
          | Err(message) -> Err(message)
          | Ok(converted_result) -> Ok(FunctionType(InferredFunctionType {
            argument: converted_argument,
            result: converted_result
          }))
        }
      }
    | TupleType(items) -> match (convert_type_list items variables visible_types aliases) {
      | Err(message) -> Err(message)
      | Ok(converted_items) -> Ok(InferredTuple(converted_items))
    }
    | RecordType(fields) -> match (convert_record_type_fields fields variables visible_types aliases) {
      | Err(message) -> Err(message)
      | Ok(converted_fields) -> Ok(InferredRecord(converted_fields))
    }
    | VariantType(variants) -> match (convert_variant_type_cases
        variants variables visible_types aliases) {
      | Err(message) -> Err(message)
      | Ok(cases) -> Ok(InferredVariant(cases))
    }
    | TypeVariable(TypeVariableName(name)) -> match (lookup_type_parameter name variables) {
      | None -> Err("unbound type variable `" + name + "`")
      | Some(inferred_type) -> Ok(inferred_type)
    }
  }

let append_type_parameter_bindings = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(binding, remaining) -> Cons(binding, append_type_parameter_bindings remaining second)
  }

let inferred_type_from_arguments = fn arguments -> fn result_type ->
  match arguments {
    | Nil -> result_type
    | Cons(argument, remaining) -> FunctionType(InferredFunctionType {
      argument: argument,
      result: inferred_type_from_arguments remaining result_type
    })
  }

let type_parameter_generic_types = fn bindings ->
  match bindings {
    | Nil -> Nil
    | Cons(TypeParameterBinding { name: _, inferred_type }, remaining) ->
      Cons(inferred_type, type_parameter_generic_types remaining)
  }

let constructor_scheme = fn constructor_name -> fn parameters -> fn argument_types ->
  fn result_type -> fn field_names -> {
  let parameter_info = type_parameter_bindings parameters 0;
  match parameter_info {
    | TypedParameters(TypeParameterState { bindings: _, generic_variables, next_index: _ }) -> InferredConstructor {
      name: constructor_name,
      scheme: TypeScheme {
        quantified_variables: generic_variables,
        body: inferred_type_from_arguments argument_types result_type
      },
      field_names: field_names
    }
  }
}

let constructor_from_tuple_variant = fn wanted_name -> fn parameters ->
  fn scope -> fn result_type -> fn visible_types ->
  fn (TupleVariantData { constructor_name, fields }) ->
  match (equal wanted_name (constructor_name_text constructor_name)) {
    | false -> Err("constructor not found")
    | true -> match (convert_type_list fields scope visible_types Nil) {
      | Err(message) -> Err(message)
      | Ok(argument_types) -> Ok(constructor_scheme constructor_name parameters
        argument_types result_type Nil)
    }
  }

let constructor_from_record_variant = fn wanted_name -> fn parameters ->
  fn scope -> fn result_type -> fn visible_types ->
  fn (RecordVariantData { constructor_name, fields }) ->
  match (equal wanted_name (constructor_name_text constructor_name)) {
    | false -> Err("constructor not found")
    | true -> match (convert_record_type_fields fields scope visible_types Nil) {
      | Err(message) -> Err(message)
      | Ok(record_fields) -> Ok(constructor_scheme constructor_name parameters
        (record_field_types record_fields) result_type (record_type_field_values fields))
    }
  }

let record_type_field_values = fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordTypeField { name, field_type: _ }, remaining) ->
      Cons(name, record_type_field_values remaining)
  }

let find_variant_constructor = fn wanted_name -> fn variants -> fn parameters ->
  fn scope -> fn result_type -> fn visible_types ->
  match variants {
    | Nil -> Err("constructor not found")
    | Cons(TupleVariant(tuple_variant), remaining) -> match (constructor_from_tuple_variant
        wanted_name parameters scope result_type visible_types tuple_variant) {
      | Ok(constructor) -> Ok(constructor)
      | Err(_) -> find_variant_constructor wanted_name remaining parameters scope result_type visible_types
    }
    | Cons(RecordVariant(record_variant), remaining) -> match (constructor_from_record_variant
        wanted_name parameters scope result_type visible_types record_variant) {
      | Ok(constructor) -> Ok(constructor)
      | Err(_) -> find_variant_constructor wanted_name remaining parameters scope result_type visible_types
    }
  }

let find_constructor_in_type = fn wanted_name -> fn visible_types -> fn type_visibility ->
  match type_visibility {
    | VisibleType(VisibleTypeData {
        declaration: TypeDeclarationData { name: TypeName(type_name), parameters, definition },
        type_identity,
        visible_names,
        available_names: _
      }) -> match (contains_text wanted_name visible_names) {
      | false -> Err("constructor `" + wanted_name + "` is not imported")
      | true -> {
        let parameter_info = type_parameter_bindings parameters 0;
        match parameter_info {
          | TypedParameters(TypeParameterState { bindings: scope, generic_variables: _, next_index: _ }) -> {
            let result_type = AppliedType(InferredNamedType {
              name: type_identity,
              arguments: type_parameter_generic_types scope
            });
            match definition {
              | TypeAlias(_) -> Err("constructor not found")
              | TypeRecord(fields) -> match (equal wanted_name type_name) {
                | false -> Err("constructor not found")
                | true -> match (convert_record_type_fields fields scope visible_types Nil) {
                  | Err(message) -> Err(message)
                  | Ok(converted_fields) -> Ok(constructor_scheme
                    (ConstructorName(type_name)) parameters
                    (record_field_types converted_fields) result_type
                    (record_type_field_values fields))
                }
              }
              | TypeVariants(variants) -> find_variant_constructor wanted_name variants
                parameters scope result_type visible_types
            }
          }
        }
      }
    }
  }

let find_constructor_in_types = fn wanted_name -> fn visible_types -> fn types ->
  match types {
    | Nil -> Err("unknown constructor `" + wanted_name + "`")
    | Cons(type_visibility, remaining) -> match (find_constructor_in_type
        wanted_name visible_types type_visibility) {
      | Ok(constructor) -> Ok(constructor)
      | Err(_) -> find_constructor_in_types wanted_name visible_types remaining
    }
  }

let text_list_summary = fn names ->
  match names {
    | Nil -> ""
    | Cons(name, Nil) -> name
    | Cons(name, remaining) -> name + ", " + text_list_summary remaining
  }

let visible_type_summary = fn visible_types ->
  match visible_types {
    | Nil -> ""
    | Cons(VisibleType(VisibleTypeData {
        declaration: TypeDeclarationData { name: TypeName(name), parameters: _, definition: _ },
        type_identity, visible_names, available_names: _
      }), remaining) -> {
      let rest = visible_type_summary remaining;
      let current = name + "=" + text_list_summary visible_names;
      match (equal rest "") {
        | true -> current
        | false -> current + "; " + rest
      }
    }
  }

let find_constructor = fn wanted_name -> fn visible_types ->
  match (find_constructor_in_types wanted_name visible_types visible_types) {
    | Err(message) -> Err(message + " (visible types: " +
      visible_type_summary visible_types + ")")
    | Ok(constructor) -> Ok(constructor)
  }

let module_identity = fn (LoadedModule { module_path: _, source_path, source_module: _ }) ->
  source_path

let type_identity_for_module = fn module_name -> fn (TypeDeclarationData {
  name: TypeName(name), parameters: _, definition: _
}) -> module_name + "::" + name

let local_type_visibilities = fn module_name -> fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(TypeDeclaration(declaration), remaining) -> Cons(
      VisibleType(VisibleTypeData {
        declaration: declaration,
        type_identity: type_identity_for_module module_name declaration,
        visible_names: type_declaration_export_names declaration,
        available_names: type_declaration_export_names declaration
      }),
      local_type_visibilities module_name remaining)
    | Cons(ValueDeclaration(_), remaining) ->
      local_type_visibilities module_name remaining
  }

let same_module_path_segments = fn first -> fn second ->
  match first {
    | Nil -> match second {
      | Nil -> true
      | _ -> false
    }
    | Cons(ModuleName(name), remaining) -> match second {
      | Nil -> false
      | Cons(ModuleName(other_name), other_remaining) ->
        match (equal name other_name) {
          | false -> false
          | true -> same_module_path_segments remaining other_remaining
        }
    }
  }

let same_module_path = fn first -> fn second ->
  match first {
    | ModulePath(first_components) -> match second {
      | ModulePath(second_components) ->
        same_module_path_segments first_components second_components
    }
  }

let lookup_checked_module = fn wanted_path -> fn checked_modules ->
  match checked_modules {
    | Nil -> None
    | Cons(CheckedModuleInterface {
        loaded_module: LoadedModule {
          module_path: Some(module_path), source_path, source_module: _
        },
        interface,
        typed_module
      }, remaining) -> match (same_module_path wanted_path module_path) {
      | true -> Some(CheckedModuleInterface {
        loaded_module: LoadedModule {
          module_path: Some(module_path),
          source_path: source_path,
          source_module: SourceModule { imports: Nil, declarations: Nil }
        },
        interface: interface,
        typed_module: typed_module
      })
      | false -> lookup_checked_module wanted_path remaining
    }
    | Cons(CheckedModuleInterface {
        loaded_module: LoadedModule {
          module_path: None, source_path: _, source_module: _
        },
        interface: _
      }, remaining) -> lookup_checked_module wanted_path remaining
  }

let reverse_value_schemes = fn (values: List<ValueScheme>) ->
  fold (fn (reversed: List<ValueScheme>) -> fn (value: ValueScheme) ->
    Cons(value, reversed)) (Nil: List<ValueScheme>) values

let append_value_schemes = fn (first: List<ValueScheme>) ->
  fn (second: List<ValueScheme>) ->
  fold (fn (appended: List<ValueScheme>) -> fn (value: ValueScheme) ->
    Cons(value, appended)) second (reverse_value_schemes first)

let select_imported_values = fn names -> fn available_values ->
  match names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      match (lookup_value_scheme name available_values) {
        | None -> select_imported_values remaining available_values
        | Some(scheme) -> Cons(SchemeBinding(ValueName(name), scheme),
          select_imported_values remaining available_values)
      }
  }

let imported_values_for_imports = fn (imports: List<Import>) ->
  fn (checked_modules: List<CheckedModuleInterface>) ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path, selected_names }, remaining) -> {
      let imported = match (lookup_checked_module module_path checked_modules) {
        | None -> Nil
        | Some(CheckedModuleInterface { loaded_module: _, interface }) ->
          select_imported_values selected_names (interface_values interface)
      };
      append_value_schemes imported
        (imported_values_for_imports remaining checked_modules)
    }
  }

let interface_values = fn (InferredModuleInterface { values, types: _ }) -> values
let interface_types = fn (InferredModuleInterface { values: _, types }) -> types

let binding_name = fn (BindingIdentity { source_path: _, declaration_span: _, name, scope: _ }) -> name

let binding_environment_entry = fn binding -> BindingEnvironmentEntry {
  name: binding_name binding,
  binding: binding
}

let append_binding_environments = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(entry, remaining) ->
      Cons(entry, append_binding_environments remaining second)
  }

let module_binding_identities = fn module_path -> fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(ValueDeclarationData {
        name, body: _, span
      }), remaining) -> Cons(BindingIdentity {
        source_path: module_path,
        declaration_span: span,
        name: name,
        scope: ModuleValueBinding
      }, module_binding_identities module_path remaining)
    | Cons(TypeDeclaration(_), remaining) ->
      module_binding_identities module_path remaining
  }

let binding_environment_for = fn bindings ->
  match bindings {
    | Nil -> Nil
    | Cons(binding, remaining) -> Cons(binding_environment_entry binding,
      binding_environment_for remaining)
  }

let find_module_binding = fn wanted_name -> fn bindings ->
  match bindings {
    | Nil -> None
    | Cons(BindingIdentity {
        name: ValueName(name), scope, source_path, declaration_span
      }, remaining) -> match (equal name wanted_name) {
        | false -> find_module_binding wanted_name remaining
        | true -> match scope {
          | ModuleValueBinding -> Some(BindingIdentity {
            source_path: source_path,
            declaration_span: declaration_span,
            name: ValueName(name),
            scope: ModuleValueBinding
          })
          | LocalValueBinding -> find_module_binding wanted_name remaining
        }
      }
  }

let select_imported_bindings = fn selected_names -> fn bindings ->
  match selected_names {
    | Nil -> Nil
    | Cons(ImportedName(MemberName(name)), remaining) ->
      match (find_module_binding name bindings) {
        | None -> select_imported_bindings remaining bindings
        | Some(binding) -> Cons(binding_environment_entry binding,
          select_imported_bindings remaining bindings)
      }
  }

let imported_binding_environment = fn imports -> fn checked_modules ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path, selected_names }, remaining) -> {
      let selected_bindings = match (lookup_checked_module module_path checked_modules) {
        | None -> Nil
        | Some(CheckedModuleInterface {
            loaded_module: _, interface: _,
            typed_module: TypedModule {
              source_module: _, expression_types: _, pattern_types: _,
              nominal_types: _, substitutions: _, bindings, value_references: _,
              constructor_references: _
            }
          }) -> select_imported_bindings selected_names bindings
      };
      append_binding_environments selected_bindings
        (imported_binding_environment remaining checked_modules)
    }
  }

let lookup_binding_identity = fn wanted_name -> fn environment ->
  match environment {
    | Nil -> None
    | Cons(BindingEnvironmentEntry { name: ValueName(name), binding }, remaining) ->
      match (equal wanted_name name) {
        | true -> Some(binding)
        | false -> lookup_binding_identity wanted_name remaining
      }
  }

let prepend_binding_identity = fn binding -> fn (BindingResolution {
  bindings, value_references, constructor_references
}) -> BindingResolution {
  bindings: Cons(binding, bindings),
  value_references: value_references,
  constructor_references: constructor_references
}

let prepend_value_reference = fn reference -> fn (BindingResolution {
  bindings, value_references, constructor_references
}) -> BindingResolution {
  bindings: bindings,
  value_references: Cons(reference, value_references),
  constructor_references: constructor_references
}

let prepend_constructor_reference = fn reference -> fn (BindingResolution {
  bindings, value_references, constructor_references
}) -> BindingResolution {
  bindings: bindings,
  value_references: value_references,
  constructor_references: Cons(reference, constructor_references)
}

let constructor_result_type = fn inferred_type ->
  match inferred_type {
    | FunctionType(InferredFunctionType { argument: _, result }) ->
      constructor_result_type result
    | _ -> inferred_type
  }

let constructor_identity_for = fn wanted_name -> fn visible_types ->
  match (find_constructor wanted_name visible_types) {
    | Err(message) -> Err(message)
    | Ok(InferredConstructor {
        name,
        scheme: TypeScheme { quantified_variables: _, body },
        field_names: _
      }) -> {
      let result_type = constructor_result_type body;
      match result_type {
        | AppliedType(InferredNamedType { name: type_identity, arguments: _ }) ->
          Ok(ConstructorIdentity {
            type_identity: type_identity,
            constructor_name: name
          })
        | _ -> Err("constructor `" + wanted_name
          + "` does not resolve to a nominal result type")
      }
    }
  }

let resolve_pattern_list = fn patterns -> fn module_path -> fn visible_types ->
  fn state -> match patterns {
    | Nil -> Ok(PatternResolution { environment: Nil, state: state })
    | Cons(pattern, remaining) -> match (resolve_pattern pattern module_path
        visible_types state) {
      | Err(message) -> Err(message)
      | Ok(PatternResolution { environment: first_environment, state: next_state }) ->
        match (resolve_pattern_list remaining module_path visible_types next_state) {
          | Err(message) -> Err(message)
          | Ok(PatternResolution { environment: remaining_environment, state: final_state }) ->
            Ok(PatternResolution {
              environment: append_binding_environments first_environment remaining_environment,
              state: final_state
            })
        }
    }
  }

let resolve_record_pattern_fields = fn fields -> fn module_path -> fn visible_types ->
  fn state -> match fields {
    | Nil -> Ok(PatternResolution { environment: Nil, state: state })
    | Cons(RecordPatternField { name: _, pattern }, remaining) ->
      match (resolve_pattern pattern module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(PatternResolution { environment: first_environment, state: next_state }) ->
          match (resolve_record_pattern_fields remaining module_path visible_types next_state) {
            | Err(message) -> Err(message)
            | Ok(PatternResolution {
                environment: remaining_environment, state: final_state
              }) -> Ok(PatternResolution {
                environment: append_binding_environments first_environment remaining_environment,
                state: final_state
              })
          }
      }
  }

let resolve_constructor_pattern = fn pattern -> fn constructor_name -> fn arguments ->
  fn module_path -> fn visible_types -> fn state ->
  match (constructor_identity_for constructor_name visible_types) {
    | Err(message) -> Err("TypedAST constructor resolution: " + message)
    | Ok(entity) -> {
      let with_constructor = prepend_constructor_reference
        (ResolvedConstructorReference {
          reference_span: pattern_span pattern,
          entity: entity
        }) state;
      resolve_pattern_list arguments module_path visible_types with_constructor
    }
  }

let resolve_pattern = fn pattern -> fn module_path -> fn visible_types -> fn state ->
  match (pattern_kind pattern) {
    | Variable(name) -> {
      let binding = BindingIdentity {
        source_path: module_path,
        declaration_span: pattern_span pattern,
        name: name,
        scope: LocalValueBinding
      };
      Ok(PatternResolution {
        environment: Cons(binding_environment_entry binding, Nil),
        state: prepend_binding_identity binding state
      })
    }
    | PatternUnit -> Ok(PatternResolution { environment: Nil, state: state })
    | Wildcard -> Ok(PatternResolution { environment: Nil, state: state })
    | PatternInteger(_) -> Ok(PatternResolution { environment: Nil, state: state })
    | PatternBoolean(_) -> Ok(PatternResolution { environment: Nil, state: state })
    | PatternString(_) -> Ok(PatternResolution { environment: Nil, state: state })
    | TuplePattern(patterns) -> resolve_pattern_list patterns
      module_path visible_types state
    | ConstructorPattern(ConstructorPatternData { constructor_name: ConstructorName(name), arguments }) ->
      resolve_constructor_pattern pattern name arguments module_path visible_types state
    | RecordPattern(RecordPatternData {
        constructor_name: ConstructorName(name), fields
      }) -> match (resolve_constructor_pattern pattern name Nil
          module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(PatternResolution { environment: _, state: constructor_state }) ->
          resolve_record_pattern_fields fields module_path visible_types constructor_state
      }
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      resolve_pattern inner module_path visible_types state
  }

let resolve_expression_list = fn expressions -> fn environment -> fn module_path ->
  fn visible_types -> fn state -> match expressions {
    | Nil -> Ok(state)
    | Cons(expression, remaining) -> match (resolve_expression expression environment
        module_path visible_types state) {
      | Err(message) -> Err(message)
      | Ok(next_state) -> resolve_expression_list remaining environment
        module_path visible_types next_state
    }
  }

let resolve_record_expression_fields = fn fields -> fn environment -> fn module_path ->
  fn visible_types -> fn state -> match fields {
    | Nil -> Ok(state)
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      match (resolve_expression value environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_record_expression_fields remaining environment
          module_path visible_types next_state
      }
  }

let resolve_match_arms = fn arms -> fn environment -> fn module_path ->
  fn visible_types -> fn state -> match arms {
    | Nil -> Ok(state)
    | Cons(MatchArm { pattern, body }, remaining) ->
      match (resolve_pattern pattern module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(PatternResolution { environment: arm_bindings, state: next_state }) ->
          match (resolve_expression body
              (append_binding_environments arm_bindings environment)
              module_path visible_types next_state) {
            | Err(message) -> Err(message)
            | Ok(arm_state) -> resolve_match_arms remaining environment
              module_path visible_types arm_state
          }
      }
  }

let resolve_constructor_expression = fn expression -> fn constructor_name ->
  fn visible_types -> fn state ->
  match (constructor_identity_for constructor_name visible_types) {
    | Err(message) -> Err("TypedAST constructor resolution: " + message)
    | Ok(entity) -> Ok(prepend_constructor_reference (ResolvedConstructorReference {
      reference_span: expression_span expression,
      entity: entity
    }) state)
  }

let resolve_expression = fn expression -> fn environment -> fn module_path ->
  fn visible_types -> fn state -> match (expression_kind expression) {
    | Integer(_) -> Ok(state)
    | Boolean(_) -> Ok(state)
    | StringLiteral(_) -> Ok(state)
    | Unit -> Ok(state)
    | VariableExpr(ValueName(name)) -> match (lookup_binding_identity name environment) {
      | None -> Err("TypedAST unresolved value reference `" + name + "`")
      | Some(binding) -> Ok(prepend_value_reference (ResolvedValueReference {
        reference_span: expression_span expression,
        binding: binding
      }) state)
    }
    | TupleExpr(items) -> resolve_expression_list items environment
      module_path visible_types state
    | RecordExpr(RecordExpression {
        constructor_name: ConstructorName(name), fields
      }) -> match (resolve_constructor_expression expression name visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_record_expression_fields fields environment
          module_path visible_types next_state
      }
    | Binary(BinaryExpression { operator: _, left, right }) ->
      match (resolve_expression left environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_expression right environment module_path
          visible_types next_state
      }
    | Negate(operand) -> resolve_expression operand environment module_path
      visible_types state
    | Function(FunctionExpression { parameter, body }) ->
      match (resolve_pattern parameter module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(PatternResolution { environment: parameter_bindings, state: next_state }) ->
          resolve_expression body (append_binding_environments parameter_bindings environment)
            module_path visible_types next_state
      }
    | Apply(ApplicationExpression { callee, argument }) ->
      match (resolve_expression callee environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_expression argument environment module_path
          visible_types next_state
      }
    | Let(LetExpression { pattern, annotation: _, value, body }) ->
      match (resolve_expression value environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(value_state) -> match (resolve_pattern pattern module_path visible_types value_state) {
          | Err(message) -> Err(message)
          | Ok(PatternResolution { environment: local_bindings, state: pattern_state }) ->
            resolve_expression body (append_binding_environments local_bindings environment)
              module_path visible_types pattern_state
        }
      }
    | Match(MatchExpression { scrutinee, arms }) ->
      match (resolve_expression scrutinee environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_match_arms arms environment module_path
          visible_types next_state
      }
    | ConstructorExpr(ConstructorExpression {
        constructor_name: ConstructorName(name), arguments
      }) -> match (resolve_constructor_expression expression name visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_expression_list arguments environment
          module_path visible_types next_state
      }
    | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
      resolve_expression inner environment module_path visible_types state
    | Block(items) -> resolve_expression_list items environment
      module_path visible_types state
  }

let resolve_module_value_bodies = fn declarations -> fn environment -> fn module_path ->
  fn visible_types -> fn state -> match declarations {
    | Nil -> Ok(state)
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body, span: _ }), remaining) ->
      match (resolve_expression body environment module_path visible_types state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> resolve_module_value_bodies remaining environment
          module_path visible_types next_state
      }
    | Cons(TypeDeclaration(_), remaining) -> resolve_module_value_bodies remaining
      environment module_path visible_types state
  }

let resolve_module_bindings = fn module_path -> fn imports -> fn declarations ->
  fn checked_modules -> fn visible_types -> {
  let module_bindings = module_binding_identities module_path declarations;
  let imported_bindings = imported_binding_environment imports checked_modules;
  let module_environment = append_binding_environments
    (binding_environment_for module_bindings) imported_bindings;
  let initial_state = BindingResolution {
    bindings: module_bindings,
    value_references: Nil,
    constructor_references: Nil
  };
  resolve_module_value_bodies declarations module_environment module_path
    visible_types initial_state
}

let selected_export_names = fn exported_names -> fn selected_names ->
  match selected_names {
    | Nil -> Nil
    | Cons(name, remaining) -> match (contains_text name exported_names) {
      | true -> Cons(name, selected_export_names exported_names remaining)
      | false -> selected_export_names exported_names remaining
    }
  }

let imported_type_visibilities_for_declarations = fn types -> fn selected_names ->
  match types {
    | Nil -> Nil
    | Cons(VisibleType(VisibleTypeData {
        declaration, type_identity, visible_names, available_names
      }), remaining) -> {
      let selected = selected_export_names visible_names selected_names;
      Cons(VisibleType(VisibleTypeData {
        declaration: declaration,
        type_identity: type_identity,
        visible_names: selected,
        available_names: available_names
      }), imported_type_visibilities_for_declarations remaining selected_names)
    }
  }

let imported_types_for_imports = fn (imports: List<Import>) ->
  fn (checked_modules: List<CheckedModuleInterface>) ->
  match imports {
    | Nil -> Nil
    | Cons(Import { module_path, selected_names }, remaining) -> {
      let selected_names_text = imported_member_names selected_names;
      let imported = match (lookup_checked_module module_path checked_modules) {
        | None -> Nil
        | Some(CheckedModuleInterface { loaded_module: _, interface }) ->
          imported_type_visibilities_for_declarations
            (interface_types interface) selected_names_text
      };
      append_type_visibilities imported
        (imported_types_for_imports remaining checked_modules)
    }
  }

let type_visibility_identity = fn (VisibleType(VisibleTypeData {
  declaration: _, type_identity, visible_names: _, available_names: _
})) -> type_identity

let contains_type_identity = fn wanted_identity -> fn visible_types ->
  match visible_types {
    | Nil -> false
    | Cons(visibility, remaining) -> match (equal
        wanted_identity (type_visibility_identity visibility)) {
      | true -> true
      | false -> contains_type_identity wanted_identity remaining
    }
  }

let merge_type_visibility = fn (VisibleType(VisibleTypeData {
  declaration, type_identity, visible_names, available_names
})) -> fn visible_types ->
  match visible_types {
    | Nil -> Cons(VisibleType(VisibleTypeData {
      declaration: declaration,
      type_identity: type_identity,
      visible_names: visible_names,
      available_names: available_names
    }), Nil)
    | Cons(VisibleType(VisibleTypeData {
        declaration: existing_declaration,
        type_identity: existing_identity,
        visible_names: existing_names,
        available_names: existing_available_names
      }), remaining) -> match (equal type_identity existing_identity) {
      | true -> Cons(VisibleType(VisibleTypeData {
        declaration: existing_declaration,
        type_identity: existing_identity,
        visible_names: append_texts visible_names existing_names,
        available_names: existing_available_names
      }), remaining)
      | false -> Cons(VisibleType(VisibleTypeData {
        declaration: existing_declaration,
        type_identity: existing_identity,
        visible_names: existing_names,
        available_names: existing_available_names
      }), merge_type_visibility (VisibleType(VisibleTypeData {
        declaration: declaration,
        type_identity: type_identity,
        visible_names: visible_names,
        available_names: available_names
      })) remaining)
    }
  }

let append_type_visibilities = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(visibility, remaining) -> {
      let appended = append_type_visibilities remaining second;
      match (contains_type_identity (type_visibility_identity visibility) appended) {
        | true -> merge_type_visibility visibility appended
        | false -> Cons(visibility, appended)
      }
    }
  }

let lookup_value_scheme = fn wanted_name -> fn values ->
  match values {
    | Nil -> None
    | Cons(SchemeBinding(ValueName(name), scheme), remaining) ->
      match (equal wanted_name name) {
        | true -> Some(scheme)
        | false -> lookup_value_scheme wanted_name remaining
      }
  }

let replace_value_scheme = fn wanted_name -> fn replacement -> fn values ->
  match values {
    | Nil -> Cons(replacement, Nil)
    | Cons(SchemeBinding(ValueName(name), scheme), remaining) ->
      match (equal wanted_name name) {
        | true -> Cons(replacement, remaining)
        | false -> Cons(SchemeBinding(ValueName(name), scheme),
          replace_value_scheme wanted_name replacement remaining)
      }
  }

let remove_value_scheme = fn wanted_name -> fn values ->
  match values {
    | Nil -> Nil
    | Cons(SchemeBinding(ValueName(name), scheme), remaining) ->
      match (equal wanted_name name) {
        | true -> remaining
        | false -> Cons(SchemeBinding(ValueName(name), scheme),
          remove_value_scheme wanted_name remaining)
      }
  }

let source_value_declarations = fn declarations ->
  match declarations {
    | Nil -> Nil
    | Cons(ValueDeclaration(declaration), remaining) ->
      Cons(declaration, source_value_declarations remaining)
    | Cons(TypeDeclaration(_), remaining) -> source_value_declarations remaining
  }

let environment_with_bindings = fn new_bindings -> fn environment ->
  append_value_schemes new_bindings environment

let module_state_substitutions = fn (InferenceState {
  next_variable: _, substitutions, expression_annotations: _, pattern_annotations: _
}) ->
  substitutions

let apply_state_substitutions = fn inferred_type -> fn state ->
  apply_substitutions inferred_type (module_state_substitutions state)

let make_inference_result = fn inferred_type -> fn state ->
  InferenceResult { inferred_type: inferred_type, state: state }

let infer_expression_list = fn infer_child -> fn expressions -> fn environment -> fn visible_types ->
  fn state ->
  match expressions {
    | Nil -> Ok(InferredTypeList(Nil, state))
    | Cons(expression, remaining) -> match (infer_child expression environment visible_types state) {
      | Err(message) -> Err(message)
      | Ok(InferenceResult { inferred_type, state: next_state }) ->
        match (infer_expression_list infer_child remaining environment visible_types next_state) {
          | Err(message) -> Err(message)
          | Ok(InferredTypeList(rest_types, final_state)) ->
            Ok(InferredTypeList(Cons(inferred_type, rest_types), final_state))
        }
    }
  }

let infer_sequence = fn infer_child -> fn expressions -> fn environment -> fn visible_types ->
  fn state ->
  match expressions {
    | Nil -> Ok(make_inference_result (builtin_inferred_type "unit") state)
    | Cons(expression, Nil) -> infer_child expression environment visible_types state
    | Cons(expression, remaining) -> match (infer_child expression environment visible_types state) {
      | Err(message) -> Err(message)
      | Ok(InferenceResult { inferred_type: _, state: next_state }) ->
        infer_sequence infer_child remaining environment visible_types next_state
    }
  }

let infer_variable_expression = fn (ValueName(name)) -> fn environment -> fn state ->
  match (lookup_value_scheme name environment) {
    | None -> Err("unbound value name `" + name + "`")
    | Some(scheme) -> match (instantiate_scheme scheme state) {
      | Err(message) -> Err(message)
      | Ok(FreshInference(inferred_type, next_state)) ->
        Ok(make_inference_result inferred_type next_state)
    }
  }

let reverse_inference_type_list = fn inferred_types -> fn reversed ->
  match inferred_types {
    | Nil -> reversed
    | Cons(inferred_type, remaining) ->
      reverse_inference_type_list remaining (Cons(inferred_type, reversed))
  }

let split_constructor_type = fn inferred_type -> fn accumulated_arguments ->
  match inferred_type {
    | FunctionType(InferredFunctionType { argument, result }) ->
      split_constructor_type result (Cons(argument, accumulated_arguments))
    | _ -> ConstructorShape(
      reverse_inference_type_list accumulated_arguments Nil,
      inferred_type
    )
  }

let instantiate_constructor = fn wanted_name -> fn visible_types -> fn state ->
  match (find_constructor wanted_name visible_types) {
    | Err(message) -> Err(message)
    | Ok(InferredConstructor { name: _, scheme, field_names }) ->
      match (instantiate_scheme scheme state) {
        | Err(message) -> Err(message)
        | Ok(FreshInference(constructor_type, next_state)) -> match (split_constructor_type
            constructor_type Nil) {
          | ConstructorShape(argument_types, result_type) ->
            Ok(InstantiatedConstructor(argument_types, result_type, next_state))
        }
      }
  }

let constrain_types = fn expected -> fn actual -> fn state ->
  unify_types expected actual state

let append_pattern_bindings = fn first -> fn second ->
  match first {
    | Nil -> second
    | Cons(binding, remaining) -> Cons(binding, append_pattern_bindings remaining second)
  }

let infer_fresh_pattern_types = fn patterns -> fn state ->
  match patterns {
    | Nil -> Ok(InferredTypeList(Nil, state))
    | Cons(_, remaining) -> match (fresh_type_variable state) {
      | FreshInference(inferred_type, next_state) ->
        match (infer_fresh_pattern_types remaining next_state) {
          | Err(message) -> Err(message)
          | Ok(InferredTypeList(rest_types, final_state)) ->
            Ok(InferredTypeList(Cons(inferred_type, rest_types), final_state))
        }
    }
  }

let infer_pattern_list = fn infer_child -> fn patterns -> fn expected_types ->
  fn visible_types -> fn state ->
  match patterns {
    | Nil -> match expected_types {
      | Nil -> Ok(PatternBindings { bindings: Nil, state: state })
      | Cons(_, _) -> Err("constructor pattern has the wrong number of arguments")
    }
    | Cons(pattern, remaining_patterns) -> match expected_types {
      | Nil -> Err("constructor pattern has the wrong number of arguments")
      | Cons(expected_type, remaining_types) -> match (infer_child
          pattern expected_type visible_types state) {
        | Err(message) -> Err(message)
        | Ok(PatternBindings { bindings, state: next_state }) ->
          match (infer_pattern_list infer_child remaining_patterns remaining_types visible_types next_state) {
            | Err(message) -> Err(message)
            | Ok(PatternBindings { bindings: rest, state: final_state }) ->
              Ok(PatternBindings {
                bindings: append_pattern_bindings bindings rest,
                state: final_state
              })
          }
      }
    }
  }

let find_pattern_field_type = fn wanted_name -> fn field_names -> fn field_types ->
  match field_names {
    | Nil -> None
    | Cons(ValueName(name), remaining_names) -> match field_types {
      | Nil -> None
      | Cons(field_type, remaining_types) -> match (equal wanted_name name) {
        | true -> Some(field_type)
        | false -> find_pattern_field_type wanted_name remaining_names remaining_types
      }
    }
  }

let infer_record_pattern_fields = fn infer_child -> fn fields -> fn field_names -> fn field_types ->
  fn visible_types -> fn state ->
  match fields {
    | Nil -> Ok(PatternBindings { bindings: Nil, state: state })
    | Cons(RecordPatternField { name: ValueName(name), pattern }, remaining) ->
      match (find_pattern_field_type name field_names field_types) {
        | None -> Err("record pattern uses an unknown field `" + name + "`")
        | Some(field_type) -> match (infer_child pattern field_type visible_types state) {
          | Err(message) -> Err(message)
          | Ok(PatternBindings { bindings, state: next_state }) ->
            match (infer_record_pattern_fields infer_child remaining field_names field_types
                visible_types next_state) {
              | Err(message) -> Err(message)
              | Ok(PatternBindings { bindings: rest, state: final_state }) ->
                Ok(PatternBindings {
                  bindings: append_pattern_bindings bindings rest,
                  state: final_state
                })
            }
        }
      }
  }

let infer_pattern_node = fn pattern -> fn expected_type -> fn visible_types -> fn state ->
  match (pattern_kind pattern) {
    | Variable(name) -> Ok(PatternBindings {
      bindings: Cons(SchemeBinding(name,
        TypeScheme { quantified_variables: Nil, body: expected_type }
      ), Nil),
      state: state
    })
    | Wildcard -> Ok(PatternBindings { bindings: Nil, state: state })
    | PatternUnit -> match (constrain_types expected_type (builtin_inferred_type "unit") state) {
      | Err(message) -> Err(message)
      | Ok(next_state) -> Ok(PatternBindings { bindings: Nil, state: next_state })
    }
    | PatternInteger(IntegerLiteral(value)) -> match (integer_literal_fits value "i32") {
      | false -> Err("integer pattern is outside i32 range")
      | true -> match (constrain_types expected_type (builtin_inferred_type "i32") state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> Ok(PatternBindings { bindings: Nil, state: next_state })
      }
    }
    | PatternBoolean(_) -> match (constrain_types expected_type (builtin_inferred_type "bool") state) {
      | Err(message) -> Err(message)
      | Ok(next_state) -> Ok(PatternBindings { bindings: Nil, state: next_state })
    }
    | PatternString(_) -> match (constrain_types expected_type (builtin_inferred_type "string") state) {
      | Err(message) -> Err(message)
      | Ok(next_state) -> Ok(PatternBindings { bindings: Nil, state: next_state })
    }
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation }) ->
      match (convert_type_expression annotation Nil visible_types Nil) {
        | Err(message) -> Err(message)
        | Ok(annotation_type) -> match (constrain_types expected_type annotation_type state) {
          | Err(message) -> Err(message)
          | Ok(next_state) -> infer_pattern inner annotation_type visible_types next_state
        }
      }
    | TuplePattern(patterns) -> match (infer_fresh_pattern_types patterns state) {
      | Err(message) -> Err(message)
      | Ok(InferredTypeList(item_types, next_state)) -> match (constrain_types
          expected_type (InferredTuple(item_types)) next_state) {
        | Err(message) -> Err(message)
        | Ok(unified_state) -> infer_pattern_list infer_pattern patterns item_types
          visible_types unified_state
      }
    }
    | ConstructorPattern(ConstructorPatternData { constructor_name: ConstructorName(name), arguments }) ->
      match (instantiate_constructor name visible_types state) {
        | Err(message) -> Err(message)
        | Ok(InstantiatedConstructor(argument_types, result_type, next_state)) ->
          match (constrain_types expected_type result_type next_state) {
            | Err(message) -> Err("constructor pattern `" + name + "`: " + message)
            | Ok(unified_state) -> infer_pattern_list infer_pattern arguments argument_types
              visible_types unified_state
          }
      }
    | RecordPattern(RecordPatternData { constructor_name: ConstructorName(name), fields }) ->
      match (instantiate_constructor name visible_types state) {
        | Err(message) -> Err(message)
        | Ok(InstantiatedConstructor(argument_types, result_type, next_state)) ->
          match (constrain_types expected_type result_type next_state) {
            | Err(message) -> Err("record pattern `" + name + "`: " + message)
            | Ok(unified_state) -> match (find_constructor name visible_types) {
              | Err(message) -> Err(message)
              | Ok(InferredConstructor { name: _, scheme: _, field_names }) ->
                infer_record_pattern_fields infer_pattern fields field_names argument_types
                  visible_types unified_state
            }
          }
      }
  }

let record_pattern_annotation = fn pattern -> fn inferred_type -> fn (InferenceState {
  next_variable, substitutions, expression_annotations, pattern_annotations
}) -> InferenceState {
  next_variable: next_variable,
  substitutions: substitutions,
  expression_annotations: expression_annotations,
  pattern_annotations: Cons(PatternTypeAnnotation {
    span: pattern_span pattern,
    inferred_type: inferred_type
  }, pattern_annotations)
}

let infer_pattern = fn pattern -> fn expected_type -> fn visible_types -> fn state ->
  match (infer_pattern_node pattern expected_type visible_types state) {
    | Err(message) -> Err(message)
    | Ok(PatternBindings { bindings, state: next_state }) -> Ok(PatternBindings {
      bindings: bindings,
      state: record_pattern_annotation pattern
        (apply_state_substitutions expected_type next_state) next_state
    })
  }

let generalize_pattern_bindings = fn bindings -> fn environment -> fn state ->
  match bindings {
    | Nil -> Nil
    | Cons(SchemeBinding(name,
        TypeScheme { quantified_variables: _, body }
      ), remaining) -> {
      let substitutions = module_state_substitutions state;
      let generalized = generalize_type environment body substitutions;
      Cons(SchemeBinding(name, generalized),
        generalize_pattern_bindings remaining environment state)
    }
  }

let infer_addition_types = fn left_type -> fn right_type -> fn state -> {
  let left = apply_state_substitutions left_type state;
  let right = apply_state_substitutions right_type state;
  match left {
    | AppliedType(InferredNamedType { name: "string", arguments: Nil }) ->
      match (constrain_types left right state) {
        | Err(message) -> Err("addition requires two strings or two i32 operands: " + message)
        | Ok(next_state) -> Ok(InferenceResult {
          inferred_type: builtin_inferred_type "string", state: next_state
        })
      }
    | AppliedType(InferredNamedType { name: "i32", arguments: Nil }) ->
      match (constrain_types left right state) {
        | Err(message) -> Err("addition requires two strings or two i32 operands: " + message)
        | Ok(next_state) -> Ok(InferenceResult {
          inferred_type: builtin_inferred_type "i32", state: next_state
        })
      }
    | FreeVariable(_) -> match right {
      | AppliedType(InferredNamedType { name: "string", arguments: Nil }) ->
        match (constrain_types left right state) {
          | Err(message) -> Err(message)
          | Ok(next_state) -> Ok(InferenceResult {
            inferred_type: builtin_inferred_type "string", state: next_state
          })
        }
      | _ -> match (constrain_types left (builtin_inferred_type "i32") state) {
        | Err(message) -> Err(message)
        | Ok(next_state) -> match (constrain_types right (builtin_inferred_type "i32") next_state) {
          | Err(_) -> Err("addition requires two strings or two i32 operands")
          | Ok(final_state) -> Ok(InferenceResult {
            inferred_type: builtin_inferred_type "i32", state: final_state
          })
        }
      }
    }
    | _ -> Err("addition requires two strings or two i32 operands")
  }
}

let infer_binary_types = fn operator -> fn left_type -> fn right_type -> fn state ->
  match operator {
    | Addition -> infer_addition_types left_type right_type state
    | Subtraction -> match (constrain_types left_type (builtin_inferred_type "i32") state) {
      | Err(message) -> Err("integer arithmetic requires i32 values: " + message)
      | Ok(next_state) -> match (constrain_types right_type (builtin_inferred_type "i32") next_state) {
        | Err(message) -> Err("integer arithmetic requires i32 values: " + message)
        | Ok(final_state) -> Ok(InferenceResult {
          inferred_type: builtin_inferred_type "i32", state: final_state
        })
      }
    }
    | Multiplication -> infer_binary_types Subtraction left_type right_type state
    | Division -> infer_binary_types Subtraction left_type right_type state
    | Remainder -> infer_binary_types Subtraction left_type right_type state
    | Equal -> infer_comparison_types left_type right_type state
    | NotEqual -> infer_comparison_types left_type right_type state
    | LessThan -> infer_comparison_types left_type right_type state
    | LessThanOrEqual -> infer_comparison_types left_type right_type state
    | GreaterThan -> infer_comparison_types left_type right_type state
    | GreaterThanOrEqual -> infer_comparison_types left_type right_type state
  }

let infer_comparison_types = fn left_type -> fn right_type -> fn state ->
  match (constrain_types left_type (builtin_inferred_type "i32") state) {
    | Err(message) -> Err("integer comparison requires i32 values: " + message)
    | Ok(next_state) -> match (constrain_types right_type (builtin_inferred_type "i32") next_state) {
      | Err(message) -> Err("integer comparison requires i32 values: " + message)
      | Ok(final_state) -> Ok(InferenceResult {
        inferred_type: builtin_inferred_type "bool", state: final_state
      })
    }
  }

let infer_binary_expression = fn infer_child -> fn (BinaryExpression {
  operator,
  left,
  right
}) -> fn environment -> fn visible_types -> fn state ->
  match (infer_child left environment visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InferenceResult { inferred_type: left_type, state: next_state }) ->
      match (infer_child right environment visible_types next_state) {
        | Err(message) -> Err(message)
        | Ok(InferenceResult { inferred_type: right_type, state: final_state }) ->
          infer_binary_types operator left_type right_type final_state
      }
  }

let infer_function_expression = fn infer_child -> fn (FunctionExpression { parameter, body }) ->
  fn environment -> fn visible_types -> fn state ->
  match (fresh_type_variable state) {
    | FreshInference(argument_type, next_state) -> match (infer_pattern
        parameter argument_type visible_types next_state) {
      | Err(message) -> Err(message)
      | Ok(PatternBindings { bindings, state: pattern_state }) -> {
        let body_environment = environment_with_bindings bindings environment;
        match (infer_child body body_environment visible_types pattern_state) {
          | Err(message) -> Err(message)
          | Ok(InferenceResult { inferred_type: result_type, state: body_state }) ->
            Ok(InferenceResult {
              inferred_type: FunctionType(InferredFunctionType {
                argument: argument_type,
                result: result_type
              }),
              state: body_state
            })
        }
      }
    }
  }

let inferred_type_description = fn inferred_type ->
  match inferred_type {
    | FreeVariable(_) -> "unresolved type variable"
    | GenericVariable(_) -> "generic type variable"
    | AppliedType(InferredNamedType { name, arguments: _ }) -> name
    | FunctionType(InferredFunctionType { argument, result }) ->
      "(" + inferred_type_description argument + " -> " +
        inferred_type_description result + ")"
    | InferredTuple(_) -> "tuple"
    | InferredRecord(_) -> "record"
    | InferredVariant(_) -> "variant"
  }

let infer_application_expression = fn infer_child -> fn (ApplicationExpression {
  callee,
  argument
}) -> fn environment -> fn visible_types -> fn state ->
  match (infer_child callee environment visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InferenceResult { inferred_type: callee_type, state: next_state }) ->
      match (infer_child argument environment visible_types next_state) {
        | Err(message) -> Err(message)
        | Ok(InferenceResult { inferred_type: argument_type, state: argument_state }) ->
          match (fresh_type_variable argument_state) {
            | FreshInference(result_type, fresh_state) -> match (constrain_types
                callee_type (FunctionType(InferredFunctionType {
                  argument: argument_type,
                  result: result_type
                })) fresh_state) {
              | Err(message) -> Err("function application: function argument has an incompatible type: " +
                inferred_type_description callee_type + " applied to " +
                inferred_type_description argument_type + ": " + message)
              | Ok(final_state) -> Ok(InferenceResult {
                inferred_type: apply_state_substitutions result_type final_state,
                state: final_state
              })
            }
          }
      }
  }

let infer_constructor_arguments = fn infer_child -> fn expected_types -> fn expressions ->
  fn environment -> fn visible_types -> fn state -> fn result_type ->
  match expected_types {
    | Nil -> match expressions {
      | Nil -> Ok(make_inference_result (apply_state_substitutions result_type state) state)
      | Cons(_, _) -> Err("constructor application has too many arguments")
    }
    | Cons(expected_type, remaining_types) -> match expressions {
      | Nil -> Err("constructor application has too few arguments")
      | Cons(expression, remaining_expressions) -> match (infer_child
          expression environment visible_types state) {
        | Err(message) -> Err(message)
        | Ok(InferenceResult { inferred_type, state: next_state }) ->
          match (constrain_types expected_type inferred_type next_state) {
            | Err(message) -> Err("constructor argument: " + message)
            | Ok(unified_state) -> infer_constructor_arguments infer_child
              remaining_types remaining_expressions environment visible_types
              unified_state result_type
          }
      }
    }
  }

let infer_constructor_expression = fn infer_child -> fn (ConstructorExpression {
  constructor_name: ConstructorName(name),
  arguments
}) -> fn environment -> fn visible_types -> fn state ->
  match (instantiate_constructor name visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InstantiatedConstructor(argument_types, result_type, next_state)) ->
      infer_constructor_arguments infer_child argument_types arguments
        environment visible_types next_state result_type
  }

let count_record_expression_fields = fn fields ->
  match fields {
    | Nil -> 0
    | Cons(_, remaining) -> 1 + count_record_expression_fields remaining
  }

let record_expression_has_field = fn wanted_name -> fn fields ->
  match fields {
    | Nil -> false
    | Cons(RecordExpressionField { name: ValueName(name), value: _ }, remaining) ->
      match (equal wanted_name name) {
        | true -> true
        | false -> record_expression_has_field wanted_name remaining
      }
  }

let all_record_type_fields_supplied = fn field_names -> fn expression_fields ->
  match field_names {
    | Nil -> count_record_expression_fields expression_fields == 0
    | Cons(ValueName(name), remaining) -> match (record_expression_has_field name expression_fields) {
      | false -> false
      | true -> all_record_type_fields_supplied remaining
        (remove_record_expression_field name expression_fields)
    }
  }

let remove_record_expression_field = fn removed_name -> fn fields ->
  match fields {
    | Nil -> Nil
    | Cons(RecordExpressionField { name: ValueName(name), value }, remaining) ->
      match (equal removed_name name) {
        | true -> remaining
        | false -> Cons(RecordExpressionField {
          name: ValueName(name), value: value
        }, remove_record_expression_field removed_name remaining)
      }
  }

let infer_record_expression_fields = fn infer_child -> fn fields -> fn field_names ->
  fn field_types -> fn environment -> fn visible_types -> fn state ->
  match fields {
    | Nil -> Ok(state)
    | Cons(RecordExpressionField { name: ValueName(name), value }, remaining) ->
      match (find_pattern_field_type name field_names field_types) {
        | None -> Err("record expression uses an unknown field `" + name + "`")
        | Some(field_type) -> match (infer_child value environment visible_types state) {
          | Err(message) -> Err(message)
          | Ok(InferenceResult { inferred_type, state: next_state }) ->
            match (constrain_types field_type inferred_type next_state) {
              | Err(message) -> Err("record field `" + name + "`: " + message)
              | Ok(unified_state) -> infer_record_expression_fields infer_child
                remaining field_names field_types environment visible_types unified_state
            }
        }
      }
  }

let infer_record_expression = fn infer_child -> fn (RecordExpression {
  constructor_name: ConstructorName(name),
  fields
}) -> fn environment -> fn visible_types -> fn state ->
  match (instantiate_constructor name visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InstantiatedConstructor(field_types, result_type, next_state)) ->
      match (find_constructor name visible_types) {
        | Err(message) -> Err(message)
        | Ok(InferredConstructor { name: _, scheme: _, field_names }) ->
          match (all_record_type_fields_supplied field_names fields) {
            | false -> Err("record expression must supply each declared field exactly once")
            | true -> match (infer_record_expression_fields infer_child fields
                field_names field_types environment visible_types next_state) {
              | Err(message) -> Err(message)
              | Ok(final_state) -> Ok(InferenceResult {
                inferred_type: apply_state_substitutions result_type final_state,
                state: final_state
              })
            }
          }
      }
  }

let infer_let_expression = fn infer_child -> fn infer_let_body -> fn (LetExpression {
  pattern,
  annotation,
  value,
  body
}) -> fn environment -> fn visible_types -> fn state ->
  match (infer_child value environment visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InferenceResult { inferred_type: value_type, state: value_state }) ->
      match annotation {
        | None -> infer_let_body infer_child pattern value_type body
          environment visible_types value_state
        | Some(annotation_expression) -> match (convert_type_expression
            annotation_expression Nil visible_types Nil) {
          | Err(message) -> Err(message)
          | Ok(annotation_type) -> match (constrain_types value_type annotation_type value_state) {
            | Err(message) -> Err("let annotation: " + message)
            | Ok(next_state) -> infer_let_body infer_child pattern
              annotation_type body environment visible_types next_state
          }
        }
      }
  }

let infer_let_pattern_body = fn infer_child -> fn pattern -> fn value_type -> fn body ->
  fn environment -> fn visible_types -> fn state ->
  match (infer_pattern pattern value_type visible_types state) {
    | Err(message) -> Err(message)
    | Ok(PatternBindings { bindings, state: pattern_state }) -> {
      let generalized = generalize_pattern_bindings bindings environment pattern_state;
      let body_environment = environment_with_bindings generalized environment;
      infer_child body body_environment visible_types pattern_state
    }
  }

let infer_match_arms = fn infer_child -> fn arms -> fn scrutinee_type ->
  fn environment -> fn visible_types -> fn state -> fn current_result ->
  match arms {
    | Nil -> match current_result {
      | None -> Err("match expression must have at least one arm")
      | Some(result_type) -> Ok(make_inference_result
        (apply_state_substitutions result_type state) state)
    }
    | Cons(MatchArm { pattern, body }, remaining) ->
      match (infer_pattern pattern scrutinee_type visible_types state) {
        | Err(message) -> Err("match pattern does not match the scrutinee type: " + message)
        | Ok(PatternBindings { bindings, state: pattern_state }) -> {
          let arm_environment = environment_with_bindings bindings environment;
          match (infer_child body arm_environment visible_types pattern_state) {
            | Err(message) -> Err(message)
            | Ok(InferenceResult { inferred_type: body_type, state: body_state }) ->
              match current_result {
                | None -> infer_match_arms infer_child remaining scrutinee_type
                  environment visible_types body_state (Some(body_type))
                | Some(previous_type) -> match (constrain_types previous_type body_type body_state) {
                  | Err(message) -> Err("match arms return incompatible types: " + message)
                  | Ok(unified_state) -> infer_match_arms infer_child remaining
                    scrutinee_type environment visible_types unified_state
                    (Some(previous_type))
                }
              }
          }
        }
      }
  }

let validate_boolean_match_arms = fn saw_true -> fn saw_false -> fn arms ->
  match arms {
    | Nil -> match saw_true {
      | false -> Err("boolean match is missing a `true` arm")
      | true -> match saw_false {
        | false -> Err("boolean match is missing a `false` arm")
        | true -> Ok(())
      }
    }
    | Cons(MatchArm { pattern, body: _ }, remaining) -> match (pattern_kind pattern) {
      | PatternBoolean(true) -> match saw_true {
        | true -> Err("duplicate `true` match arm")
        | false -> validate_boolean_match_arms true saw_false remaining
      }
      | PatternBoolean(false) -> match saw_false {
        | true -> Err("duplicate `false` match arm")
        | false -> validate_boolean_match_arms saw_true true remaining
      }
      | _ -> Err("boolean match accepts only `true` and `false` arms")
    }
  }

let ascribed_integer_type = fn checked_type -> match checked_type {
  | AppliedType(InferredNamedType { name, arguments }) -> match (integer_type_name name) {
    | false -> None
    | true -> match arguments {
      | Nil -> Some(name)
      | Cons(_, _) -> None
    }
  }
  | _ -> None
}

let infer_ascribed_expression = fn inner -> fn annotation_type ->
  fn environment -> fn visible_types -> fn state ->
  match (infer_expression inner environment visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InferenceResult { inferred_type, state: next_state }) ->
      match (constrain_types inferred_type annotation_type next_state) {
        | Err(message) -> Err("expression annotation: " + message)
        | Ok(final_state) -> Ok(make_inference_result
          (apply_state_substitutions annotation_type final_state) final_state)
      }
  }

let infer_i32_negation = fn operand_type -> fn state ->
  match (constrain_types operand_type (builtin_inferred_type "i32") state) {
    | Err(message) -> Err("unary `-` requires an integer operand: " + message)
    | Ok(next_state) -> Ok(make_inference_result
      (builtin_inferred_type "i32") next_state)
  }

let infer_unary_negation = fn operand_type -> fn state ->
  match (apply_state_substitutions operand_type state) {
    | AppliedType(InferredNamedType { name, arguments: Nil }) -> match
        (integer_type_name name) {
      | true -> Ok(make_inference_result
        (apply_state_substitutions operand_type state) state)
      | false -> infer_i32_negation operand_type state
    }
    | _ -> infer_i32_negation operand_type state
  }

let infer_expression_node = fn expression -> fn environment -> fn visible_types -> fn state ->
  match (expression_kind expression) {
    | Integer(IntegerLiteral(value)) -> match (integer_literal_fits value "i32") {
      | true -> Ok(make_inference_result (builtin_inferred_type "i32") state)
      | false -> Err("integer literal is outside i32 range")
    }
    | Boolean(_) -> Ok(make_inference_result (builtin_inferred_type "bool") state)
    | StringLiteral(_) -> Ok(make_inference_result (builtin_inferred_type "string") state)
    | Unit -> Ok(make_inference_result (builtin_inferred_type "unit") state)
    | VariableExpr(name) -> infer_variable_expression name environment state
    | TupleExpr(items) -> match (infer_expression_list infer_expression items
        environment visible_types state) {
      | Err(message) -> Err(message)
      | Ok(InferredTypeList(item_types, next_state)) ->
        Ok(make_inference_result (InferredTuple(item_types)) next_state)
    }
    | RecordExpr(record) -> infer_record_expression infer_expression record
      environment visible_types state
    | Binary(binary) -> infer_binary_expression infer_expression binary
      environment visible_types state
    | Negate(operand) -> match (infer_expression operand environment visible_types state) {
      | Err(message) -> Err(message)
      | Ok(InferenceResult { inferred_type, state: next_state }) ->
        infer_unary_negation inferred_type next_state
    }
    | Function(function_expression) -> infer_function_expression infer_expression
      function_expression environment visible_types state
    | Apply(application) -> infer_application_expression infer_expression
      application environment visible_types state
    | Let(let_expression) -> infer_let_expression infer_expression infer_let_pattern_body
      let_expression environment visible_types state
    | Match(MatchExpression { scrutinee, arms }) ->
      match (infer_expression scrutinee environment visible_types state) {
        | Err(message) -> Err(message)
        | Ok(InferenceResult { inferred_type: scrutinee_type, state: next_state }) ->
          match (infer_match_arms infer_expression arms scrutinee_type environment
              visible_types next_state None) {
            | Err(message) -> Err(message)
            | Ok(InferenceResult { inferred_type, state: final_state }) -> {
              let resolved_scrutinee = apply_state_substitutions scrutinee_type final_state;
              match resolved_scrutinee {
                | AppliedType(InferredNamedType { name: "bool", arguments: Nil }) ->
                  match (validate_boolean_match_arms false false arms) {
                    | Err(message) -> Err(message)
                    | Ok(_) -> Ok(make_inference_result inferred_type final_state)
                  }
                | _ -> Ok(make_inference_result inferred_type final_state)
              }
            }
          }
      }
    | ConstructorExpr(constructor_expression) ->
      infer_constructor_expression infer_expression constructor_expression
        environment visible_types state
    | Ascription(AscriptionExpression { expression: inner, annotation }) ->
      match (convert_type_expression annotation Nil visible_types Nil) {
        | Err(message) -> Err(message)
        | Ok(annotation_type) -> match (ascribed_integer_type annotation_type) {
          | Some(type_name) -> match (expression_kind inner) {
            | Integer(IntegerLiteral(value)) -> match (integer_literal_fits value type_name) {
              | false -> Err("integer literal is outside " + type_name + " range")
              | true -> {
                let annotated_state = record_expression_annotation inner annotation_type state;
                Ok(make_inference_result annotation_type annotated_state)
              }
            }
            | _ -> infer_ascribed_expression inner annotation_type environment
              visible_types state
          }
          | None -> infer_ascribed_expression inner annotation_type environment
            visible_types state
        }
      }
    | Block(expressions) -> infer_sequence infer_expression expressions
      environment visible_types state
  }

let record_expression_annotation = fn expression -> fn inferred_type -> fn (InferenceState {
  next_variable, substitutions, expression_annotations, pattern_annotations
}) -> InferenceState {
  next_variable: next_variable,
  substitutions: substitutions,
  expression_annotations: Cons(ExpressionTypeAnnotation {
    span: expression_span expression,
    inferred_type: inferred_type
  }, expression_annotations),
  pattern_annotations: pattern_annotations
}

let infer_expression = fn expression -> fn environment -> fn visible_types -> fn state ->
  match (infer_expression_node expression environment visible_types state) {
    | Err(message) -> Err(message)
    | Ok(InferenceResult { inferred_type, state: next_state }) -> {
      let resolved_type = apply_state_substitutions inferred_type next_state;
      Ok(make_inference_result resolved_type
        (record_expression_annotation expression resolved_type next_state))
    }
  }

let count_expression_list = fn expressions ->
  match expressions {
    | Nil -> 0
    | Cons(expression, remaining) ->
      count_expression_nodes expression + count_expression_list remaining
  }

let count_pattern_list = fn patterns ->
  match patterns {
    | Nil -> 0
    | Cons(pattern, remaining) -> count_pattern_nodes pattern + count_pattern_list remaining
  }

let count_ast_record_expression_fields = fn fields ->
  match fields {
    | Nil -> 0
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      count_expression_nodes value + count_ast_record_expression_fields remaining
  }

let count_record_pattern_fields = fn fields ->
  match fields {
    | Nil -> 0
    | Cons(RecordPatternField { name: _, pattern }, remaining) ->
      count_pattern_nodes pattern + count_record_pattern_fields remaining
  }

let count_match_arms = fn arms ->
  match arms {
    | Nil -> 0
    | Cons(MatchArm { pattern: _, body }, remaining) ->
      count_expression_nodes body + count_match_arms remaining
  }

let count_expression_nodes = fn expression ->
  match (expression_kind expression) {
    | Integer(_) -> 1
    | Boolean(_) -> 1
    | StringLiteral(_) -> 1
    | Unit -> 1
    | VariableExpr(_) -> 1
    | TupleExpr(items) -> 1 + count_expression_list items
    | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
      1 + count_ast_record_expression_fields fields
    | Binary(BinaryExpression { operator: _, left, right }) ->
      1 + count_expression_nodes left + count_expression_nodes right
    | Negate(operand) -> 1 + count_expression_nodes operand
    | Function(FunctionExpression { parameter: _, body }) ->
      1 + count_expression_nodes body
    | Apply(ApplicationExpression { callee, argument }) ->
      1 + count_expression_nodes callee + count_expression_nodes argument
    | Let(LetExpression { pattern: _, annotation: _, value, body }) ->
      1 + count_expression_nodes value + count_expression_nodes body
    | Match(MatchExpression { scrutinee, arms }) ->
      1 + count_expression_nodes scrutinee + count_match_arms arms
    | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
      1 + count_expression_list arguments
    | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
      1 + count_expression_nodes inner
    | Block(items) -> 1 + count_expression_list items
  }

let count_pattern_nodes = fn pattern ->
  match (pattern_kind pattern) {
    | Variable(_) -> 1
    | PatternUnit -> 1
    | Wildcard -> 1
    | PatternInteger(_) -> 1
    | PatternBoolean(_) -> 1
    | PatternString(_) -> 1
    | TuplePattern(patterns) -> 1 + count_pattern_list patterns
    | ConstructorPattern(ConstructorPatternData { constructor_name: _, arguments }) ->
      1 + count_pattern_list arguments
    | RecordPattern(RecordPatternData { constructor_name: _, fields }) ->
      1 + count_record_pattern_fields fields
    | AscribedPattern(AscribedPatternData { pattern: inner, annotation: _ }) ->
      1 + count_pattern_nodes inner
  }

let count_source_expression_nodes = fn declarations ->
  match declarations {
    | Nil -> 0
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body, span: _ }), remaining) ->
      count_expression_nodes body + count_source_expression_nodes remaining
    | Cons(TypeDeclaration(_), remaining) -> count_source_expression_nodes remaining
  }

let count_expression_annotations = fn annotations ->
  match annotations {
    | Nil -> 0
    | Cons(_, remaining) -> 1 + count_expression_annotations remaining
  }

let count_pattern_annotations = fn annotations ->
  match annotations {
    | Nil -> 0
    | Cons(_, remaining) -> 1 + count_pattern_annotations remaining
  }

let count_pattern_nodes_in_expression_list = fn expressions ->
  match expressions {
    | Nil -> 0
    | Cons(expression, remaining) ->
      count_pattern_nodes_in_expression expression
        + count_pattern_nodes_in_expression_list remaining
  }

let count_pattern_nodes_in_match_arms = fn arms ->
  match arms {
    | Nil -> 0
    | Cons(MatchArm { pattern, body }, remaining) ->
      count_pattern_nodes pattern + count_pattern_nodes_in_expression body
        + count_pattern_nodes_in_match_arms remaining
  }

let count_pattern_nodes_in_expression = fn expression ->
  match (expression_kind expression) {
    | Integer(_) -> 0
    | Boolean(_) -> 0
    | StringLiteral(_) -> 0
    | Unit -> 0
    | VariableExpr(_) -> 0
    | TupleExpr(items) -> count_pattern_nodes_in_expression_list items
    | RecordExpr(RecordExpression { constructor_name: _, fields }) ->
      count_pattern_nodes_in_record_fields fields
    | Binary(BinaryExpression { operator: _, left, right }) ->
      count_pattern_nodes_in_expression left + count_pattern_nodes_in_expression right
    | Negate(operand) -> count_pattern_nodes_in_expression operand
    | Function(FunctionExpression { parameter, body }) ->
      count_pattern_nodes parameter + count_pattern_nodes_in_expression body
    | Apply(ApplicationExpression { callee, argument }) ->
      count_pattern_nodes_in_expression callee + count_pattern_nodes_in_expression argument
    | Let(LetExpression { pattern, annotation: _, value, body }) ->
      count_pattern_nodes pattern + count_pattern_nodes_in_expression value
        + count_pattern_nodes_in_expression body
    | Match(MatchExpression { scrutinee, arms }) ->
      count_pattern_nodes_in_expression scrutinee + count_pattern_nodes_in_match_arms arms
    | ConstructorExpr(ConstructorExpression { constructor_name: _, arguments }) ->
      count_pattern_nodes_in_expression_list arguments
    | Ascription(AscriptionExpression { expression: inner, annotation: _ }) ->
      count_pattern_nodes_in_expression inner
    | Block(items) -> count_pattern_nodes_in_expression_list items
  }

let count_pattern_nodes_in_record_fields = fn fields ->
  match fields {
    | Nil -> 0
    | Cons(RecordExpressionField { name: _, value }, remaining) ->
      count_pattern_nodes_in_expression value + count_pattern_nodes_in_record_fields remaining
  }

let count_source_pattern_nodes = fn declarations ->
  match declarations {
    | Nil -> 0
    | Cons(ValueDeclaration(ValueDeclarationData { name: _, body, span: _ }), remaining) ->
      count_pattern_nodes_in_expression body + count_source_pattern_nodes remaining
    | Cons(TypeDeclaration(_), remaining) -> count_source_pattern_nodes remaining
  }

let annotation_count_digit = fn value ->
  match value {
    | 0 -> "0" | 1 -> "1" | 2 -> "2" | 3 -> "3" | 4 -> "4"
    | 5 -> "5" | 6 -> "6" | 7 -> "7" | 8 -> "8" | _ -> "9"
  }

let annotation_count_text = fn value ->
  match (value < 10) {
    | true -> annotation_count_digit value
    | false -> annotation_count_text (value / 10) + annotation_count_digit (value % 10)
  }

let typed_ast_annotation_error = fn declarations -> fn expression_annotations ->
  fn pattern_annotations -> {
  let expected_expressions = count_source_expression_nodes declarations;
  let actual_expressions = count_expression_annotations expression_annotations;
  let expected_patterns = count_source_pattern_nodes declarations;
  let actual_patterns = count_pattern_annotations pattern_annotations;
  match (expected_expressions == actual_expressions) {
    | false -> "expression annotation count mismatch: expected "
      + annotation_count_text expected_expressions + ", got "
      + annotation_count_text actual_expressions
    | true -> match (expected_patterns == actual_patterns) {
      | false -> "pattern annotation count mismatch: expected "
        + annotation_count_text expected_patterns + ", got "
        + annotation_count_text actual_patterns
      | true -> ""
    }
  }
}

// Keep all recursive module bindings monomorphic while checking their bodies.
let validate_source_type_declarations = fn declarations -> fn visible_types ->
  match declarations {
    | Nil -> Ok(())
    | Cons(TypeDeclarationData { name: _, parameters, definition }, remaining) -> {
      let TypedParameters(TypeParameterState { bindings, generic_variables: _, next_index: _ }) =
        type_parameter_bindings parameters 0;
      let definition_result = match definition {
        | TypeAlias(alias_body) -> match (convert_type_expression alias_body
            bindings visible_types Nil) {
          | Err(message) -> Err(message)
          | Ok(_) -> Ok(())
        }
        | TypeRecord(fields) -> match (convert_record_type_fields fields
            bindings visible_types Nil) {
          | Err(message) -> Err(message)
          | Ok(_) -> Ok(())
        }
        | TypeVariants(variants) -> match (convert_variant_type_cases variants
            bindings visible_types Nil) {
          | Err(message) -> Err(message)
          | Ok(_) -> Ok(())
        }
      };
      match definition_result {
        | Err(message) -> Err(message)
        | Ok(_) -> validate_source_type_declarations remaining visible_types
      }
    }
  }

let prepare_source_values = fn declarations -> fn state ->
  match declarations {
    | Nil -> Ok(PreparedValues { bindings: Nil, state: state })
    | Cons(ValueDeclaration(ValueDeclarationData { name, body: _, span: _ }), remaining) ->
      match (fresh_type_variable state) {
        | FreshInference(inferred_type, next_state) -> match (prepare_source_values
            remaining next_state) {
          | Err(message) -> Err(message)
          | Ok(PreparedValues { bindings, state: final_state }) -> Ok(PreparedValues {
            bindings: Cons(SchemeBinding(name, TypeScheme {
              quantified_variables: Nil,
              body: inferred_type
            }), bindings),
            state: final_state
          })
        }
      }
    | Cons(TypeDeclaration(_), remaining) -> prepare_source_values remaining state
  }

let infer_source_value_bodies = fn declarations -> fn local_bindings ->
  fn imported_values -> fn visible_types -> fn state -> {
  let environment = environment_with_bindings local_bindings imported_values;
  match declarations {
    | Nil -> Ok(state)
    | Cons(ValueDeclaration(ValueDeclarationData {
        name: ValueName(name), body, span: _
      }), remaining) -> match (lookup_value_scheme name local_bindings) {
      | None -> Err("missing recursive type variable for value `" + name + "`")
      | Some(TypeScheme { quantified_variables: _, body: declared_type }) ->
        match (infer_expression body environment visible_types state) {
          | Err(message) -> Err("value `" + name + "`: " + message)
          | Ok(InferenceResult { inferred_type, state: next_state }) ->
            match (constrain_types declared_type inferred_type next_state) {
              | Err(message) -> Err("value `" + name + "`: " + message)
              | Ok(unified_state) -> infer_source_value_bodies remaining
                local_bindings imported_values visible_types unified_state
            }
        }
    }
    | Cons(TypeDeclaration(_), remaining) -> infer_source_value_bodies
      remaining local_bindings imported_values visible_types state
  }
  }

let generalize_source_values = fn local_bindings -> fn imported_values -> fn state ->
  match local_bindings {
    | Nil -> Nil
    | Cons(SchemeBinding(name, TypeScheme {
        quantified_variables: _, body
      }), remaining) -> {
      let generalized = generalize_type imported_values body
        (module_state_substitutions state);
      Cons(SchemeBinding(name, generalized),
        generalize_source_values remaining imported_values state)
    }
  }

let hide_type_visibility_names = fn visible_types ->
  match visible_types {
    | Nil -> Nil
    | Cons(VisibleType(VisibleTypeData {
        declaration, type_identity, available_names, visible_names: _
      }), remaining) -> Cons(VisibleType(VisibleTypeData {
        declaration: declaration,
        type_identity: type_identity,
        visible_names: Nil,
        available_names: available_names
      }), hide_type_visibility_names remaining)
  }

let infer_source_module = fn (LoadedModule {
  module_path, source_path, source_module: SourceModule { imports, declarations }
}) -> fn checked_modules -> {
      let loaded_module = LoadedModule {
        module_path: module_path,
        source_path: source_path,
        source_module: SourceModule { imports: imports, declarations: declarations }
      };
      let source_module = SourceModule { imports: imports, declarations: declarations };
      let name_error = check_source_module_names source_module;
      match (equal name_error "") {
        | false -> Err(name_error)
        | true -> {
          let imported_values = imported_values_for_imports imports checked_modules;
          let imported_types = imported_types_for_imports imports checked_modules;
          let local_types = local_type_visibilities source_path declarations;
          let visible_types = append_type_visibilities imported_types local_types;
          let interface_type_metadata = append_type_visibilities
            (hide_type_visibility_names imported_types) local_types;
          match (validate_source_type_declarations
              (declaration_type_declarations declarations) visible_types) {
            | Err(message) -> Err(message)
            | Ok(_) -> match (prepare_source_values declarations
                (initial_inference_state ())) {
              | Err(message) -> Err(message)
              | Ok(PreparedValues { bindings: local_bindings, state }) ->
                match (infer_source_value_bodies declarations local_bindings
                    imported_values visible_types state) {
                  | Err(message) -> Err(message)
                  | Ok(final_state) -> {
                    let values = generalize_source_values local_bindings
                      imported_values final_state;
                    let InferenceState {
                      next_variable: _,
                      substitutions,
                      expression_annotations,
                      pattern_annotations
                    } = final_state;
                    let annotation_error = typed_ast_annotation_error declarations
                      expression_annotations pattern_annotations;
                    match (equal annotation_error "") {
                      | false -> Err("internal error: TypedAST " + annotation_error)
                      | true -> match (resolve_module_bindings source_path imports
                          declarations checked_modules visible_types) {
                        | Err(message) -> Err("internal error: " + message)
                        | Ok(BindingResolution {
                            bindings, value_references, constructor_references
                          }) -> {
                          let typed_module = TypedModule {
                            source_module: source_module,
                            expression_types: expression_annotations,
                            pattern_types: pattern_annotations,
                            nominal_types: visible_types,
                            substitutions: substitutions,
                            bindings: bindings,
                            value_references: value_references,
                            constructor_references: constructor_references
                          };
                          Ok(CheckedModuleInterface {
                            loaded_module: loaded_module,
                            interface: InferredModuleInterface {
                              values: values,
                              types: interface_type_metadata
                            },
                            typed_module: typed_module
                          })
                        }
                      }
                    }
                  }
                }
            }
          }
        }
      }
    }

let has_native_entry = fn declarations ->
  match declarations {
    | Nil -> false
    | Cons(ValueDeclaration(ValueDeclarationData {
        name: ValueName("main"),
        body,
        span: _
      }), _) -> match (expression_kind body) {
      | Function(FunctionExpression { parameter, body: _ }) -> match (pattern_kind parameter) {
        | PatternUnit -> true
        | _ -> false
      }
      | _ -> false
    }
    | Cons(ValueDeclaration(_), remaining) -> has_native_entry remaining
    | Cons(TypeDeclaration(_), remaining) -> has_native_entry remaining
  }

let validate_native_entry_module = fn (SourceModule {
  imports: _, declarations
}) -> fn interface ->
  match (has_native_entry declarations) {
    | false -> Err("native entry module must define `main : unit -> i32`")
    | true -> match (lookup_value_scheme "main" (interface_values interface)) {
      | None -> Err("native entry module must define `main : unit -> i32`")
      | Some(scheme) -> match (instantiate_scheme scheme (initial_inference_state ())) {
        | Err(message) -> Err(message)
        | Ok(FreshInference(main_type, state)) -> {
          let required_type = FunctionType(InferredFunctionType {
            argument: builtin_inferred_type "unit",
            result: builtin_inferred_type "i32"
          });
          match (constrain_types main_type required_type state) {
            | Err(_) -> Err("native entry point `main` must have type `unit -> i32`")
            | Ok(_) -> Ok(())
          }
        }
      }
    }
  }
